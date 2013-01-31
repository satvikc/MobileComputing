import           Control.Applicative     ((<$>),(<*>))
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception       (finally)
import           Control.Monad
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C8
import qualified Data.Map                as M
import           Network
import           System.IO
import           System.Environment
import           Text.ParserCombinators.Parsec

import           MSC.System
import           MSC.Communication


data Server = Server (MVar MSC) MSCLoc

serverInit :: MSCLoc -> IO Server
serverInit p = Server <$> newMVar (MSC M.empty M.empty) <*> return p

-- | Starts the server
runServer :: PortNumber -> IO ()
runServer port = do
    server <- serverInit (fromIntegral port)
    sock <- listenOn $ PortNumber port
    putStrLn $ "MSC Listening on port " ++ (show port)
    let serv =  do
        (handle, host , p) <- accept sock
        hSetNewlineMode handle universalNewlineMode
        hSetBuffering handle NoBuffering
        putStrLn $ "Accepted connection from " ++ host ++ ":" ++ show p
--        addToHLR server (read mobile :: Mobile)
        forkIO $ (serve server handle `finally` hClose handle)
        serv
    serv `finally` sClose sock

serve :: Server ->  Handle -> IO ()
serve s@(Server server _) handle = do
    bool <- hIsEOF handle
    if not bool then do
      name <- hGetLine handle
      hFlush handle
      case parseServerCommand name of
        Nothing -> return ()
        Just sc -> serveCommand s handle sc
      state <- readMVar server
      print state
      serve s handle
     else return ()

serveCommand :: Server -> Handle -> ServerCommand -> IO ()
serveCommand s handle (ChangeCell i m)  = changeCell s i m
serveCommand s handle (AddToHLR m)      = addToHLR s m
serveCommand s handle (AddToVLR m)      = addToVLR s m
serveCommand s handle (RemoveFromVLR m) = removeFromVLR s m
serveCommand s handle (ChangeMSC i m)   = changeMSC s m

changeCell :: Server -> Int -> Mobile -> IO ()
changeCell (Server server p) i (Mobile _ s) = modifyMVar_ server updateServer
                     where
                       net = hlrLoc s
                       updateServer (MSC hlr vlr) =
                          return $ if net == p then MSC (changeCellhlr hlr i) vlr
                                               else MSC hlr (changeCellvlr vlr i)
                       changeCellhlr hlr i =
                         M.adjust (\a -> a {hlrcellLocation = i}) (msisdn s) hlr
                       changeCellvlr vlr i =
                         M.adjust (\a -> a {vlrcellLocation = i}) (msisdn s) vlr

addToHLR :: Server -> Mobile -> IO ()
addToHLR (Server server p) mobile@(Mobile _ sm) =
  modifyMVar_ server (\msc -> return $ msc {mschlr = newhlr (mschlr msc)})
 where
  newhlr oldhlr = M.insert (msisdn $ sim mobile) (hlrrecord mobile) oldhlr
  hlrrecord (Mobile i s) = if mloc == p then HLRRecord i (msisdn s) Nothing 0
                              else HLRRecord i (msisdn s) (Just mloc) 0
  hloc = hlrLoc sm
  mloc = mscLoc sm

addToVLR :: Server -> Mobile -> IO ()
addToVLR (Server server _) mobile =
  modifyMVar_ server (\msc -> return $ msc {mscvlr = newvlr (mscvlr msc)})
 where
  newvlr oldvlr = M.insert (msisdn $ sim mobile) (vlrrecord mobile) oldvlr
  vlrrecord (Mobile i s) = VLRRecord i (msisdn s) (hlrLoc $ sim mobile) 0

removeFromVLR :: Server -> Mobile -> IO ()
removeFromVLR (Server server _) mobile =
  modifyMVar_ server (\msc -> return $ msc {mscvlr = newvlr (mscvlr msc)})
 where
  newvlr oldvlr = M.delete (msisdn $ sim mobile) oldvlr

changeMSC :: Server -> Mobile -> IO ()
changeMSC s@(Server server p) m@(Mobile i sm) = do
  when (net /= p) $ addToVLR s m
  when (net /= prev) $ sendMessage prev ("removeFromVLR " ++ show m)
  sendMessage net  ("addToHLR " ++ show (Mobile i (sm {mscLoc = p})))
 where
  net  = hlrLoc sm
  prev = mscLoc sm

---------------------------------------------------------------------------------
-------------------------------- SERVER COMMANDS --------------------------------
---------------------------------------------------------------------------------

data ServerCommand = ChangeCell Int Mobile
                   | AddToHLR Mobile
                   | UpdateHLR Mobile
                   | AddToVLR Mobile
                   | RemoveFromVLR Mobile
                   | ChangeMSC MSCLoc Mobile
                     deriving Show

pChangeCell :: GenParser Char () ServerCommand
pChangeCell = do
  string "changecell"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  space
  mobile <- many anyChar
  return $ ChangeCell (read number) (read mobile)

pChangeMSC :: GenParser Char () ServerCommand
pChangeMSC = do
  string "changemsc"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  space
  mobile <- many anyChar
  return $ ChangeMSC (read number) (read mobile)

pAddToHLR :: GenParser Char () ServerCommand
pAddToHLR = do
  string "addToHLR"
  spaces
  mobile <- many anyChar
  return $ AddToHLR (read mobile)

pAddToVLR :: GenParser Char () ServerCommand
pAddToVLR = do
  string "addToVLR"
  spaces
  mobile <- many anyChar
  return $ AddToVLR (read mobile)

pRemoveFromVLR :: GenParser Char () ServerCommand
pRemoveFromVLR = do
  string "removeFromVLR"
  spaces
  mobile <- many anyChar
  return $ RemoveFromVLR (read mobile)


pServerCommand :: GenParser Char () ServerCommand
pServerCommand = do
  spaces
  e <- try pChangeCell <|> try pChangeMSC <|> try pAddToHLR <|> try pAddToVLR <|> try pRemoveFromVLR
  optional eof
  return e

parseServerCommand :: String -> Maybe ServerCommand
parseServerCommand s = case parse pServerCommand "" s of
  Right sc -> Just sc
  _        -> Nothing



main = do 
  (port:_) <- getArgs
  runServer (fromIntegral (read port :: Int))
