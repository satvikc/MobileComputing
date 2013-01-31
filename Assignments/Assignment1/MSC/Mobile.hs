
import           Control.Concurrent
import           Control.Monad
import           Network
import           System.IO
import           Text.ParserCombinators.Parsec

import           MSC.System
import           MSC.Communication

---------------------------------------------------------------------------------
-------------------------------- MOBILE COMMANDS --------------------------------
---------------------------------------------------------------------------------

data MobileCommand = ChangeCell Int
                   | AddToHLR MSCLoc
                   | ChangeMSC MSCLoc
                     deriving Show
                   
pChangeCell :: GenParser Char () MobileCommand
pChangeCell = do
  string "changecell"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  return $ ChangeCell (read number)

pChangeMSC :: GenParser Char () MobileCommand
pChangeMSC = do
  string "changemsc"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  return $ ChangeMSC (read number)

pAddToHLR :: GenParser Char () MobileCommand
pAddToHLR = do
  string "addtohlr"
  spaces
  number <- many1 $ oneOf ['0' .. '9']
  return $ AddToHLR (read number)

pServerCommand :: GenParser Char () MobileCommand
pServerCommand = do
  spaces
  e <- try pChangeCell <|> try pChangeMSC <|> try pAddToHLR
  optional eof
  return e

parseServerCommand :: String -> Maybe MobileCommand
parseServerCommand s = case parse pServerCommand "" s of
  Right sc -> Just sc
  _        -> Nothing


loop :: Mobile -> IO ()
loop mobile = do
  w <- getLine
  case parseServerCommand w of
    Nothing -> loop mobile
    Just sc -> do 
               m <- serveCommand sc mobile
               loop m

serveCommand :: MobileCommand -> Mobile -> IO Mobile
serveCommand (ChangeCell i) m@(Mobile _ s) = do
  sendMessage (mscLoc s) ("changecell "  ++ show i ++ " " ++ show m)
  return m
serveCommand (AddToHLR p) m@(Mobile i s) = do 
  let mobile = Mobile i (s {hlrLoc = p,mscLoc = p})
  sendMessage p ("addToHLR " ++ show mobile)
  return mobile
serveCommand (ChangeMSC p) m@(Mobile i s) = do 
  sendMessage p ("changemsc " ++ show p ++ " " ++ show m)
  return (Mobile i (s {mscLoc = p}))

  


-- client :: PortNumber -> IO ()
-- client port = do
--   hndl <- connectTo "localhost" (PortNumber port)
--   hSetBuffering hndl NoBuffering
--   let mob = (initiate :: Mobile)
--   done <- newEmptyMVar
--   let spawnWorker io = forkIO (io `finally` tryPutMVar done ())
--   recv_tid <- spawnWorker $ forever $ do
--     e <- hIsEOF hndl
--     unless e $ do msg <- hGetLine hndl
--                   putStrLn msg
--   send_tid <- spawnWorker $ forever $ do
--     w <- getLine
--     when (w=="quit") (tryPutMVar done () >> return ())
--     hPutStrLn hndl (w ++ " " ++ show mob)
--     hFlush hndl
--   takeMVar done
--   mapM_ killThread [recv_tid, send_tid] `finally` hClose hndl

main = loop initiate
