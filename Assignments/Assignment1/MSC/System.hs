{-

This is a simulation of Mobile Switching Centers and associated
location updates to HLR and VLR for gsm architecture. It is a very
simplistic implementation.

-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MSC.System where

import qualified Data.Map              as M
import           Network

-- | The mobile number of a person divided into country code, area
-- code and the actual number.
data MobileNumber = Number { countryCode :: String
                           , areaCode    :: String
                           , phoneNumber :: String
                           } deriving (Read,Show,Ord,Eq)

instance Initiate MobileNumber where
    initiate = Number "+91" "" "9999999999"

-- | The sim consists of the mobile number with which this sim is
-- associated with and the sim number.
data Sim = Sim {
                 imsi         :: String
               , msisdn       :: MobileNumber
               , pimsi        :: String
               , ki           :: String
               , a3           :: String
               , a8           :: String
               , kc           :: String
               , timsi        :: String
               , lai          :: String
               , rai          :: String
               , msrsn        :: String
               , mscLoc       :: MSCLoc
               , hlrLoc       :: MSCLoc
               } deriving (Read,Show)

instance Initiate Sim where
    initiate = Sim initiate initiate initiate initiate initiate initiate initiate initiate initiate initiate initiate 1234 1234

-- | Mobile handset consists of the imei associated with it and
-- contain a sim card or no sim card.
data Mobile = Mobile { imei      :: String
                     , sim       :: Sim
                     } deriving (Read,Show)

instance Initiate Mobile where
    initiate = Mobile initiate initiate

-- | HLR just acts as a database to store information about mobile
-- stations in its domain.
type HLR = M.Map MobileNumber HLRRecord

data HLRRecord = HLRRecord { hlrimei     :: String
                           , hlrmsisdn   :: MobileNumber
                           , vlr         :: Maybe MSCLoc
                           , hlrcellLocation :: Int
                           } deriving (Read,Show)

-- | VLR
type VLR = M.Map MobileNumber VLRRecord

data VLRRecord = VLRRecord { vlrimei         :: String
                           , vlrmsisdn       :: MobileNumber
                           , vlrhlrLoc       :: MSCLoc
                           , vlrcellLocation :: Int
                           } deriving (Read,Show)

-- | MSC contains the `HLR` and `VLR` entries.
data MSC = MSC { mschlr :: HLR
               , mscvlr :: VLR
               } deriving (Read,Show)

instance Initiate MSC where
    initiate = MSC (M.empty) (M.empty)
type MSCLoc = Int

-- | To initiateiate a default value
class Initiate a where
    initiate :: a

instance Initiate String where
    initiate = "Initiateial String"
