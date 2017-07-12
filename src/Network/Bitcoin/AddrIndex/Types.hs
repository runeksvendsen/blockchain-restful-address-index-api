{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, OverloadedStrings #-}

module Network.Bitcoin.AddrIndex.Types
(
    AddressFundingInfo(..)
  , Addr(..)
  , PushTxReq(..)
  , PushTxResp(..)
  , FundingProof(..)
  , HB.MerkleBlock
  , AddrIndexServerUrl(..), getServerUrl
  , module Orphans
)
where

import           Lib.FundingInfo.Types      (AddressFundingInfo(..))
import           Lib.TxOutProof.Types
import           Orphans
import qualified Network.Haskoin.Transaction            as HT
import qualified Network.Haskoin.Block                  as HB
import qualified Network.Haskoin.Crypto                 as HC
import qualified Network.Haskoin.Constants              as HCC

import Servant.Common.BaseUrl
import qualified Web.HttpApiData                        as Web
import           Data.Aeson

import           GHC.Generics

newtype Addr = Addr { getAddress :: HC.Address }
    deriving (Generic, Web.FromHttpApiData, Web.ToHttpApiData)

data PushTxReq = PushTxReq { tx_data :: HT.Tx } deriving Generic
data PushTxResp = PushTxResp { tx_id :: HT.TxHash } deriving Generic

-- | Describe the location of an AddrIndex server for both Bitcoin and testnet3
data AddrIndexServerUrl = AddrIndexServerUrl
    { aisuLivenetServer :: BaseUrl
    , aisuTestnetServer :: BaseUrl
    }

getServerUrl :: AddrIndexServerUrl -> BaseUrl
getServerUrl (AddrIndexServerUrl livenet testnet) =
    if HCC.getNetwork == HCC.prodnet
        then livenet
        else testnet

instance FromJSON PushTxReq
instance ToJSON PushTxReq
instance FromJSON PushTxResp
instance ToJSON PushTxResp
