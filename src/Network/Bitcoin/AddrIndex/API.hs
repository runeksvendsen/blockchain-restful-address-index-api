{-# LANGUAGE DataKinds, LambdaCase, TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Network.Bitcoin.AddrIndex.API
( -- *Server API
  BlockchainApi
  -- *Endpoints (individual)
, AllOuts
, UnspentOuts
, TxOutProof
, PublishTx
, EstimateFee
, RawCmd
  -- *Utility
, verifyFundingProof
  -- *Re-exports
, module Network.Bitcoin.AddrIndex.Types
)
where

import           Network.Bitcoin.AddrIndex.Types
import           Lib.TxOutProof.Types           (verifyFundingProof)
--import           Data.Word                      (Word64)
import           Servant.API
import qualified Network.Haskoin.Transaction    as HT
import qualified Data.Aeson                     as JSON
import qualified Data.Text                      as T


-- |The API exposed by this server.
type BlockchainApi =
       AllOuts
  :<|> UnspentOuts
  :<|> TxOutProof
  :<|> PublishTx
  :<|> EstimateFee
  :<|> RawCmd

-- | Get all outputs paying to address
type AllOuts =
    "outputs"     :> Capture "address" Addr :> "all"        :> Get  '[JSON] [AddressFundingInfo]

-- | Get unspent outputs paying to address
type UnspentOuts =
    "outputs"     :> Capture "address" Addr :> "unspent"    :> Get  '[JSON] [AddressFundingInfo]

-- | Get proof that transaction is in block
type TxOutProof =
    "txOutProof"  :> Capture "txid" HT.TxHash               :> Get  '[JSON] FundingProof

-- | Publish a transaction
type PublishTx =
    "publishTx"   :> ReqBody '[JSON] PushTxReq              :> Post '[JSON] PushTxResp

-- | Estimate fee (in satoshis per byte) needed to get transaction confirmed within "maxBlocks"
type EstimateFee =
    "estimateFee" :> Capture "maxBlocks" Word               :> Get '[JSON] Word

type RawCmd =
    "rawCmd"      :> Capture "method" String
                  :> CaptureAll "args" T.Text               :> Get  '[JSON] JSON.Value

