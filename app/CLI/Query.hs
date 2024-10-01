{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module CLI.Query
  ( runQueryPersonalAddress
  , runSubmitTx
  , runEvaluateTx
  , runGetParams
  , runQuerySpots
  , runQueryAuctions
  , runQueryBids
  , runQuerySpecificMarketUTxO
  ) where

import Relude
import Servant.Client 
import Network.HTTP.Client hiding (responseBody)
import Network.HTTP.Client.TLS
import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson

import CLI.Data.ApiService
import CLI.Data.Bech32Address
import CLI.Data.Network
import CLI.Data.MarketUTxO
import CLI.Data.PersonalUTxO
import CLI.Data.TxCBOR
import CLI.Query.Koios as Koios

import CardanoAftermarket

runQueryPersonalAddress :: Network -> ApiService -> PaymentAddress -> IO [PersonalUTxO]
runQueryPersonalAddress network api addr = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryPersonalAddress addr) env

runSubmitTx :: Network -> ApiService -> FilePath -> IO Value
runSubmitTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.submitTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runEvaluateTx :: Network -> ApiService -> FilePath -> IO Value
runEvaluateTx network api txFile = do
  tx' <- decode @TxCBOR <$> LBS.readFile txFile
  case tx' of
    Nothing -> return "Failed to deserialise transaction file"
    Just tx -> do
      manager' <- newManager tlsManagerSettings
      res <- case (network,api) of
        (PreProdTestnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
        (Mainnet,Koios) -> do
          let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1/ogmios")
          runClientM (Koios.evaluateTx tx) env
      case res of
        Right r -> return r
        Left e@(FailureResponse _ err) -> case decode $ responseBody err of
          Just response -> return response
          Nothing -> throw e
        Left err -> throw err

runGetParams :: Network -> ApiService -> IO Value
runGetParams network api = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM Koios.getParams env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM Koios.getParams env

runQuerySpots 
  :: Network 
  -> ApiService
  -> CurrencySymbol
  -> Maybe PaymentAddress 
  -> IO [MarketUTxO]
runQuerySpots network api nftPolicy mSellerAddr = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpots nftPolicy mSellerAddr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpots nftPolicy mSellerAddr) env

runQueryAuctions
  :: Network 
  -> ApiService
  -> CurrencySymbol
  -> Maybe PaymentAddress 
  -> IO [MarketUTxO]
runQueryAuctions network api nftPolicy mSellerAddr = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAuctions nftPolicy mSellerAddr) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryAuctions nftPolicy mSellerAddr) env

runQueryBids
  :: Network 
  -> ApiService
  -> CurrencySymbol
  -> Maybe PaymentAddress 
  -> Maybe Credential 
  -> IO [MarketUTxO]
runQueryBids network api nftPolicy mSellerAddr mBidderCred = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.queryBids nftPolicy mSellerAddr mBidderCred) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.queryBids nftPolicy mSellerAddr mBidderCred) env

runQuerySpecificMarketUTxO :: Network -> ApiService -> TxOutRef -> IO [MarketUTxO]
runQuerySpecificMarketUTxO network api outRef = do
  manager' <- newManager tlsManagerSettings
  either throw return =<< case (network,api) of
    (PreProdTestnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "preprod.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificMarketUTxO outRef) env
    (Mainnet,Koios) -> do
      let env = mkClientEnv manager' (BaseUrl Https "api.koios.rest" 443 "api/v1")
      runClientM (Koios.querySpecificMarketUTxO outRef) env

