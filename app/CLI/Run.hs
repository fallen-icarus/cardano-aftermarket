module CLI.Run
  (
    runCommand
  ) where

import Relude
import Data.Aeson
import Data.Aeson.Encoding qualified as Aeson
import qualified Data.ByteString.Lazy as LBS
import Prettyprinter
import Prettyprinter.Render.Terminal
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

import CardanoAftermarket

import CLI.Data.Bech32Address
import CLI.Data.Commands
import CLI.Data.Network
import CLI.Data.MarketUTxO
import CLI.Data.Output
import CLI.Data.PersonalUTxO
import CLI.Query

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
  ExportScript script file -> runExportScript script file
  CreateDatum protocolDatum file -> runCreateDatum protocolDatum file
  CreateRedeemer newRedeemer file -> runCreateRedeemer newRedeemer file
  BeaconName info output -> runBeaconName info output
  Query query -> runQuery query
  SubmitTx network api txFile ->
    runSubmitTx network api txFile >>= LBS.putStr . encode
  EvaluateTx network api txFile ->
    runEvaluateTx network api txFile >>= LBS.putStr . encode

runExportScript :: Script -> FilePath -> IO ()
runExportScript script file = do
  flip whenLeftM_ (\e -> putStrLn $ "There was an error: " <> show e) $
    writeScript file $ case script of
      BeaconScript -> beaconScript
      PaymentObserverScript -> paymentObserverScript
      MarketScript -> aftermarketScript
      ProxyScript -> proxyScript

runCreateDatum :: NewDatum -> FilePath -> IO ()
runCreateDatum (NewSpotDatum newSpotDatum) file = 
  writeData file $ unsafeCreateSpotDatum newSpotDatum
runCreateDatum (NewAuctionDatum newAuctionDatum) file = 
  writeData file $ unsafeCreateAuctionDatum newAuctionDatum
runCreateDatum (NewBidDatum newBidDatum) file = 
  writeData file $ unsafeCreateBidDatum newBidDatum
runCreateDatum (NewPaymentDatum marketRef) file = 
  writeData file $ PaymentDatum (BeaconId beaconCurrencySymbol, marketRef)

runCreateRedeemer :: NewRedeemer -> FilePath -> IO ()
runCreateRedeemer (NewBeaconRedeemer redeemer) file = writeData file redeemer
runCreateRedeemer (NewMarketRedeemer redeemer) file = writeData file redeemer
runCreateRedeemer (NewPaymentObserverRedeemer redeemer) file = writeData file redeemer

runBeaconName :: BeaconName -> Output -> IO ()
runBeaconName name output = 
    displayName $ case name of
      BeaconPolicyId -> show beaconCurrencySymbol
      SpotBeaconName -> showTokenName "Spot"
      AuctionBeaconName -> showTokenName "Auction"
      BidBeaconName -> showTokenName "Bid"
      BidderIdName bidderCred -> showTokenName $ unBidderId $ toBidderId bidderCred
      PolicyBeaconName policyId -> showTokenName $ unPolicyBeacon $ toPolicyBeacon policyId
  where
    displayName :: String -> IO ()
    displayName = case output of
      Stdout -> putStr
      File file -> writeFile file

runQuery :: Query -> IO ()
runQuery query = case query of
  QueryParameters network api output -> runGetParams network api >>= \params -> do
    let toByte = Aeson.encodingToLazyByteString . Aeson.value
    case output of
      Stdout -> LBS.putStr $ toByte params
      File file -> LBS.writeFile file $ toByte params
  QueryPersonal network api addr format output ->
    runQueryPersonalAddress network api addr >>= 
      case format of
        JSON -> toJSONOutput output
        Pretty -> toPrettyOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO 
        Plain -> toPlainOutput output 
                . (<> hardline) 
                . (personalHeader <>) 
                . vsep 
                . map prettyPersonalUTxO
  QuerySpots network api nftPolicy mSellerCred format output -> do
    let mSellerAddr = genMarketAddress network <$> mSellerCred
    runQuerySpots network api nftPolicy mSellerAddr >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)
  QueryAuctions network api nftPolicy mSellerCred format output -> do
    let mSellerAddr = genMarketAddress network <$> mSellerCred
    runQueryAuctions network api nftPolicy mSellerAddr >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)
  QueryBids network api nftPolicy mSellerCred mBidderCred format output -> do
    let mSellerAddr = genMarketAddress network <$> mSellerCred
    runQueryBids network api nftPolicy mSellerAddr mBidderCred >>= case format of
      JSON -> toJSONOutput output
      Pretty -> toPrettyOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)
      Plain -> toPlainOutput output . (<> hardline) . vsep . map (prettyMarketUTxO network)

-------------------------------------------------
-- Helper Functions
-------------------------------------------------
toPlainOutput :: Output -> Doc AnsiStyle -> IO ()
toPlainOutput Stdout xs = TIO.putStr $ T.pack $ show $ unAnnotate xs
toPlainOutput (File file) xs = TIO.writeFile file $ T.pack $ show xs

toPrettyOutput :: Output -> Doc AnsiStyle -> IO ()
toPrettyOutput Stdout xs = putDoc xs
toPrettyOutput (File file) xs = 
  TIO.writeFile file $ renderStrict $ layoutPretty defaultLayoutOptions xs

toJSONOutput :: (ToJSON a) => Output -> [a] -> IO ()
toJSONOutput Stdout xs = LBS.putStr $ encode xs
toJSONOutput (File file) xs = LBS.writeFile file $ encode xs

genMarketAddress :: Network -> Credential -> PaymentAddress
genMarketAddress network sellerCred = 
  either error fst $ plutusToBech32 network $
    Address (ScriptCredential aftermarketScriptHash) (Just $ StakingHash sellerCred)

