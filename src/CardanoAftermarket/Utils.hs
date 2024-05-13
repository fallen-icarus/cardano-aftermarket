{-# LANGUAGE OverloadedStrings #-}

module CardanoAftermarket.Utils
  (
    -- * Serialization
    writeData
  , writeScript
  , decodeDatum
  , dataFromCBOR
  , decodeHex
  , toCBOR
  , parseScriptFromCBOR

    -- * Parsing User Inputs
    -- This is just so that certain things do not need to be re-exported.
  , readAsset
  , readAssetAmount
  , readTokenName
  , readCurrencySymbol
  , readTxId
  , readTxOutRef
  , readPubKeyHash
  , readScriptHash

    -- * Misc
  , unsafeFromRight
  , showTokenName
  , unsafeToBuiltinByteString
  , scriptHash
  , datumHash
  , toLedgerScript
  , toVersionedLedgerScript
  , wrapVersionedLedgerScript
  , toCardanoApiScript
  , UPLC.serialisedSize

  -- * Re-exports
  , applyArguments
  , PV2.CurrencySymbol(..)
  , PV2.TokenName(..)
  , PV2.Address(..)
  , PV2.Credential(..)
  , PV2.PubKeyHash(..)
  , PV2.POSIXTime(..)
  , PV2.StakingCredential(..)
  , PV2.adaSymbol
  , PV2.adaToken
  , numerator
  , denominator
  , PV2.TxOutRef(..)
  , PV2.TxId(..)
  , PV2.SerialisedScript
  , PV2.ScriptHash(..)
  , BuiltinByteString(..)
  , L.Slot(..)
  , unBuiltinByteString
  ) where

import qualified Data.Aeson as Aeson
import Lens.Micro (over)
import qualified Codec.Serialise as Serial
import Data.ByteString.Lazy (fromStrict,toStrict)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import Data.String (fromString)
import Text.Read (readMaybe)
import qualified Data.ByteString.Base16 as Base16
import Relude (Text,toShort,encodeUtf8,maybeToRight)
import Data.Ratio (numerator,denominator)

import qualified PlutusCore.MkPlc as PLC
import qualified UntypedPlutusCore as UPLC
import qualified Cardano.Api as Api 
import Cardano.Api.Shelley (fromPlutusData,PlutusScript(..))
import PlutusLedgerApi.V1.Bytes (fromHex,bytes,encodeByteString,LedgerBytesError)
import Ledger.Tx.CardanoAPI.Internal (fromCardanoScriptData)
import qualified Ledger as L
import qualified PlutusTx.Builtins as Builtins
import PlutusTx.Builtins.Internal (BuiltinByteString(..))
import qualified Plutus.Script.Utils.Scripts as PV2
import qualified PlutusLedgerApi.V2 as PV2

import CardanoAftermarket.Types

-------------------------------------------------
-- Serialization
-------------------------------------------------
toJSONValue :: PV2.ToData a => a -> Aeson.Value
toJSONValue = Api.scriptDataToJson Api.ScriptDataJsonDetailedSchema
            . Api.unsafeHashableScriptData
            . fromPlutusData
            . PV2.toData

writeScript :: FilePath -> PV2.SerialisedScript -> IO (Either (Api.FileError ()) ())
writeScript file script = 
  Api.writeFileTextEnvelope @(Api.PlutusScript Api.PlutusScriptV2) (Api.File file) Nothing $ 
    PlutusScriptSerialised script

writeData :: PV2.ToData a => FilePath -> a -> IO ()
writeData file = LBS.writeFile file . Aeson.encode . toJSONValue

decodeDatum :: (PV2.FromData a) => Aeson.Value -> Maybe a
decodeDatum = either (const Nothing) (PV2.fromBuiltinData . fromCardanoScriptData)
            . Api.scriptDataFromJson Api.ScriptDataJsonDetailedSchema

parseScriptFromCBOR :: String -> PV2.SerialisedScript
parseScriptFromCBOR script =
  case Base16.decode $ encodeUtf8 script of
    Left e -> error $ "Failed to decode validator: " <> show e
    Right bytes' -> toShort bytes'

dataFromCBOR :: String -> Either LedgerBytesError PV2.Data
dataFromCBOR = fmap Serial.deserialise . decodeHex

decodeHex :: String -> Either LedgerBytesError LBS.ByteString
decodeHex = fmap (fromStrict . bytes) . fromHex . fromString

toCBOR :: Serial.Serialise a => a -> Text
toCBOR = encodeByteString . toStrict . Serial.serialise

-------------------------------------------------
-- Functions for parsing user input.
-------------------------------------------------
-- | Parse an asset from user supplied `String`. The input is expected to either be
-- "lovelace" or of the form "policy_id.asset_name".
readAsset :: String -> Either String Asset
readAsset s =
    if s == "lovelace" then Right $ Asset (PV2.adaSymbol,PV2.adaToken)
    else fmap Asset . (,) <$> readCurrencySymbol policy <*> readTokenName (drop 1 name)
  where
    (policy,name) = span (/='.') s

-- | Parse an asset amount from user supplied `String`. The asset name is expected to either be
-- "lovelace" or of the form "policy_id.asset_name".
readAssetAmount :: String -> Either String (Asset,Integer)
readAssetAmount s = case words s of
  [num,name] -> 
    (,) <$> readAsset name <*> maybeToRight ("Could not parse: " <> s) (readMaybe num)
  _ -> Left $ "Could not parse: " <> s

-- | Parse `CurrencySymbol` from user supplied `String`.
readCurrencySymbol :: String -> Either String PV2.CurrencySymbol
readCurrencySymbol s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.CurrencySymbol bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TokenName` from user supplied `String`.
readTokenName :: String -> Either String PV2.TokenName
readTokenName s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.TokenName bytes'
  Left msg                   -> Left $ show msg

-- | Parse `TxId` from user supplied `String`.
readTxId :: String -> Either String PV2.TxId
readTxId s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.TxId bytes'
  Left msg                   -> Left $ show msg

readTxOutRef :: String -> Either String PV2.TxOutRef
readTxOutRef s = PV2.TxOutRef <$> readTxId txHash <*> readIndex (drop 1 index)
  where
    (txHash,index) = span (/='#') s

    readIndex :: String -> Either String Integer
    readIndex i = case readMaybe i of
      Nothing -> Left $ "could not convert: " <> i
      Just i' -> Right i'

-- | Parse PubKeyHash from user supplied String
readPubKeyHash :: String -> Either String PV2.PubKeyHash
readPubKeyHash s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.PubKeyHash bytes'
  Left msg                   -> Left $ show msg

-- | Parse ValidatorHash from user supplied String
readScriptHash :: String -> Either String PV2.ScriptHash
readScriptHash s = case fromHex $ fromString s of
  Right (PV2.LedgerBytes bytes') -> Right $ PV2.ScriptHash bytes'
  Left msg                   -> Left $ show msg

-------------------------------------------------
-- Misc
-------------------------------------------------
toCardanoApiScript :: PV2.SerialisedScript -> Api.Script Api.PlutusScriptV2
toCardanoApiScript = Api.PlutusScript Api.PlutusScriptV2 . PlutusScriptSerialised

toLedgerScript :: PV2.SerialisedScript -> PV2.Script
toLedgerScript = PV2.Script

toVersionedLedgerScript :: PV2.SerialisedScript -> PV2.Versioned PV2.Script
toVersionedLedgerScript script = PV2.Versioned (toLedgerScript script) PV2.PlutusV2

wrapVersionedLedgerScript :: (PV2.Script -> a) -> PV2.Versioned PV2.Script -> PV2.Versioned a
wrapVersionedLedgerScript wrapper v@PV2.Versioned{PV2.unversioned} = 
  v{PV2.unversioned = wrapper unversioned}

scriptHash :: PV2.SerialisedScript -> PV2.ScriptHash
scriptHash =
  PV2.ScriptHash
    . Builtins.toBuiltin
    . Api.serialiseToRawBytes
    . Api.hashScript
    . toCardanoApiScript

datumHash :: (PV2.ToData a) => a -> PV2.DatumHash
datumHash = L.datumHash . L.Datum . PV2.dataToBuiltinData . PV2.toData

applyArguments :: PV2.SerialisedScript -> [PV2.Data] -> PV2.SerialisedScript
applyArguments p args =
    let termArgs = fmap (PLC.mkConstant ()) args
        applied t = PLC.mkIterAppNoAnn t termArgs
    in PV2.serialiseUPLC $ over UPLC.progTerm applied $ PV2.uncheckedDeserialiseUPLC p

unsafeFromRight :: Either a b -> b
unsafeFromRight (Right x) = x
unsafeFromRight _ = error "unsafeFromRight used on Left"

-- | Show the token name in hexidecimal.
showTokenName :: PV2.TokenName -> String
showTokenName (PV2.TokenName name) = show $ PV2.PubKeyHash name

unsafeToBuiltinByteString :: String -> Builtins.BuiltinByteString
unsafeToBuiltinByteString = (\(PV2.LedgerBytes bytes') -> bytes')
                          . unsafeFromRight
                          . fromHex
                          . fromString

unBuiltinByteString :: BuiltinByteString -> SBS.ByteString
unBuiltinByteString (BuiltinByteString bs) = bs
