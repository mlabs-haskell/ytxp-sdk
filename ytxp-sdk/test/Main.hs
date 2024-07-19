module Main (main) where

import Cardano.YTxP.SDK.ControlParameters (ControlParameters (ControlParameters), HexStringScript (HexStringScript), YieldingScripts (YieldingScripts, yieldingMintingPolicies, yieldingStakingValidators, yieldingValidator), hexTextToSbs, sbsToHexText)
import Cardano.YTxP.SDK.SdkParameters (
  AuthorisedScriptsSTCS (AuthorisedScriptsSTCS),
  SdkParameters (SdkParameters),
 )
import Control.Monad (guard)
import Data.Aeson (encode)
import Data.ByteString.Short (ShortByteString)
import Data.Text (unpack)
import GHC.Exts (fromList, toList)
import PlutusLedgerApi.V1.Value (
  CurrencySymbol (CurrencySymbol),
 )
import PlutusLedgerApi.V2 (getLedgerBytes)
import Test.Laws (aesonLawsWith)
import Test.QuickCheck (
  Gen,
  NonNegative (getNonNegative),
  arbitrary,
  counterexample,
  shrink,
  (===),
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests, forAllShrinkShow, testProperty)

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "serialization" $
    [ testProperty "Hex encoding of ShortByteString roundtrips"
        . forAllShrinkShow genSBS shrinkSBS (show . toList)
        $ \sbs ->
          let converted = sbsToHexText sbs
           in counterexample ("Converted: " <> unpack converted) $
                Just sbs === (hexTextToSbs . sbsToHexText $ sbs)
    , aesonLawsWith @SdkParameters genSdkParams (const [])
    , aesonLawsWith @ControlParameters genControlParams (const [])
    , goldenVsString
        "SdkParameters"
        "goldens/SdkParameters.golden"
        (pure . encode $ sampleYLS)
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max 10_000

-- Golden data

sampleYLS :: SdkParameters
sampleYLS =
  SdkParameters
    [1, 2]
    [1, 2, 3]
    (AuthorisedScriptsSTCS dummySymbolOne)

-- Generators and shrinkers

-- TODO: This definitely needs more thought.
genSdkParams :: Gen SdkParameters
genSdkParams = do
  stakingValsNonceList <- map (fromInteger . getNonNegative) <$> arbitrary
  mintingPoliciesNonceList <- map (fromInteger . getNonNegative) <$> arbitrary
  pure $
    SdkParameters
      stakingValsNonceList
      mintingPoliciesNonceList
      (AuthorisedScriptsSTCS dummySymbolOne)

genControlParams :: Gen ControlParameters
genControlParams = do
  sdkParameters <- genSdkParams
  yieldingMP <- genSBS
  yieldingValidator <- genSBS
  yieldingSV <- genSBS
  pure $
    ControlParameters
      ( YieldingScripts
          { yieldingMintingPolicies = [HexStringScript @"YieldingMP" yieldingMP]
          , yieldingValidator = HexStringScript @"YieldingValidator" yieldingValidator
          , yieldingStakingValidators = [HexStringScript @"YieldingSV" yieldingSV]
          }
      )
      sdkParameters

genSBS :: Gen ShortByteString
genSBS = fromList <$> arbitrary

shrinkSBS :: ShortByteString -> [ShortByteString]
shrinkSBS sbs = do
  let asList = toList sbs
  shrunk <- shrink asList
  guard (not . null $ shrunk)
  pure . fromList $ shrunk

-- TODO: Make proper generator for cs

-- | Sample symbol for tests
dummySymbolOne :: CurrencySymbol
dummySymbolOne =
  CurrencySymbol $
    getLedgerBytes "00000000000000000000000000000000000000000000000000000000"
