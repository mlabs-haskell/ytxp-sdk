module Main (main) where

import Cardano.YTxP.SDK.SdkParameters (
  Config (Config),
  SdkParameters (SdkParameters),
  TracingMode (DetTracing, DoTracing, DoTracingAndBinds, NoTracing),
 )
import Data.Aeson (encode)
import Test.Laws (aesonLawsWith)
import Test.QuickCheck (
  Gen,
  NonNegative (getNonNegative),
  arbitrary,
  elements,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.QuickCheck (QuickCheckTests)

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "serialization" $
    [ aesonLawsWith @SdkParameters genCPI (const [])
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
    "aaaa"
    (Config NoTracing)

-- Generators and shrinkers

-- TODO: This definitely needs more thought.
genCPI :: Gen SdkParameters
genCPI = do
  stakingValsNonceList <- map (fromInteger . getNonNegative) <$> arbitrary
  mintingPoliciesNonceList <- map (fromInteger . getNonNegative) <$> arbitrary
  tm <- elements [NoTracing, DoTracing, DetTracing, DoTracingAndBinds]
  pure $
    SdkParameters
      stakingValsNonceList
      mintingPoliciesNonceList
      "aaaa"
      (Config tm)
