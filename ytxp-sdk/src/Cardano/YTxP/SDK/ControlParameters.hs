{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.SDK.ControlParameters (
  -- * Types
  YieldingScripts (..),
  ControlParameters (..),
) where

import Cardano.YTxP.SDK.SdkParameters (SdkParameters (..))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  (.:),
  (.=),
 )
import Data.Text (Text)

{- | Scripts that yield to transaction families identified by reference scripts
which carry a state thread token

@since 0.1.0
-}
data YieldingScripts = YieldingScripts
  { yieldingMintingPolicies :: [Text]
  -- ^ @since 0.1.0
  , yieldingValidator :: Text
  -- ^ @since 0.1.0
  , yieldingStakingValidators :: [Text]
  -- ^ @since 0.1.0
  }

-- | @since 0.1.0
instance ToJSON YieldingScripts where
  {-# INLINEABLE toJSON #-}
  toJSON ys =
    object
      [ "yieldingMintingPolicies" .= yieldingMintingPolicies ys
      , "yieldingValidator" .= yieldingValidator ys
      , "yieldingStakingValidators" .= yieldingStakingValidators ys
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding ys =
    pairs $
      "yieldingMintingPolicies" .= yieldingMintingPolicies ys
        <> "yieldingValidator" .= yieldingValidator ys
        <> "yieldingStakingValidators" .= yieldingStakingValidators ys

-- | @since 0.1.0
instance FromJSON YieldingScripts where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "YieldingScripts" $ \obj -> do
    ysmp <- obj .: "yieldingMintingPolicies"
    ysv <- obj .: "yieldingValidator"
    ysvs <- obj .: "yieldingStakingValidators"
    pure $ YieldingScripts ysmp ysv ysvs

{- | Contains the compiled scripts along with the parameters
they were compiled against. This is useful for _library consumers_
and should contain all of the information needed to work with the
library.

@since 0.1.0
-}
data ControlParameters = ControlParameters
  { yieldingScripts :: YieldingScripts
  -- ^ @since 0.1.0
  , sdkParameters :: SdkParameters
  -- ^ @since 0.1.0
  }

-- | @since 0.1.0
instance ToJSON ControlParameters where
  {-# INLINEABLE toJSON #-}
  toJSON cp =
    object
      [ "yieldingScripts" .= yieldingScripts cp
      , "sdkParameters" .= sdkParameters cp
      ]
  {-# INLINEABLE toEncoding #-}
  toEncoding cp =
    pairs $
      "yieldingScripts" .= yieldingScripts cp
        <> "sdkParameters" .= sdkParameters cp

-- | @since 0.1.0
instance FromJSON ControlParameters where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withObject "ControlParameters" $ \obj -> do
    ys <- obj .: "yieldingScripts"
    cpi <- obj .: "sdkParameters"
    pure $ ControlParameters ys cpi
