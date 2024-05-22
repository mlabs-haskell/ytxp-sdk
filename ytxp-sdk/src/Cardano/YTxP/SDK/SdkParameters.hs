{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.SDK.SdkParameters (
  SdkParameters (..),
  YieldListSTCS (..), -- dont export constructor?
  Config (..),
  TracingMode (..),
) where

import Control.Monad ((<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  withText,
 )
import Data.String (IsString, fromString)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import PlutusLedgerApi.V2 (CurrencySymbol)

data TracingMode = DoTracing | NoTracing
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Compilation config parameters
newtype Config = Config
  { tracing :: TracingMode
  }
  deriving stock (Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

-- | Parameters available during compilation (therefore not containing any script hashes).
data SdkParameters = SdkParameters
  { stakingValidatorsNonceList :: [Natural]
  -- ^ A list of nonces for the yielding staking validators. One staking
  -- validator is compiled for each nonce.
  -- @since 0.1.0
  , mintingPoliciesNonceList :: [Natural]
  -- ^ A list of nonces for the yielding minting policies. One minting
  -- policy is compiled for each nonce.
  -- @since 0.1.0
  , authorisedScriptsSTCS :: YieldListSTCS
  -- ^ The Currency symbol of the token that identifies authorised reference scripts .
  -- @since 0.1.0
  , compilationConfig :: Config
  -- ^ Plutarch compilation config
  }
  deriving stock (Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Semantic newtype for the YieldList state thread currency symbol
newtype YieldListSTCS = YieldListSTCS CurrencySymbol
  deriving newtype (Eq, IsString)

instance FromJSON YieldListSTCS where
  {-# INLINEABLE parseJSON #-}
  parseJSON =
    (pure . YieldListSTCS)
      <=< withText "YieldListSTCS" (pure . fromString . unpack)

instance ToJSON YieldListSTCS where
  {-# INLINEABLE toJSON #-}
  toJSON (YieldListSTCS cs) = toJSON . show $ cs

  {-# INLINEABLE toEncoding #-}
  toEncoding (YieldListSTCS cs) = toEncoding . show $ cs
