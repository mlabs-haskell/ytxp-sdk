{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

{- | Module: Cardano.YTxP.Control.Parameters
Description: Data required to work with the compiled control scripts
-}
module Cardano.YTxP.SDK.SdkParameters (
  SdkParameters (..),
  AuthorisedScriptsSTCS (..),
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
import PlutusCore (DefaultUni)
import PlutusTx qualified
#if MIN_VERSION_plutus_ledger_api(1,1,0)
import PlutusLedgerApi.V3 (CurrencySymbol)
#else
import Plutus.V3.Ledger.Api (CurrencySymbol)
#endif

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
  , authorisedScriptsSTCS :: AuthorisedScriptsSTCS
  -- ^ The Currency symbol of the token that identifies authorised reference scripts .
  -- @since 0.1.0
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Semantic newtype for the YieldList state thread currency symbol
newtype AuthorisedScriptsSTCS = AuthorisedScriptsSTCS CurrencySymbol
  deriving newtype
    ( Eq
    , IsString
    , Show
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    , PlutusTx.Typeable DefaultUni
    , PlutusTx.Lift DefaultUni
    )

instance FromJSON AuthorisedScriptsSTCS where
  {-# INLINEABLE parseJSON #-}
  parseJSON =
    (pure . AuthorisedScriptsSTCS)
      <=< withText "AuthorisedScriptsSTCS" (pure . fromString . unpack)

instance ToJSON AuthorisedScriptsSTCS where
  {-# INLINEABLE toJSON #-}
  toJSON (AuthorisedScriptsSTCS cs) = toJSON . show $ cs

  {-# INLINEABLE toEncoding #-}
  toEncoding (AuthorisedScriptsSTCS cs) = toEncoding . show $ cs
