{-# LANGUAGE QuantifiedConstraints #-}

{- | Vendored utilities from open source libraries.
See the appropriate License for details on usage.
-}
module Cardano.YTxP.SDK.Vendored (
  EnumIsData (EnumIsData),
) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import PlutusTx (
  FromData (fromBuiltinData),
  ToData (toBuiltinData),
  UnsafeFromData (unsafeFromBuiltinData),
 )

--------------------------------------------------------------------------------
-- PEnumData

{- |
  Wrapper for deriving 'ToData', 'FromData' using an Integer representation via 'Enum'.

  Vendored from LPE
  TODO: Licensing info

  @since 1.1.0
-}
newtype EnumIsData (a :: Type) = EnumIsData a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => ToData (EnumIsData a) where
  toBuiltinData = coerce $ toBuiltinData . toInteger . fromEnum @a

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => FromData (EnumIsData a) where
  fromBuiltinData = coerce $ fmap (toEnum @a . fromInteger) . fromBuiltinData @Integer

-- | @since 1.1.0
instance forall (a :: Type). (Enum a) => UnsafeFromData (EnumIsData a) where
  unsafeFromBuiltinData = coerce . toEnum @a . fromInteger . unsafeFromBuiltinData @Integer
