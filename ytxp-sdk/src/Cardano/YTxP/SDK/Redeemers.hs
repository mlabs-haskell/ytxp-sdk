{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex (AuthorisedScriptIndex), AuthorisedScriptPurpose (Minting, Spending, Rewarding), AuthorisedScriptProofIndex (AuthorisedScriptProofIndex), YieldingRedeemer (YieldingRedeemer, authorisedScriptIndex, authorisedScriptProofIndex)) where

import Cardano.YTxP.SDK.Vendored (EnumIsData (EnumIsData))
import GHC.Generics (Generic)
import PlutusTx qualified

-- | Represents an index into a authorised reference script in a TxInReferenceInput list
newtype AuthorisedScriptIndex = AuthorisedScriptIndex Integer
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{- The type of yielded to scripts
-}
data AuthorisedScriptPurpose = Minting | Spending | Rewarding
  deriving stock (Show, Generic, Enum, Bounded)
  deriving
    (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    via (EnumIsData AuthorisedScriptPurpose)

{- Index for the yielding redeemer
-}
newtype AuthorisedScriptProofIndex = AuthorisedScriptProofIndex (AuthorisedScriptPurpose, Integer)
  deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

{- | The redeemer passed to the yielding minting policy, validator,
and staking validators
-}
data YieldingRedeemer = YieldingRedeemer
  { authorisedScriptIndex :: AuthorisedScriptIndex
  -- ^ Integer The index of the TxInReferenceInput that contains the authorised reference script.
  , authorisedScriptProofIndex :: AuthorisedScriptProofIndex
  -- ^ A tuple containing yielded to script type and the index at which to find proof: this allows us to avoid having to loop through inputs/mints/withdrawls to find the script we want to ensure is run.
  }

PlutusTx.makeIsDataIndexed ''YieldingRedeemer [('YieldingRedeemer, 0)]
