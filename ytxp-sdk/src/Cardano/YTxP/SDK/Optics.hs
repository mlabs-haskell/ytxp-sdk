module Cardano.YTxP.SDK.Optics where

import Cardano.YTxP.SDK.Redeemers (AuthorisedScriptIndex, AuthorisedScriptProofIndex, YieldingRedeemer)

import Control.Lens (makeClassy, makeClassyFor, makeWrapped)

makeWrapped ''AuthorisedScriptProofIndex

makeWrapped ''AuthorisedScriptIndex

makeClassyFor
  "HasYieldingRedeemer"
  "yieldingRedeemer"
  [ ("authorisedScriptIndex", "authorisedScriptIndex")
  , ("authorisedScriptProofIndex", "authorisedScriptProofIndex")
  ]
  ''YieldingRedeemer
