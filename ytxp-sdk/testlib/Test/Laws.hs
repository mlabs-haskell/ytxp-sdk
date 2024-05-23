{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Laws (aesonLaws, aesonLawsWith) where

import Data.Aeson (FromJSON, ToJSON (toJSON), decode, encode)
import Data.Kind (Type)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  Property,
  counterexample,
  forAllShrink,
  (===),
 )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Type.Reflection (Typeable, tyConName, typeRep, typeRepTyCon)

{- | Verifies the laws that any valid type implementing both 'ToJSON' and
'FromJSON' should follow. More precisely, this checks:

1. Roundtripping: @'decode' '.' 'encode'@ @=@ @'Just'@
2. 'toJSON' and 'toEncoding' must agree: @'decode' '.' 'encode'@ @=@ @'Just'
'.' 'toJSON'@

@since 0.1.0
-}
aesonLaws ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Arbitrary a, Show a, Eq a) =>
  TestTree
aesonLaws = aesonLawsWith @a arbitrary shrink

{- | As 'aesonLaws', but allows explicit control of generationa and shrinking.

@since 0.1.0
-}
aesonLawsWith ::
  forall (a :: Type).
  (Typeable a, ToJSON a, FromJSON a, Eq a, Show a) =>
  Gen a ->
  (a -> [a]) ->
  TestTree
aesonLawsWith gen shr =
  testGroup
    groupName
    [ testProperty "decode . encode = Just" . forAllShrink gen shr $ \x ->
        Just x === (decode . encode $ x)
    , testProperty "decode . encode = Just . toJSON"
        . forAllShrink gen shr
        $ \x -> (decode . encode $ x) === (Just . toJSON $ x)
    ]
  where
    groupName :: String
    groupName = "Aeson laws for " <> typeName @a

-- Helpers

-- Helper for turning the name of an arbitrary Typeable into a String
typeName ::
  forall (a :: Type).
  (Typeable a) =>
  String
typeName = tyConName . typeRepTyCon $ typeRep @a
