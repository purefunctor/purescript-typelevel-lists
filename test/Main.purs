module Test.Main where

import Prelude

import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Data.List (type (:>), Nil')
import Type.Data.List as TL
import Type.Data.Peano as P
import Type.Prelude (Proxy(..))


data Literals

foreign import data LiteralSymbol :: Symbol -> Literals


spec :: Spec Unit
spec = do
  let xs = Proxy :: _ ( Int :> Char :> Nil' )

  describe "IsMember" do
    it "tests empty lists" do
      TL.isMember (Proxy :: _ Int) (Proxy :: _ Nil') `shouldEqual` false
    it "tests members" do
      TL.isMember (Proxy :: _ Int) xs `shouldEqual` true
    it "tests non-members" do
      TL.isMember (Proxy :: _ Boolean) xs `shouldEqual` false

  let ys = Proxy :: _ ( Int :> Nil' )
      zs = Proxy :: _ ( Char :> Nil' )
      nl = Proxy :: _ Nil'

  describe "Concat" do
    it "concatenates two lists" do
      TL.concat ys zs `shouldEqual` xs
    it "has a left unit" do
      TL.concat nl ys `shouldEqual` ys
    it "has a right unit" do
      TL.concat zs nl `shouldEqual` zs
    it "is an associative operation" do
      let l = ys `TL.concat` (ys `TL.concat` zs)
          r = (ys `TL.concat` ys) `TL.concat` zs
      l `shouldEqual` r

  describe "IsEmpty" do
    it "returns true on empty" do
      TL.isEmpty nl `shouldEqual` true
    it "returns false on non-empty" do
      TL.isEmpty xs `shouldEqual` false

  describe "Init" do
    it "returns the init items" do
      TL.init xs `shouldEqual` ys
    it "returns an empty list on singleton lists" do
      TL.init ys `shouldEqual` nl

  describe "Last" do
    it "returns the last item on regular lists" do
      TL.last xs `shouldEqual` ( Proxy :: _ Char )
    it "returns the last item on singleton lists" do
      TL.last zs `shouldEqual` ( Proxy :: _ Char )

  describe "Length" do
    it "returns zero on an empty list" do
      TL.length nl `shouldEqual` ( Proxy :: _ P.P0 )

    it "returns the length of a non-empty list" do
      TL.length xs `shouldEqual` ( Proxy :: _ P.P2 )

  describe "Take" do
    it "returns an empty list when given zero" do
      TL.take P.p0 xs `shouldEqual` nl
      TL.take P.p0 ys `shouldEqual` nl
      TL.take P.p0 nl `shouldEqual` nl
    it "returns an empty list for empty lists" do
      TL.take P.p1 nl `shouldEqual` nl
    it "returns a list with the first n items" do
      TL.take P.p1 xs `shouldEqual` ys

  describe "Drop" do
    it "returns the full list when given zero" do
      TL.drop P.p0 xs `shouldEqual` xs
      TL.drop P.p0 ys `shouldEqual` ys
      TL.drop P.p0 nl `shouldEqual` nl
    it "returns an empty list for empty lists" do
      TL.drop P.p1 nl `shouldEqual` nl
    it "returns a list without the first n items" do
      TL.drop P.p1 xs `shouldEqual` zs

  describe "Zip" do
    it "stops at Nil'" do
      TL.zip xs nl `shouldEqual` nl
      TL.zip nl xs `shouldEqual` nl
      TL.zip xs ys `shouldEqual` ( Proxy :: _ ( Tuple Int Int :> Nil' ) )
    it "zips two lists together" do
      TL.zip ys zs `shouldEqual` ( Proxy :: _ ( Tuple Int Char :> Nil' ) )

  describe "Map" do
    it "returns an empty list when empty" do
      TL.map ( Proxy :: _ Array ) nl `shouldEqual` nl
    it "transforms a non-empty list" do
      let r = Proxy :: _ ( Array Int :> Array Char :> Nil' )
      TL.map ( Proxy :: _ Array ) xs `shouldEqual` r
    it "allows mapping into another kind" do
      let k = Proxy :: _ ( "hello" :> Nil' )
          l = Proxy :: _ ( LiteralSymbol "hello" :> Nil' )
      TL.map ( Proxy :: _ LiteralSymbol ) k `shouldEqual` l

  let f = ( Proxy :: _ Tuple )
      a = ( Proxy :: _ String )

  describe "Fold" do
    it "returns the accumulator when given an empty list" do
      TL.fold f a nl `shouldEqual` a
    it "performs a left-associative fold" do
      let r = Proxy :: _ ( Tuple ( Tuple String Int ) Char )
      TL.fold f a xs `shouldEqual` r

  describe "Foldr" do
    it "returns the accumulator when given an empty list" do
      TL.foldr f a nl `shouldEqual` a
    it "performs a right-associative fold" do
      let r = Proxy :: _ ( Tuple Int ( Tuple Char String ) )
      TL.foldr f a xs `shouldEqual` r


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] spec
