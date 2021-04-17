module SplitSpec where

import Split
import Test.Hspec
import Test.QuickCheck

-- splitSpec0 verifie juste si split de "aa/bb/ccc/dd d" delimité avec le
-- character '/' est bien egale aux partitions des tokens correspondent
splitSpec0 = do
  describe "split" $ do
    it "splits a string wrt. a given character into a list of words" $
      (split '/' "aa/bb/ccc/dd d") `shouldBe` ["aa", "bb", "ccc", "dd d"]



-- splitSpec1 teste en utilisant des fonctions de prop QuickCheck
-- la proprieté qui verifie si la fonction unsplit neutralise-
-- bien la fonction split, en affichant la distribution sur les tailles
-- du domaine de test fait par QuickCheck - On deduit le type par le typage
-- des fonctions donc (c::Char) et (xs::String)
splitSpec1 = do
  describe "split" $ do
    it "can be undone with unsplit (v1)" $
      property $
        \c xs -> collect (length xs) $ prop_split_unsplit c xs

-- #{DOC
-- elements :: [a] -> Gen a
--      Generates one of the given values. The input list must be non-empty.
-- forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
--      Explicit universal quantification: uses an explicitly given test case generator.
-- #DOC}

-- splitSpec2 test aussi la derniere propriete (unsplit(split(c,x)) = x) mais
-- en utilisant des tests aleatoires un peu plus explicites.
-- `elements xs` force que `c` appartient a xs pour avoir des tests plus interessants
-- xs reste toujours un string aleatoire.
splitSpec2 = do
  describe "split" $ do
    it "can be undone with unsplit (v2)" $
      property $
        \xs -> forAll (elements xs) $ \c -> collect (length xs) $ prop_split_unsplit c xs

-- #{DOC
-- oneof :: [Gen a] -> Gen a
--      Randomly uses one of the given generators. The input list must be non-empty.
-- #DOC}

-- splitSpec3 teste la meme propriete `split_unsplit` mais cette fois en utilisant
-- une domaine explicite |"bla bla bli", "toto", "", "un..| pour notre entree r(xs :: String) 
-- ainsi le character `whiteSpace` comme delimiter.
splitSpec3 = do
  describe "split" $ do
    it "can be undone with unsplit (v3)" $
      property $
        forAll
          ( oneof
              [ return "bla bla bli",
                return "toto",
                return "",
                return "un    deux trois   quatre"
              ]
          )
          $ \xs -> prop_split_unsplit ' ' xs
