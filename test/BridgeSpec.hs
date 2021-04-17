module BridgeSpec where

import Bridge
import Test.Hspec
import Test.QuickCheck

prop_initBridge_inv :: Int -> Property
prop_initBridge_inv lim =
  (initBridge_pre lim) ==> property $ islandBridge_inv (initBridge lim)

-- Pour tester directement dans ghci :
-- stack ghci --test --main-is Spec
-- (choisir   Spec comme module main)
-- puis dans ghci :
-- blabla...> :set prompt "> "
-- > import Test.QuickCheck
-- > quickCheck prop_initBridge_inv

-- (et maintenir ghci ouvert pour la suite, et :reload
-- en cas de changement de code source)

bridgeSpecInit = do
  describe "initBridge" $ do
    it "preserves the invariant" $ property $ \lim -> prop_initBridge_inv lim

-- Exemple de générateur aléatoire monadique (cf. cours 8)
-- Creer un generateur de IslandBridge aleatoire en choisisent des attribut dans certain borne
-- en prenent en compte notre logique pour la creation d'entite c-a-d
-- distinction entre BridgeClosed et BridgeOpened mais la generation peut generer
-- des IslandBridge qui satisfait pas l'invariant par example :
-- lim = 10, nbI = 20, nbTo = 30, nbFrom = 40; `islandBridge_inv` sera donc faux
genBridgeFree :: Gen IslandBridge
genBridgeFree = do
  lim <- choose (1, 100) -- la limite initiale
  nbI <- choose (0, 50)
  nbTo <- choose (0, 50)
  nbFrom <- choose (0, 50)
  return $
    if nbI + nbTo + nbFrom == lim
      then BridgeClosed lim nbTo nbFrom
      else BridgeOpened lim nbTo nbI nbFrom

-- Exemple de générateur garantissant l'invariant
-- similaire a genBridgeFree mais en prenant en compte l'invariant
-- on fixe d'abord notre limite et a partir de ca on genere les autres
-- attributs pour faire en sorte que `islandBridge_inv` soit vrai
genBridgeOk :: Gen IslandBridge
genBridgeOk = do
  lim <- choose (1, 100) -- la limite initiale
  nbCars <- choose (0, lim) --
  nbI <- choose (0, nbCars)
  nbTo <- choose (0, nbI)
  let nbFrom = nbCars - (nbI + nbTo)
  return $ mkBridge lim nbTo nbI nbFrom

-- verifie les jeux de teste generer avec `genBridgeOk` en verifiant
-- pour chaque IslandBridge genere, l'invariant
prop_genBridgeOK_inv :: Property
prop_genBridgeOK_inv = forAll genBridgeOk $ islandBridge_inv

-- >> quickCheck prop_genBridgeOK_inv
-- +++ OK, passed 100 tests.

-- en utilisant Hsepc integre avec quickCheck decrit et test le dernier prop
bridgeSpecGenOk = do
  describe "genBridgeOk" $ do
    it "generates bridges that satisfy their invariant" $
      property prop_genBridgeOK_inv

-- verifie les jeux de teste generer avec `genBridgeFree` en verifiant
-- pour chaque IslandBridge genere, l'invariant; !! la property sera faux
-- pour certain testes
prop_genBridgeFree_inv :: Property
prop_genBridgeFree_inv = forAll genBridgeFree $ islandBridge_inv

-- quickCheck prop_genBridgeFree_inv

-- en utilisant Hsepc integre avec quickCheck decrit et test le dernier prop
bridgeSpecGenFree = do
  describe "genBridgeFree" $ do
    it "generates bridges that satisfy their invariant" $
      property prop_genBridgeFree_inv

-- Générateur par défaut, qui peut être incohérent dans max. 20% des cas
-- (ce qui permet de tester les préconditions)
instance Arbitrary IslandBridge where
  arbitrary =
    frequency
      [ (2, genBridgeFree), -- 20% de génération libre
        (8, genBridgeOk) -- 80% de génération sûre
      ]

-- quickCheck islandBridge_inv

-- pour un IslandBridge(b) donnee verifie l'implication:
-- (si precondition et inveriant sont vrai alors l'invariant du resultat de l'appelle a `enterToIsland` sera aussi vrai)
-- classify permet tester d'etiquete les tests on peut combiner les classify et les appele
-- sur un Property
prop_enterToIsland_inv :: IslandBridge -> Property
prop_enterToIsland_inv b =
  (islandBridge_inv b)
    && (enterToIsland_pre b)
    ==> classify
      (bridgeLimit b < 10)
      "small capacity (<10)"
      ( classify
          (bridgeLimit b < 50)
          "medium capacity (<50)"
          ( classify
              (bridgeLimit b <= 100)
              "large capacity (>=50)"
              ( property $ islandBridge_inv (enterToIsland b)
              )
          )
      )

-- >> quickCheck prop_enterToIsland_inv

-- pour ajouter au module Spec qui utilise Hspec
enterToIslandSpec = do
  describe "enterToIsland" $ do
    it "preserves the invariant" $
      property prop_enterToIsland_inv

-- idem pour leaveToIsland
prop_leaveToIsland_inv :: IslandBridge -> Property
prop_leaveToIsland_inv b =
  (islandBridge_inv b)
    && (leaveToIsland_pre b)
    ==> classify (bridgeLimit b < 10) "small capacity (<10)"
    $ classify (bridgeLimit b < 50) "medium capacity (<50)" $
      classify (bridgeLimit b <= 100) "large capacity (>=50)" $
        property $ islandBridge_inv (leaveToIsland b)

-- >> quickCheck prop_leaveToIsland_inv

leaveToIslandSpec = do
  describe "leaveToIsland" $ do
    it "preserves the invariant" $
      property prop_leaveToIsland_inv

-- idem pour enterFromIsland
prop_enterFromIsland_inv :: IslandBridge -> Property
prop_enterFromIsland_inv b =
  (islandBridge_inv b)
    && (enterFromIsland_pre b)
    ==> classify (bridgeLimit b < 10) "small capacity (<10)"
    $ classify (bridgeLimit b < 50) "medium capacity (<50)" $
      classify (bridgeLimit b <= 100) "large capacity (>=50)" $
        property $ islandBridge_inv (enterFromIsland b)

-- >> quickCheck prop_enterFromIsland_inv

enterFromIslandSpec = do
  describe "enterFromIsland" $ do
    it "preserves the invariant" $
      property prop_enterFromIsland_inv

-- idem pour leaveFromIsland
prop_leaveFromIsland_inv :: IslandBridge -> Property
prop_leaveFromIsland_inv b =
  (islandBridge_inv b)
    && (leaveFromIsland_pre b)
    ==> classify (bridgeLimit b < 10) "small capacity (<10)"
    $ classify (bridgeLimit b < 50) "medium capacity (<50)" $
      classify (bridgeLimit b <= 100) "large capacity (>=50)" $
        property $ islandBridge_inv (leaveFromIsland b)

-- >> quickCheck prop_leaveFromIsland_inv

leaveFromIslandSpec = do
  describe "leaveFromIsland" $ do
    it "preserves the invariant" $
      property prop_leaveFromIsland_inv
