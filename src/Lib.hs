{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib where
    -- ( someFunc
    -- ) where

import Statistics.Distribution (probability)
import Statistics.Distribution.Binomial (binomial)
import Data.List
import qualified Data.Map.Strict as Map
import qualified  Data.Text.Lazy as TL
import Web.Scotty as S
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Internal (customAttribute)
import Network.Wai.Middleware.Static


import Data.Aeson
import GHC.Generics
import Views (indexPage)


someFunc :: IO ()
someFunc = scotty 5000 $ do
  middleware $ staticPolicy (noDots >-> addBase "Static")
  get "/" indexPage
  post "/data" postRoute

postRoute :: ActionM ()
postRoute = do
  shippy <- S.body
  maybeShip shippy
  

maybeShip param = let shippy = decode param :: Maybe Parameters
  in case (shippy) of
       (Just fleet) -> S.json $ calcFleet fleet
       Nothing -> S.json $ calcFleet testParam

data ShipPart = Empty
              | IonCannon
              | ElectronComputer
              | Hull
              | NuclearDrive
              | NuclearSource
              | PlasmaCannon
              | AntimatterCannon
              | PlasmaMissile
              | PositronComputer
              | GluonComputer
              | GaussShield
              | PhaseShield
              | ImprovedHull
              | FusionDrive
              | TachyonDrive
              | FusionSource
              | TachyonSource
              deriving (Show, Generic)

instance ToJSON ShipPart
instance FromJSON ShipPart

data Parameters = Parameters {ships :: [([ShipPart], Count)], enemyShield :: Count}
  deriving (Show, Generic)

instance ToJSON Parameters
instance FromJSON Parameters


partsToShip :: [ShipPart] -> Ship
partsToShip xs = foldr (\x acc -> sumShips acc (partToShip x)) (Ship 0 0 0 0 0) xs
  where sumShips (Ship i p a c m) (Ship i' p' a' c' m') =
          Ship {ions = i + i'
               , plasmas = p + p'
               , antis = a + a'
               , computers = c + c'
               , missiles = m + m'}

partToShip :: ShipPart -> Ship
partToShip IonCannon = Ship 1 0 0 0 0
partToShip ElectronComputer = Ship 0 0 0 1 0
partToShip PlasmaCannon = Ship 0 1 0 0 0 
partToShip AntimatterCannon = Ship 0 0 1 0 0
partToShip PlasmaMissile = Ship 0 0 0 0 1
partToShip PositronComputer = Ship 0 0 0 2 0
partToShip GluonComputer = Ship 0 0 0 3 0
partToShip _ = Ship 0 0 0 0 0
  

type Damage = Int
type Dices = Int
type ToHit = Int
type Count = Int
type Percentage = Double

type ResMap = Map.Map Int Percentage


data Probability = Prob [(Damage, Percentage)] deriving Show
data Hits = Hits [(Count, Percentage)] deriving Show
type Damages = [(Damage, Percentage)] 
data Results = Results {labels :: [Damage], probs:: [Percentage],
                        mprobs :: [Percentage]}
             deriving (Show, Generic)

instance ToJSON Results
instance FromJSON Results


testParam :: Parameters
testParam = Parameters {ships = [([IonCannon, IonCannon, ElectronComputer, Empty], 2),
                                 ([IonCannon, PlasmaCannon, Hull, Empty], 1)]
                       , enemyShield = 0}


data Ship = Ship {ions     :: Count,
                  plasmas  :: Count,
                  antis    :: Count,
                  computers:: ToHit,
                  missiles :: Count} deriving (Generic, Show)

instance ToJSON Ship 

instance FromJSON Ship

type EnemyShields = Count


calcHits :: Dices -> ToHit -> Hits
calcHits k hit
  | k < 1 || hit < 1 || hit > 6 = Hits []
  | otherwise = Hits $ fmap (\x -> (x, probability binom x)) [0..k]
  where binom = binomial k p
        p     = if hit <= 2 then fromIntegral 5 /6 else fromIntegral (6 - hit + 1) / 6 

calcDmgs :: Ship -> EnemyShields -> Damages
calcDmgs ship shield = combineDmgs [ionDmg,plasmaDmg,antiDmg]
  where ionDmg    = hitsToDmgs 1 $ calcHits (ions ship) toHit
        plasmaDmg = hitsToDmgs 2 $ calcHits (plasmas ship) toHit
        antiDmg = hitsToDmgs 4 $ calcHits (antis ship) toHit
        toHit | shield - (computers ship) > 0  = 6
              | otherwise = 6 - (computers ship) + shield

calcMissiles :: Ship -> EnemyShields -> Damages
calcMissiles ship shield = hitsToDmgs 2 $ calcHits (missiles ship * 2) toHit
  where toHit | shield - (computers ship) > 0  = 6
              | otherwise = 6 - (computers ship) + shield

hitsToDmgs :: Damage -> Hits -> Damages
hitsToDmgs d (Hits xs) = fmap (\x -> (fst x * d, snd x)) xs
  
combineDmgs :: [Damages] -> Damages
combineDmgs [] = []
combineDmgs dmgs = foldr1 combine dmgs
  where combine xs [] =  xs
        combine [] ys =  ys
        combine xs ys =  func <$> xs <*> ys        
        func = \x y -> (fst x + (fst y), snd x * (snd y))


sortMerge :: Damages -> ResMap
sortMerge xs = foldr helper startMap xs
  where largest = foldr (\x acc -> if fst x > acc then fst x else acc) 0 xs
        startMap = Map.fromList [(k, 0::Percentage) | k<-[0..largest]]
        helper x acc = Map.insertWith (+) (fst x) (snd x) acc


calcFleet :: Parameters ->  Results
calcFleet (Parameters fleet shields) = 
  Results {labels = labels', probs = probs', mprobs = mprobs'}
  where mergedShips = uncurry mergeShip <$> fmap (\x -> (partsToShip (fst x), snd x)) fleet
        cannonsDmgs = flip calcDmgs shields <$> mergedShips
        missileDmgs = flip calcMissiles shields <$> mergedShips
        otherDmgs = sortMerge $ combineDmgs cannonsDmgs
        misDmg = sortMerge $ combineDmgs missileDmgs
        len = Prelude.max (Map.size otherDmgs) (Map.size misDmg)
        labels' = [0..len - 1]                  
        probs' = Map.elems $ Map.map (*100) $ fillMap otherDmgs labels'
        mprobs' = Map.elems $ Map.map (*100) $ fillMap misDmg labels'        

mergeShip :: Ship -> Count -> Ship
mergeShip ship k = Ship {ions = ions ship * k
                        , plasmas = plasmas ship * k
                        , antis = antis ship * k
                        , missiles = missiles ship * k
                        , computers = computers ship}

fillMap mp l = foldr fun mp l
  where fun x acc = if Map.notMember x mp then Map.insert x 0 acc else acc
          
        




