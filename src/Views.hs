{-# LANGUAGE OverloadedStrings #-}
module Views
  (indexPage) where

import qualified  Data.Text.Lazy as TL
import Web.Scotty as S
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Text.Blaze.Internal (customAttribute, dataAttribute)

-- Helper functions for correct attributes
integrity :: AttributeValue -> Attribute
integrity = customAttribute "integrity"

crossorigin :: AttributeValue -> Attribute
crossorigin = customAttribute "crossorigin"

dataToggle :: AttributeValue -> Attribute
dataToggle = dataAttribute "toggle"

dataTarget :: AttributeValue -> Attribute
dataTarget = dataAttribute "target"

ariaExpanded :: AttributeValue -> Attribute
ariaExpanded = customAttribute "aria-expanded"

ariaControls :: AttributeValue -> Attribute
ariaControls = customAttribute "aria-controls"

indexPage :: ActionM ()
indexPage = do
  S.html . TL.pack . renderHtml . (docTypeHtml ! lang "en" )$ do
    headerStuff   
    H.body $ do
      H.div ! class_ "container-fluid" $ do
        jumbotron
      H.div ! class_ "container" $ do
        h1 "How to use"
        p $ mconcat ["Specify your ship and fleet composition (remember to add ships!). "
                    , "The resulting firepower for the missile round"
                    , " and first cannon round is calculated and displayed"
                    , " as probability of each outcome for the whole fleet firing"
                    , " a salvo."]
      H.div ! class_ "container" $ do
        H.div ! class_ "row my-3" $ do
          H.div ! class_ "col" $ interceptorCard
          H.div ! class_ "col" $ cruiserCard
        H.div ! class_ "row my-3" $ do
          H.div ! class_ "col" $ dreadnoughtCard
          H.div ! class_ "col" $ starbaseCard
        H.div ! class_ "d-flex align-items-baseline" $ do
          H.h4 ! class_ "mx-2" $ "Enemy ship's shields"
          selectEnemyShields 
        H.canvas ! A.id "myChart" ! class_ "my-4" $ ""
        H.script ! A.src "/js/Chart.js" $ ""
        chartScript


headerStuff = H.head $ do
  H.title "Eclipse boardgame's ship analysis"
  H.meta ! charset  "utf-8"
  H.meta ! name "viewport" ! content "width=device-width, initial-scale=1, shrink-to-fit=yes" 
  H.link ! rel "stylesheet" !
    href "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/css/bootstrap.min.css" !
    integrity "sha384-MCw98/SFnGE8fJT3GXwEOngsV7Zt27NXFoaoApmYm81iuXoPkFOJwJ8ERdknLPMO" !
    crossorigin "anonymous"
  H.script ! src "https://ajax.aspnetcdn.com/ajax/jQuery/jquery-3.3.1.min.js" $ ""
  -- H.script ! src "https://code.jquery.com/jquery-3.3.1.slim.min.js" !
  --   integrity "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo" !
  --   crossorigin "anonymous" $ ""
  H.script ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.3/umd/popper.min.js" !
    integrity "sha384-ZMP7rVo3mIykV+2+9J3UJ46jBk0WLaUAdn689aCwoqbBJiSnjAK/l8WvCWPIPm49" !
    crossorigin "anonymous" $ ""
  H.script ! src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.3/js/bootstrap.min.js" !
    integrity "sha384-ChfqqxuZUCnJSK3+MXmPNIyE6ZbWh2IMqE241rYiqJxyMiZ6OW/JmZQ5stwEULTy" !
    crossorigin "anonymous" $ ""


jumbotron = H.div ! class_ "jumbotron bg-primary text-white" $ do
  H.h1 ! class_ "display-4" $ "Eclipse boardgame's ship analysis tool"
  H.p ! class_ "lead" $ mconcat ["Analyse different ship combinations damage's probabibility.",
                                 " After assembling ship and fleet, you can see what kind of",
                                 " cannon and missile damage is inflicted."]


chartScript = H.script ! A.src "/js/chart_data.js" $ ""


interceptorCard :: Html
interceptorCard =  H.div ! class_ "card" $ do
    H.div ! class_ "card-header" $ do
      H.div ! class_ "d-flex align-items-baseline" $ do
        H.h3 ! class_ "mx-2" $ "Interceptor"
        H.button ! class_ "btn btn-primary" ! type_ "button" ! dataToggle "collapse"
          ! dataTarget "#intercollapse" ! ariaExpanded "false"
          ! ariaControls "intercollapse" $ "Show/Hide"
    H.div ! class_ "card-body"  $ do
      H.div ! class_ "collapse" ! A.id "intercollapse" $ do 
        select8Ships "intercept_fleet"
        H.ul ! class_ "list-group" $ do
          listItem' "intercept_1" selectEmpty
          listItem' "intercept_2" selectIon
          listItem' "intercept_3" selectNuclearDrive
          listItem' "intercept_4" selectNuclearSource

cruiserCard :: Html
cruiserCard =  H.div ! class_ "card" $ do
    H.div ! class_ "card-header" $ do
      H.div ! class_ "d-flex align-items-baseline" $ do
        H.h3 ! class_ "mx-2" $ "Cruiser"
        H.button ! class_ "btn btn-primary" ! type_ "button" ! dataToggle "collapse"
          ! dataTarget "#cruisercollapse" ! ariaExpanded "false"
          ! ariaControls "cruisercollapse" $ "Show/Hide"
    H.div ! class_ "card-body" $ do
      H.div ! class_ "collapse" ! A.id "cruisercollapse" $ do 
        select4Ships "cruiser_fleet"
        H.ul ! class_ "list-group" $ do
          listItem' "cruiser_1" selectEmpty
          listItem' "cruiser_2" selectIon
          listItem' "cruiser_3" selectNuclearDrive
          listItem' "cruiser_4" selectNuclearSource
          listItem' "cruiser_5" selectHull
          listItem' "cruiser_6" selectComputer

dreadnoughtCard :: Html
dreadnoughtCard =  H.div ! class_ "card" $ do
    H.div ! class_ "card-header" $ do
      H.div ! class_ "d-flex align-items-baseline" $ do
        H.h3 ! class_ "mx-2" $ "Dreadnought"
        H.button ! class_ "btn btn-primary" ! type_ "button" ! dataToggle "collapse"
          ! dataTarget "#dreadcollapse" ! ariaExpanded "false"
          ! ariaControls "dreadcollapse" $ "Show/Hide"
    H.div ! class_ "card-body" $ do
      H.div ! class_ "collapse" ! A.id "dreadcollapse" $ do 
        select2Ships "dread_fleet"
        H.ul ! class_ "list-group" $ do
          listItem' "dread_1" selectEmpty
          listItem' "dread_2" selectIon
          listItem' "dread_3" selectIon
          listItem' "dread_4" selectNuclearDrive
          listItem' "dread_5" selectNuclearSource 
          listItem' "dread_6" selectHull
          listItem' "dread_7" selectHull
          listItem' "dread_8" selectComputer

starbaseCard :: Html
starbaseCard =  H.div ! class_ "card" $ do
    H.div ! class_ "card-header" $ do
      H.div ! class_ "d-flex align-items-baseline" $ do
        H.h3 ! class_ "mx-2" $ "Starbase"
        H.button ! class_ "btn btn-primary" ! type_ "button" ! dataToggle "collapse"
          ! dataTarget "#starcollapse" ! ariaExpanded "false"
          ! ariaControls "starcollapse" $ "Show/Hide"        
    H.div ! class_ "card-body" $ do
      H.div ! class_ "collapse" ! A.id "dreadcollapse" $ do 
        select4Ships "starbase_fleet"
        H.ul ! class_ "list-group" $ do
          listItem' "starbase_1" selectEmpty
          listItem' "starbase_2" selectIon
          listItem' "starbase_3" selectHull
          listItem' "starbase_4" selectHull
          listItem' "starbase_5" selectComputer

selectEnemyShields :: Html
selectEnemyShields = H.select ! class_ "custom-select" ! A.id "enemy-shields" $ do
  H.option ! value "0" $ "No shields"
  H.option ! value "1" $  "One" 
  H.option ! value "2" $ "Two"
  H.option ! value "3" $  "Three"
  H.option ! value "4" $  "Four"
  H.option ! value "5" $  "Five"
  


select8Ships :: AttributeValue -> Html
select8Ships idx =  H.select ! class_ "custom-select my-2" ! A.id idx $ do
  optionsTo4
  H.option ! value "5" $  "Five"
  H.option ! value "6" $  "Six"
  H.option ! value "7" $  "Seven"
  H.option ! value "8" $  "Eight"

select4Ships :: AttributeValue -> Html
select4Ships idx =  H.select ! class_ "custom-select my-2" ! A.id idx $ do
  optionsTo4

select2Ships :: AttributeValue -> Html
select2Ships idx =  H.select ! class_ "custom-select my-2" ! A.id idx $ do
  H.option ! value "0" $ "Zero in fleet"
  H.option ! value "1" $  "One" 
  H.option ! value "2" $ "Two"


optionsTo4 = do
  H.option ! value "0" $ "Zero in fleet"
  H.option ! value "1" $  "One" 
  H.option ! value "2" $ "Two"
  H.option ! value "3" $  "Three"
  H.option ! value "4" $  "Four"



listItem :: Html -> AttributeValue -> AttributeValue -> AttributeValue -> Html
listItem name idx idinc iddec= H.li ! class_ "list-group-item" $ do
  H.div ! class_ "d-flex align-items-baseline"  $ do
    H.div ! class_ "p-2 flex-grow-1" $ do
      H.div ! class_ "d-inline-flex" $ do
        H.h4 ! class_ "bg-primary text-white rounded py-2 px-3 m-1" ! A.id idx $ "0"
        H.h5 ! class_ "m-1 py-2" $ "x"
        H.h4 ! class_ "m-1 py-2" $ name
    H.div ! class_ "p-2" $ do
      H.button ! type_ "button" ! class_ "btn btn-primary" ! A.id idinc $ "Increase"
      H.button ! type_ "button" ! class_ "btn btn-secondary ml-1" ! A.id iddec$ "Decrease"

listItem' :: AttributeValue -> (AttributeValue -> Html) -> Html
listItem' idx f = H.li ! class_ "list-group-item" $ do
  f idx

selectEmpty :: AttributeValue -> Html
selectEmpty idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" ! selected "" $ "Empty"
  H.option ! value "IonCannon" $ "Ion Cannon"
  H.option ! value "ElectronComputer" $ "Electron Computer"
  H.option ! value "Hull" $ "Hull"
  H.option ! value "NuclearDrive" $ "Nuclear Drive"
  H.option ! value "NuclearSource" $ "Nuclear Source"
  selectTechnologies

selectIon :: AttributeValue -> Html
selectIon idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" $ "Empty"
  H.option ! value "IonCannon" ! selected "" $ "Ion Cannon"
  H.option ! value "ElectronComputer" $ "Electron Computer"
  H.option ! value "Hull" $ "Hull"
  H.option ! value "NuclearDrive" $ "Nuclear Drive"
  H.option ! value "NuclearSource" $ "Nuclear Source"
  selectTechnologies


selectHull :: AttributeValue -> Html
selectHull idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" $ "Empty"
  H.option ! value "IonCannon" $ "Ion Cannon"
  H.option ! value "ElectronComputer" $ "Electron Computer"
  H.option ! value "Hull" ! selected "" $ "Hull"
  H.option ! value "NuclearDrive" $ "Nuclear Drive"
  H.option ! value "NuclearSource" $ "Nuclear Source"
  selectTechnologies


selectComputer :: AttributeValue -> Html
selectComputer idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" $ "Empty"
  H.option ! value "IonCannon" $ "Ion Cannon"
  H.option ! value "ElectronComputer" ! selected "" $ "Electron Computer"
  H.option ! value "Hull" $ "Hull"
  H.option ! value "NuclearDrive" $ "Nuclear Drive"
  H.option ! value "NuclearSource" $ "Nuclear Source"
  selectTechnologies

selectNuclearDrive :: AttributeValue -> Html
selectNuclearDrive idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" $ "Empty"
  H.option ! value "IonCannon" $ "Ion Cannon"
  H.option ! value "ElectronComputer" $ "Electron Computer"
  H.option ! value "Hull" $ "Hull"
  H.option ! value "NuclearDrive" ! selected "" $ "Nuclear Drive"
  H.option ! value "NuclearSource" $ "Nuclear Source"
  selectTechnologies

selectNuclearSource :: AttributeValue -> Html
selectNuclearSource idx = H.select ! class_ "custom-select" ! A.id idx $ do
  H.option ! value "Empty" $ "Empty"
  H.option ! value "IonCannon" $ "Ion Cannon"
  H.option ! value "ElectronComputer" $ "Electron Computer"
  H.option ! value "Hull" $ "Hull"
  H.option ! value "NuclearDrive" $ "Nuclear Drive"
  H.option ! value "NuclearSource" ! selected ""  $ "Nuclear Source"
  selectTechnologies


selectTechnologies = do  
  H.option ! value "PlasmaCannon" $ "Plasma Cannon"
  H.option ! value "AntimatterCannon" $ "Antimatter Cannon"
  H.option ! value "PlasmaMissile" $ "Plasma Missile"
  H.option ! value "PositronComputer" $ "Positron Computer"
  H.option ! value "GluonComputer" $ "Gluon Computer"
  H.option ! value "GaussShield" $ "Gauss Shield"
  H.option ! value "PhaseShield" $ "Phase Shield"
  H.option ! value "ImprovedHull" $ "Improved Hull"
  H.option ! value "FusionDrive" $ "Fusion Drive"
  H.option ! value "TachyonDrive" $ "Tachyon Drive"
  H.option ! value "FusionSource" $ "Fusion Source"
  H.option ! value "TachyonSource" $ "Tachyon Source"

