{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.Character
where

import GHC.Generics

import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Text (Text (..))
import qualified Data.Text as T

class TShow a where
    tshow :: a -> Text
    
data Eigenschaft
    = -- | Mut
      MU Int
    | -- | Klugheit
      KL Int
    | -- | Intuition
      IN Int
    | -- | Charisma
      CH Int
    | -- | Fingerfertigkeit
      FF Int
    | -- | Gewandtheit
      GE Int
    | -- | Konstitution
      KO Int
    | -- | Körperkraft
      KK Int
    | -- | Sozialstatus
      SO Int
    deriving (Eq, Ord, Show, Generic)

instance TShow Eigenschaft where
    tshow (MU i) = T.intercalate ": " ["Mut", T.pack . show $ i]
    tshow (KL i) = T.intercalate ": " ["Klugheit", T.pack . show $ i]
    tshow (IN i) = T.intercalate ": " ["Intuition", T.pack . show $ i]
    tshow (CH i) = T.intercalate ": " ["Charisma", T.pack . show $ i]
    tshow (FF i) = T.intercalate ": " ["Fingerfertigkeit", T.pack . show $ i]
    tshow (GE i) = T.intercalate ": " ["Gewandtheit", T.pack . show $ i]
    tshow (KO i) = T.intercalate ": " ["Konstitution", T.pack . show $ i]
    tshow (KK i) = T.intercalate ": " ["Körperkraft", T.pack . show $ i]
    tshow (SO i) = T.intercalate ": " ["Sozialstatus", T.pack . show $ i]

data TalentGruppe
    = -- | Kampf
      Kampf
    | -- | Körperlich
      Körperlich
    | -- | Gesellschaftlich
      Gesellschaftlich
    | -- | Natur
      Natur
    | -- | Wissen
      Wissen
    | -- | SpracheSchrift
      SpracheSchrift
    | -- | Handwerk
      Handwerk
    deriving (Eq, Ord, Show, Generic)
             
instance TShow TalentGruppe where
    tshow Kampf =  T.pack "Kampf"      
    tshow Körperlich =  T.pack "Körperlich"      
    tshow Gesellschaftlich =  T.pack "Gesellschaftlich"      
    tshow Natur =  T.pack "Natur"      
    tshow Wissen =  T.pack "Wissen"      
    tshow SpracheSchrift =  T.pack "SpracheSchrift"      
    tshow Handwerk =  T.pack "Handwerk"      

data Talent = Talent
    { _talentId :: Text
    , _talentValue :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    
instance TShow Talent where
    tshow t = T.intercalate ": " $ [ _talentId $ t
                                   , T.pack . show . _talentValue $ t
                                   ]

data Talente = Talente
    { _basisTalente :: Map TalentGruppe [Talent]
    , _spezialTalente :: Map TalentGruppe [Talent] 
    }
    deriving (Eq, Ord, Show, Generic)

instance TShow [Talent] where
    tshow = T.intercalate ", " . fmap tshow

instance TShow (Map TalentGruppe [Talent]) where
    tshow = T.intercalate "; " . M.foldrWithKey (\tg ts acc -> (T.intercalate " Talente: " [tshow tg, "(" <> tshow ts <> ")"]) : acc) []

instance TShow Talente where
    tshow ts = T.intercalate "; " [ tshow . _basisTalente $ ts
                                  , tshow . _spezialTalente $ ts
                                  ]

dsa41basisTalente :: Map TalentGruppe [Talent]
dsa41basisTalente =
    M.fromList $
     [ ( Kampf,
        [ Talent (T.pack "Dolche") 0,
          Talent (T.pack "Hiebwaffen") 0,
          Talent (T.pack "Raufen") 0,
          Talent (T.pack "Ringen") 0,
          Talent (T.pack "Säbel") 0,
          Talent (T.pack "Wurfmesser") 0
        ]
       )
     , ( Körperlich,
         [ Talent (T.pack "Athletik") 0
         , Talent (T.pack "Klettern") 0
         , Talent (T.pack "Körperbeherrschung") 0
         , Talent (T.pack "Schleichen") 0
         , Talent (T.pack "Schwimmen") 0
         , Talent (T.pack "Selbstbeherrschung") 0
         , Talent (T.pack "Sich Verstecken") 0
         , Talent (T.pack "Singen") 0
         , Talent (T.pack "Sinnenschärfe") 0
         , Talent (T.pack "Tanzen") 0
         , Talent (T.pack "Zechen") 0
         ]
       )
     , ( Gesellschaftlich,
         [ Talent (T.pack "Menschenkenntnis") 0,
           Talent (T.pack "Überreden") 0
         ]
       )
     , ( Natur,
         [ Talent (T.pack "Fährtensuchen") 0
         , Talent (T.pack "Orientierung") 0
         , Talent (T.pack "Wildnisleben") 0
         ]
       )
     , ( Wissen,
         [ Talent (T.pack "Götter/Kulte") 0
         , Talent (T.pack "Rechnen") 0
         , Talent (T.pack "Sagen/Legenden") 0
         ]
       )
     , ( SpracheSchrift,
         [ Talent (T.pack "Sprachen Kennen (Muttersprache)") 0 ]
       )
     , ( Handwerk,
         [ Talent (T.pack "Heilkunde Wunden") 0
         , Talent (T.pack "Holzbearbeitung") 0
         , Talent (T.pack "Kochen") 0
         , Talent (T.pack "Lederarbeiten") 0
         , Talent (T.pack "Malen/Zeichnen") 0
         , Talent (T.pack "Schneidern)") 0
         ]
       )
     ]
    
data HumanOrigin
    -- | Mittländer
    = StandardHuman
    | TulamideHuman
    | ThorwalHuman
    | NiveseHuman
    | TrollzackerHuman
    | NorbardeHuman
    | WaldmenschHuman
    | UtuluHuman
    deriving (Eq, Show)

data ElveOrigin
    = AueElve
    | WaldElve
    | SteppeElve
    | FirnElve
    deriving (Eq, Show)

data HalveElveOrigin
    = StandardHalveElve
    | FirnHalveElve
    | NivesichHalveElve
    | ThorwalischerHalveElve
    deriving (Eq, Show)
      
data DwarfOrigin
    = StandardDarf
    | BrilliantDarf
    | AmbossDarf
    | WilderDarf
    deriving (Eq, Show)

data OrcOrigin
    = StandardOrc
    | WomanOrc
    deriving (Eq, Show)

data GoblinOrigin
    = StandardGoblin
    | WomanGoblin
    deriving (Eq, Show)

data AchazOrigin
    = StandardAchaz
    | OrklandAchaz
    | WaldinselAchaz
    | MaraskanAchaz
    deriving (Eq, Show)
    
data Race
    = Human HumanOrigin
    | Elve ElveOrigin
    | Halbelf HalveElveOrigin
    | Zwerg DwarfOrigin
    | Orc OrcOrigin
    | Halborc 
    | Goblin GoblinOrigin
    | Achaz AchazOrigin
    deriving (Eq, Show)

data MidlandCulture
    = -- | Mittelländische Städte
      MidlandCities
    | -- | Mittelländische Landbevölkerung
      MidlandLandpeople
    | -- | Andergast und Nostria
      MidlandAndergastNostria
    | -- | Svellttal und Nordlande
      MidlandSvellttalNordland
    | -- | Almada
      MidlandAlmada
    | -- | Horasreich
      MidlandHoras
    | -- | Zyklopeninseln
      MidlandZyklpia
    | -- | Bornland
      MidlandBornland
    deriving (Eq, Show)

data TulamidCulture
    = -- | Aranien 
      TulamidAranier
    | -- | Mhanadistan 
      TulamidMhanadist
    | -- | Tulamidische Stadtstaaten 
      TulamidCitypeople
    | -- | Ferkin
      TulamidFerkin
    | -- | Zahori 
      TulamidZahori
    | -- | Novadi-Männer 
      TulamidNovadiMan
    | -- | Novadi-Fraue
      TulamidNovadiWoman
    deriving (Eq, Show)

data ThorwalCulture
    = -- | Fjarninger
      ThorwalFjarninger
    | -- | Gjalsker
      ThorwalGjalsker
    | -- | Thorwaler   
      ThorwalStandard
    deriving (Eq,Show)

-- data NuanaLieCulture
--     = -- | Nuanaä-Lie
--       NuanaLieCulture
--     deriving (Eq,Show)
      
-- data NivesianCulture
--     = -- | Nivesen
--       NivesianCulture
--     deriving (Eq, Show)
             
-- data NorbardCulture
--     = -- | Norbarden
--       NorbardCulture
--     deriving (Eq, Show)

-- data TrollzackCulture
--     = -- | Trollzacker
--       TrollzackCulture
--     deriving (Eq, Show)

data MixedCulture
    = -- | Amazonen 
      MixedAmazon
    | -- | Bukanier 
      MixedBukanier
    | -- | Maraskaner 
      MixedMaraskan
    | -- | Südaventurien             
      MixedSouthaventure
    deriving (Eq, Show)
                
data Culture
    = MidlandCulture MidlandCulture
    | TulamidCulture TulamidCulture
    | ThorwalCulture ThorwalCulture
    | NuanaLieCulture -- NuanaLieCulture
    | NivesianCulture -- NivesianCulture
    | NorbardCulture -- NorbardCulture
    | TrollzackCulture -- TrollzackCulture
    | MixedCulture MixedCulture
      deriving (Eq, Show)



-- data Profession
--     = -- | Sharisad, Voraussetzungen: Kultur Aranier, Mhanadistani 			
--       ProfessionSharisad
--     | -- | Hazaqi, Voraussetzungen: Kultur Zahori 			keine
--       ProfessionHazaqi
--     | -- | Ritter, Voraussetzungen: Hofritter, Raubritter, Heckenritter, Ritter alten Schlags
--       ProfessionKnight
--     | -- | Knappe, Voraussetzungen: Knappe des traditionellen Ritters 		
--       ProfessionSquire
--     | -- | Diener der Erdmutter, Voraussetzungen: Spezies Zwerg 	Gefährte des Feuers, Gefährte des Erzes, Gefährte des Humus, Gefährte der Luft, Gefährte des Wassers 		
--       ProfessionEarthServent
--     | -- | Herr der Erde, Voraussetzungen: Spezies Zwerg 	Luftherr, Eisherr, Erzherr, Feuerherr, Humusherr, Wasserherr 		
--       Profession
--     | -- | Brobim-Geode, Voraussetzungen: Spezies Zwerg, Kultur Wildzwerg 	Diener des Feuers, des Erzes, der Luft 		
--       Profession
--     | -- | Diener des Feuers, Voraussetzungen: Spezies Zwerg, Kultur Wildzwerg 			
--       Profession
--     | -- | Diener des Erzes, Voraussetzungen: Spezies Zwerg, Kultur Wildzwerg 			
--       Profession
--     | -- | Diener der Luft, Voraussetzungen: Spezies Zwerg, Kultur Wildzwerg 			
--       Profession
--     | -- | Gefährte des Feuers, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Gefährte des Erzes, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Gefährte des Humus, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Gefährte der Luft, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Gefährte des Wassers, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Luftherr, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Eisherr, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Erzherr, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Feuerherr, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Humusherr, Voraussetzungen: Spezies Zwerg 			
--       Profession
--     | -- | Wasserherr, Voraussetzungen: Spezies Zwerg 	
--       Profession
               

data Profession
    = Profession
      deriving (Eq, Show)

-- | StandardCharacter Properties for character generation
data StandardCharacter
    = StandardCharacter {
        _StandardCharacterRace :: Race
      , _StandardCharacterCulture :: Culture
      , _StandardCharacterProfession :: Profession
      }
    deriving (Eq, Show)

-- data Propertie
--     = Mut 
--     | Klugheit
-- type Propertie = String
 
-- type WeightedPropertie
--     = (Propertie, Int)

-- data Talent
--     = Abrichten (Propertie, Propertie, Propertie) Int
--     | Zweihaender Int
--     deriving (Show, Eq)

-- -- data Pro_Con
-- --     = Aberglaube
-- --     deriving (Show, Eq)
-- type Pro_Con = String

-- data Character
--     = C
--       { characterName :: String
--       , characterSize :: Float
--       , characterWeight :: Float
--       , characterEeycolor :: String
--       , characterLifepoints :: Int
--       , characterStamina :: Int
--       , characterGP :: Int
--       , characterMaxGP :: Int
--       , characterAuP :: Int
--       , characterProperties :: [WeightedPropertie]
--       , characterTalens :: [Talent]
--       , characterPro_Cons :: [Pro_Con]
--       }
--     deriving (Show, Eq)


-- type Wert = Char
-- type Aktivierg = Int

-- talentFn :: Wert -> Aktivierg -> Int
-- talentFn 'P' = (!!)  [ 1,  1,  1,  1,  2,   4,  5,    6,   8,   9,  11,  12,  14,  15,  17,  19,  20,  22,  24,  25,  27]
-- talentFn 'A' = (!!)  [ 1,  1,  2,  3,  4,   6,  7,    8,  10,  11,  13,  14,  16,  17,  19,  21,  22,  24,  26,  27,  29]
-- talentFn 'B' = (!!)  [ 2,  2,  4,  6,  8,  11,  14,  17,  19,  22,  25,  28,  32,  35,  38,  41,  45,  48,  51,  55,  58]
-- talentFn 'C' = (!!)  [ 3,  2,  6,  9, 13,  17,  21,  25,  29,  34,  38,  43,  47,  51,  55,  60,  65,  70,  75,  80,  85]
-- talentFn 'D' = (!!)  [ 4,  3,  7, 12, 17,  22,  27,  33,  39,  45,  50,  55,  65,  70,  75,  85,  90,  95, 105, 110, 115]
-- talentFn 'E' = (!!)  [ 5,  4,  9, 15, 21,  28,  34,  41,  48,  55,  65,  70,  80,  85,  95, 105, 110, 120, 130, 135, 145]
-- talentFn 'F' = (!!)  [ 8,  6, 14, 22, 32,  41,  50,  60,  75,  85,  95, 105, 120, 130, 140, 155, 165, 180, 195, 210, 220]
-- talentFn 'G' = (!!)  [10,  8, 18, 30, 42,  55,  70,  85,  95, 110, 125, 140, 160, 175, 190, 210, 220, 240, 260, 270, 290] 
-- talentFn 'H' = (!!)  [20, 16, 35, 60, 85, 110, 140, 165, 195, 220, 250, 280, 320, 350, 380, 410, 450, 480, 510, 550, 580]


-- type AddPro = Character -> Character

-- type Vorteilsname = String
-- type Vorteil = (Vorteilsname, AddPro)
-- mkStandarddVorteil :: Vorteilsname -> Int -> Vorteil
-- mkStandardVorteil s i
--     = (s, d\c -> c { characterGP = characterGP c - i
--                   , characterPro_Cons = s:(characterPro_Cons c) })

-- mkConditionVorteil s i p
--     = (s, \c -> if (p c)
--                 then c { characterGP = characterGP c - i
--                        , characterPro_Cons = s:(characterPro_Cons c) }
--                 else c
--       )

-- mkPerVorteil s i x
--     = (s, \c -> snd . mkStandardVorteil s $ (i * (x c)) )

-- vorteile :: M.Map Vorteilsname AddPro
-- vorteile = fromList
--            [ mkStandardVorteil "Adlig, adlige Abstammung" 7
--            , mkStandardVorteil "Adlig, Amtsadel" 5
--            , mkConditionVorteil "Adlig, adliges Erbe" 6 (\c -> "Adlig, adlige Abstammung" `elem` characterPro_Cons c)
--            , mkStandardVorteil "Astrale Regeneration I" 4
--            , mkConditionVorteil "Astrale Regeneration II" 8 (\c -> "Astrale Regeneration I" `elem` characterPro_Cons c)
--            , mkConditionVorteil "Astrale Regeneration III" 12 (\c -> "Astrale Regeneration II" `elem` characterPro_Cons c)
--            -- , mkPerVorteil "Ausdauernd" 1 (\c -> characterAuP c)
--            ]


-- heinz = C "Heinz" 1.80 80.0 "Brown" 110 100 110 110 0 [("Konstitution", 10)] [] []
