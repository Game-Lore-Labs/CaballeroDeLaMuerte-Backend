{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- RPG-Bot: Sistema de Libro-Juego con API REST en Haskell
-- Servidor: scotty en puerto 3000
-- Compilar: stack build
-- Ejecutar: stack run

module Main where

import Web.Scotty hiding (options)
import Data.Aeson (ToJSON, FromJSON, object, (.=))
import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status

-- ============================================================================
-- TIPOS DE DATOS FUNDAMENTALES
-- ============================================================================

data Character = Character
  { charName :: String
  , charClass :: CharClass
  , charHP :: Int
  , charMaxHP :: Int
  , charInventory :: [Item]
  , charClues :: [Clue]
  , charRelations :: Map String Relation
  , charXP :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Character
instance FromJSON Character

data CharClass = Fighter | Wizard | Rogue | Cleric | Paladin
  deriving (Show, Eq, Generic)

instance ToJSON CharClass
instance FromJSON CharClass

data Item = Item
  { itemName :: String
  , itemValue :: Int
  , itemType :: ItemType
  } deriving (Show, Eq, Generic)

instance ToJSON Item
instance FromJSON Item

data ItemType = Weapon | Armor | Potion | Quest | Misc
  deriving (Show, Eq, Generic)

instance ToJSON ItemType
instance FromJSON ItemType

data Clue = Clue String deriving (Show, Eq, Generic)

instance ToJSON Clue
instance FromJSON Clue

data Relation = Friendly | Neutral | Hostile deriving (Show, Eq, Generic)

instance ToJSON Relation
instance FromJSON Relation

type EntryId = String

data Entry = Entry
  { entryId :: EntryId
  , narrative :: String
  , entryOptions :: [Option]
  , effects :: [Effect]
  } deriving (Show, Generic)

instance ToJSON Entry
instance FromJSON Entry

data Option = Option
  { optionText :: String
  , optionTarget :: EntryId
  , optionCondition :: Condition
  } deriving (Show, Generic)

instance ToJSON Option
instance FromJSON Option

data Condition
  = Always
  | HasItem String
  | HasClass CharClass
  | HasClue String
  | MinHP Int
  | RollCheck DiceCheck
  | And Condition Condition
  | Or Condition Condition
  | Not Condition
  deriving (Show, Eq, Generic)

instance ToJSON Condition
instance FromJSON Condition

data DiceCheck = DiceCheck
  { checkType :: CheckType
  , checkDC :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON DiceCheck
instance FromJSON DiceCheck

data CheckType = Perception | Athletics | Stealth | Investigation
  deriving (Show, Eq, Generic)

instance ToJSON CheckType
instance FromJSON CheckType

data Effect
  = Damage Int
  | Heal Int
  | GainItem Item
  | LoseItem String
  | GainClue Clue
  | GainXP Int
  | ChangeRelation String Relation
  deriving (Show, Generic)

instance ToJSON Effect
instance FromJSON Effect

data GameState = GameState
  { currentEntry :: EntryId
  , character :: Character
  , visitedEntries :: [EntryId]
  , gameLog :: [String]
  } deriving (Show, Generic)

instance ToJSON GameState
instance FromJSON GameState

-- ============================================================================
-- API REQUEST/RESPONSE TYPES
-- ============================================================================

data CreateCharacterRequest = CreateCharacterRequest
  { reqName :: String
  , reqClass :: CharClass
  } deriving (Generic)

instance FromJSON CreateCharacterRequest

data GameResponse = GameResponse
  { gameState :: GameState
  , currentNarrative :: String
  , availableOptions :: [OptionResponse]
  , isGameOver :: Bool
  } deriving (Generic)

instance ToJSON GameResponse

data OptionResponse = OptionResponse
  { optionIndex :: Int
  , optionText' :: String
  , optionTarget' :: EntryId
  } deriving (Generic)

instance ToJSON OptionResponse

data ChoiceRequest = ChoiceRequest
  { choiceIndex :: Int
  , state :: GameState
  } deriving (Generic)

instance FromJSON ChoiceRequest

data ErrorResponse = ErrorResponse
  { errorMessage :: String
  } deriving (Generic)

instance ToJSON ErrorResponse

data DiceRollRequest = DiceRollRequest
  { diceCount :: Int
  , diceSides :: Int
  } deriving (Generic)

instance FromJSON DiceRollRequest

data DiceRollResponse = DiceRollResponse
  { rolls :: [Int]
  , total :: Int
  } deriving (Generic)

instance ToJSON DiceRollResponse

-- ============================================================================
-- MOTOR DEL JUEGO
-- ============================================================================

evaluateCondition :: Condition -> Character -> IO Bool
evaluateCondition Always _ = return True
evaluateCondition (HasItem name) char = 
  return $ any (\i -> itemName i == name) (charInventory char)
evaluateCondition (HasClass cls) char = 
  return $ charClass char == cls
evaluateCondition (HasClue clue) char = 
  return $ any (\(Clue c) -> c == clue) (charClues char)
evaluateCondition (MinHP hp) char = 
  return $ charHP char >= hp
evaluateCondition (RollCheck check) _ = rollDiceCheck check
evaluateCondition (And c1 c2) char = do
  r1 <- evaluateCondition c1 char
  r2 <- evaluateCondition c2 char
  return (r1 && r2)
evaluateCondition (Or c1 c2) char = do
  r1 <- evaluateCondition c1 char
  r2 <- evaluateCondition c2 char
  return (r1 || r2)
evaluateCondition (Not c) char = not <$> evaluateCondition c char

rollDiceCheck :: DiceCheck -> IO Bool
rollDiceCheck (DiceCheck _ dc) = do
  roll <- randomRIO (1, 20)
  return $ roll >= dc

rollDice :: Int -> Int -> IO Int
rollDice n sides = sum <$> sequence (replicate n (randomRIO (1, sides)))

rollDiceDetailed :: Int -> Int -> IO [Int]
rollDiceDetailed n sides = sequence (replicate n (randomRIO (1, sides)))

applyEffect :: Effect -> Character -> IO Character
applyEffect (Damage dmg) char = return $ char { charHP = max 0 (charHP char - dmg) }
applyEffect (Heal hp) char = return $ char { charHP = min (charMaxHP char) (charHP char + hp) }
applyEffect (GainItem item) char = return $ char { charInventory = item : charInventory char }
applyEffect (LoseItem name) char = 
  return $ char { charInventory = filter (\i -> itemName i /= name) (charInventory char) }
applyEffect (GainClue clue) char = return $ char { charClues = clue : charClues char }
applyEffect (GainXP xp) char = return $ char { charXP = charXP char + xp }
applyEffect (ChangeRelation npc rel) char = 
  return $ char { charRelations = M.insert npc rel (charRelations char) }

applyEffects :: [Effect] -> Character -> IO Character
applyEffects effects char = foldl (>>=) (return char) (map applyEffect effects)

getAvailableOptions :: Entry -> Character -> IO [Option]
getAvailableOptions entry char = do
  results <- mapM (\opt -> do
    valid <- evaluateCondition (optionCondition opt) char
    return (opt, valid)) (entryOptions entry)
  return [opt | (opt, True) <- results]

-- ============================================================================
-- BASE DE DATOS DE ENTRADAS
-- ============================================================================

gameEntries :: Map EntryId Entry
gameEntries = M.fromList
  [ ("START", entryStart)
  , ("ADVENTUREBEGINS", entryAdventureBegins)
  , ("BREWSKI", entryBrewski)
  , ("KEEPGOING", entryKeepGoing)
  , ("QUESTCONVO", entryQuestConvo)
  , ("REPLENISH", entryReplenish)
  , ("FORESTENTRY", entryForestEntry)
  , ("CHALLENGE", entryChallenge)
  , ("HIDEAWAY", entryHideAway)
  , ("DEATH", entryDeath)
  , ("VICTORY", entryVictory)
  ]

entryStart :: Entry
entryStart = Entry
  { entryId = "START"
  , narrative = "Bienvenido a 'El Escudero del Caballero de la Muerte'. Eres un aventurero en la ciudad de Orlbar. Lord y Lady Brewmont te han contratado para rescatar a su nieto Darek, secuestrado por el misterioso Caballero de la Muerte."
  , entryOptions = 
      [ Option "Comenzar la aventura" "ADVENTUREBEGINS" Always ]
  , effects = []
  }

entryAdventureBegins :: Entry
entryAdventureBegins = Entry
  { entryId = "ADVENTUREBEGINS"
  , narrative = "Al amanecer del día siguiente, ensillas tu caballo y cabalgas hacia las afueras de la ciudad. El viaje al Bosque Weathercote está al este. Al mediodía llegas a un letrero: quedan 15 millas por recorrer. No muy lejos hay una pequeña posada. Un anciano sentado al sol te llama: '¡Última bebida por muchas millas! ¡Ven, siéntate! ¡Te invito a una cerveza!'"
  , entryOptions =
      [ Option "Aceptar la invitación del anciano" "BREWSKI" Always
      , Option "Seguir cabalgando sin detenerse" "KEEPGOING" Always
      ]
  , effects = []
  }

entryBrewski :: Entry
entryBrewski = Entry
  { entryId = "BREWSKI"
  , narrative = "Atas tu caballo y te unes al anciano. Un niño te trae una jarra de cerveza espumosa y un cuenco de estofado. Después de un rato, el anciano pregunta: '¿Qué te trae por aquí?'"
  , entryOptions =
      [ Option "Contarle sobre tu misión" "QUESTCONVO" Always
      , Option "Solo reponerte y seguir tu camino" "REPLENISH" Always
      ]
  , effects = []
  }

entryQuestConvo :: Entry
entryQuestConvo = Entry
  { entryId = "QUESTCONVO"
  , narrative = "Le cuentas al anciano sobre el Caballero de la Muerte y tu misión de rescate. Su rostro se ensombrece. 'Ah, el Caballero... Muchos han entrado al Bosque Weathercote buscándolo. Pocos han regresado. Ten cuidado, viajero. Se dice que no es de este mundo.' Te da una pequeña piedra tallada. 'Esto me lo dio un druida hace años. Quizás te proteja.'"
  , entryOptions =
      [ Option "Agradecer y continuar tu viaje" "FORESTENTRY" Always
      ]
  , effects = 
      [ GainItem (Item "Piedra Protectora" 0 Quest)
      , GainClue (Clue "El Caballero de la Muerte no es de este mundo")
      ]
  }

entryReplenish :: Entry
entryReplenish = Entry
  { entryId = "REPLENISH"
  , narrative = "Te limitas a asentir cortésmente mientras terminas tu cerveza y estofado. Reabastecido, vuelves a montar tu caballo y continúas hacia el este."
  , entryOptions =
      [ Option "Continuar hacia el Bosque Weathercote" "FORESTENTRY" Always
      ]
  , effects = [ Heal 5 ]
  }

entryKeepGoing :: Entry
entryKeepGoing = Entry
  { entryId = "KEEPGOING"
  , narrative = "Decides no perder tiempo y continúas cabalgando. El sol comienza a descender en el horizonte mientras te acercas al oscuro borde del Bosque Weathercote."
  , entryOptions =
      [ Option "Entrar al bosque" "FORESTENTRY" Always
      ]
  , effects = []
  }

entryForestEntry :: Entry
entryForestEntry = Entry
  { entryId = "FORESTENTRY"
  , narrative = "Al anochecer, llegas al Bosque Weathercote. La luz apenas penetra el denso dosel. El camino se adentra en la oscuridad. De repente, oyes el sonido de cascos galopando. ¡Una figura montada aparece en el camino! Un caballero con armadura negra lleva a un niño sobre su caballo. El niño grita pidiendo ayuda."
  , entryOptions =
      [ Option "¡Desafiar al jinete y detenerlo!" "CHALLENGE" Always
      , Option "Esconderte y observar (Requiere: Rogue)" "HIDEAWAY" (HasClass Rogue)
      , Option "Esconderte y observar (Tirada de Sigilo CD 12)" "HIDEAWAY" (RollCheck (DiceCheck Stealth 12))
      ]
  , effects = []
  }

entryChallenge :: Entry
entryChallenge = Entry
  { entryId = "CHALLENGE"
  , narrative = "Blandes tu arma valientemente: '¡Alto, bandido! ¡Suelta a ese chico!' El jinete detiene su caballo y levanta su visera. Dentro no hay nada... solo un vacío negro y oscuro. Tu corazón se detiene. Entonces el jinete echa la cabeza hacia atrás y una risa horrible llena el bosque, haciendo eco en cada árbol. Cierras los ojos, tapándote los oídos. Cuando miras de nuevo... la figura ha desaparecido."
  , entryOptions =
      [ Option "Recuperar la compostura y continuar la búsqueda" "VICTORY" (MinHP 1)
      , Option "Tus heridas son graves..." "DEATH" (Not (MinHP 1))
      ]
  , effects = 
      [ GainClue (Clue "El Caballero de la Muerte es una entidad sobrenatural")
      , Damage 5
      ]
  }

entryHideAway :: Entry
entryHideAway = Entry
  { entryId = "HIDEAWAY"
  , narrative = "Te escondes rápidamente entre los arbustos. El jinete pasa cerca, y puedes ver mejor: la armadura está oxidada y deteriorada. El niño está atado y amordazado. El jinete se dirige hacia el norte del bosque. Cuando desaparecen de vista, sales de tu escondite con información valiosa."
  , entryOptions =
      [ Option "Seguir su rastro hacia el norte" "VICTORY" Always
      ]
  , effects =
      [ GainClue (Clue "El Caballero fue hacia el norte del bosque")
      , GainXP 50
      ]
  }

entryVictory :: Entry
entryVictory = Entry
  { entryId = "VICTORY"
  , narrative = "Has completado esta parte de la aventura con éxito. Ahora conoces la dirección del Caballero de la Muerte y tienes pistas valiosas. La aventura continúa... (Más contenido estará disponible próximamente)"
  , entryOptions = []
  , effects = [ GainXP 100 ]
  }

entryDeath :: Entry
entryDeath = Entry
  { entryId = "DEATH"
  , narrative = "Has caído en combate. Tu aventura termina aquí... pero las historias de héroes nunca mueren del todo. ¿Quizás otro aventurero tenga más suerte?"
  , entryOptions = []
  , effects = []
  }

-- ============================================================================
-- FUNCIONES DE JUEGO
-- ============================================================================

createCharacter :: String -> CharClass -> Character
createCharacter name cls = Character
  { charName = name
  , charClass = cls
  , charHP = 20
  , charMaxHP = 20
  , charInventory = 
      [ Item "Espada Larga" 15 Weapon
      , Item "Armadura de Cuero" 10 Armor
      , Item "Poción de Curación" 50 Potion
      ]
  , charClues = []
  , charRelations = M.empty
  , charXP = 0
  }

initialGameState :: Character -> GameState
initialGameState char = GameState
  { currentEntry = "START"
  , character = char
  , visitedEntries = []
  , gameLog = []
  }

getCurrentEntry :: GameState -> Maybe Entry
getCurrentEntry gs = M.lookup (currentEntry gs) gameEntries

processChoice :: Int -> GameState -> IO (Either String GameState)
processChoice choiceIdx gs = do
  case getCurrentEntry gs of
    Nothing -> return $ Left "Entrada no encontrada"
    Just entry -> do
      availableOpts <- getAvailableOptions entry (character gs)
      if choiceIdx < 0 || choiceIdx >= length availableOpts
        then return $ Left "Índice de opción inválido"
        else do
          let chosen = availableOpts !! choiceIdx
          let newEntryId = optionTarget chosen
          
          newChar <- applyEffects (effects entry) (character gs)
          
          let newGs = gs
                { currentEntry = newEntryId
                , character = newChar
                , visitedEntries = entryId entry : visitedEntries gs
                , gameLog = optionText chosen : gameLog gs
                }
          
          return $ Right newGs

buildGameResponse :: GameState -> IO GameResponse
buildGameResponse gs = do
  case getCurrentEntry gs of
    Nothing -> return $ GameResponse gs "Error: entrada no encontrada" [] True
    Just entry -> do
      availableOpts <- getAvailableOptions entry (character gs)
      let optResponses = zipWith (\idx opt -> OptionResponse idx (optionText opt) (optionTarget opt))
                                  [0..] availableOpts
      let isOver = null availableOpts || charHP (character gs) <= 0
      return $ GameResponse gs (narrative entry) optResponses isOver

-- ============================================================================
-- API REST ENDPOINTS
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=========================================="
  putStrLn "  RPG-BOT API Server"
  putStrLn "=========================================="
  putStrLn "Servidor iniciado en: http://localhost:3000"
  putStrLn ""
  putStrLn "Endpoints disponibles:"
  putStrLn "  POST /api/character     - Crear personaje"
  putStrLn "  POST /api/choice        - Procesar elección"
  putStrLn "  POST /api/dice          - Tirar dados"
  putStrLn "  GET  /api/entries       - Listar entradas"
  putStrLn "  GET  /api/entry/:id     - Obtener entrada"
  putStrLn "  GET  /api/health        - Health check"
  putStrLn "=========================================="
  
  scotty 3000 $ do
    -- Middleware CORS
    middleware $ cors $ const $ Just $ simpleCorsResourcePolicy
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      }

    -- GET /api/health - Health check
    get "/api/health" $ do
      json $ object
        [ "status" .= ("OK" :: String)
        , "service" .= ("RPG-Bot API" :: String)
        , "version" .= ("1.0.0" :: String)
        ]

    -- POST /api/character - Crear nuevo personaje y empezar juego
    post "/api/character" $ do
      reqBody <- jsonData :: ActionM CreateCharacterRequest
      let char = createCharacter (reqName reqBody) (reqClass reqBody)
      let gs = initialGameState char
      response <- liftIO $ buildGameResponse gs
      json response

    -- POST /api/choice - Procesar elección del jugador
    post "/api/choice" $ do
      reqBody <- jsonData :: ActionM ChoiceRequest
      result <- liftIO $ processChoice (choiceIndex reqBody) (state reqBody)
      case result of
        Left err -> do
          status status400
          json $ ErrorResponse err
        Right newGs -> do
          response <- liftIO $ buildGameResponse newGs
          json response

    -- POST /api/dice - Tirar dados
    post "/api/dice" $ do
      reqBody <- jsonData :: ActionM DiceRollRequest
      rolls <- liftIO $ rollDiceDetailed (diceCount reqBody) (diceSides reqBody)
      json $ DiceRollResponse rolls (sum rolls)

    -- GET /api/entries - Listar todas las entradas
    get "/api/entries" $ do
      json $ M.keys gameEntries

    -- GET /api/entry/:id - Obtener una entrada específica
    get "/api/entry/:id" $ do
      entryIdParam <- param "id"
      case M.lookup entryIdParam gameEntries of
        Nothing -> do
          status status404
          json $ ErrorResponse "Entrada no encontrada"
        Just entry -> json entry

    -- GET /api/classes - Obtener clases disponibles
    get "/api/classes" $ do
      json $ object
        [ "classes" .= (["Fighter", "Wizard", "Rogue", "Cleric", "Paladin"] :: [String])
        ]