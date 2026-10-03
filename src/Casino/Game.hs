module Casino.Game
  ( Symbol (..), symbols, symbolName, symbolWeight
  , Reels (..), reelSymbols, replaceReel, payout
  , Machine (..), bonusLimit, toggleMachine
  , Round (..), Game (..), Action (..)
  , newGame, applyAction
  ) where

import System.Random (StdGen, randomR)

data Symbol = Dash | Coin | CoinStack | Clover | Double | Snake | Net | Crown
  deriving (Eq, Ord, Show, Enum, Bounded)

symbols :: [Symbol]
symbols = [minBound .. maxBound]

symbolName :: Symbol -> String
symbolName Dash = "Dash"
symbolName Coin = "Coin"
symbolName CoinStack = "Stack"
symbolName Clover = "Clover"
symbolName Double = "Double"
symbolName Snake = "Snake"
symbolName Net = "Net"
symbolName Crown = "Crown"

symbolWeight :: Symbol -> Int
symbolWeight Dash = 27
symbolWeight Coin = 30
symbolWeight CoinStack = 10
symbolWeight Clover = 1
symbolWeight Double = 10
symbolWeight Snake = 10
symbolWeight Net = 4
symbolWeight Crown = 8

-- A round always contains exactly four reels.
data Reels = Reels Symbol Symbol Symbol Symbol deriving (Eq, Show)

reelSymbols :: Reels -> [Symbol]
reelSymbols (Reels first second third fourth) = [first, second, third, fourth]

replaceReel :: Int -> Symbol -> Reels -> Reels
replaceReel reelIndex symbol reels@(Reels first second third fourth) = case reelIndex of
  0 -> Reels symbol second third fourth
  1 -> Reels first symbol third fourth
  2 -> Reels first second symbol fourth
  3 -> Reels first second third symbol
  _ -> reels

payout :: Reels -> Integer
payout reels
  | count Snake > 0 && count Net == 0 = 0
  | otherwise = (matches + 10 * count Clover + snakeReward) * 2 ^ count Double
  where
    count symbol = toInteger . length . filter (== symbol) $ reelSymbols reels
    match symbol triple quadruple = case count symbol of
      3 -> triple
      4 -> quadruple
      _ -> 0
    matches = match Coin 3 5 + match CoinStack 9 15 + match Crown 0 100
    snakeReward = if count Net > 0 then 3 * count Snake else 0

data Machine = Regular | Golden deriving (Eq, Show)

bonusLimit :: Machine -> Int
bonusLimit Regular = 3
bonusLimit Golden = 5

toggleMachine :: Machine -> Machine
toggleMachine Regular = Golden
toggleMachine Golden = Regular

data Round = Round
  { roundReels :: Reels
  , bonusRemaining :: Int
  , roundCost :: Integer
  } deriving (Eq, Show)

data Game = Game
  { bankroll :: Integer
  , startingMoney :: Integer
  , selectedMachine :: Machine
  , activeRound :: Maybe Round
  , completedRounds :: Integer
  , totalSpent :: Integer
  , totalWon :: Integer
  , lastPayout :: Maybe Integer
  , statusMessage :: String
  , randomGenerator :: StdGen
  } deriving (Show)

data Action = Spin | Reroll Int | Collect deriving (Eq, Show)

newGame :: Integer -> Machine -> StdGen -> Game
newGame money machine generator = Game
  { bankroll = max 0 money, startingMoney = max 0 money
  , selectedMachine = machine, activeRound = Nothing
  , completedRounds = 0, totalSpent = 0, totalWon = 0
  , lastPayout = Nothing, statusMessage = "Ready. Spin costs 1 coin."
  , randomGenerator = generator
  }

drawSymbol :: StdGen -> (StdGen, Symbol)
drawSymbol generator = (nextGenerator, choose ticket symbols)
  where
    (ticket, nextGenerator) = randomR (1, 100 :: Int) generator
    choose _ [] = Dash -- Unreachable: the weights sum to 100.
    choose remaining (symbol : rest)
      | remaining <= symbolWeight symbol = symbol
      | otherwise = choose (remaining - symbolWeight symbol) rest

drawReels :: StdGen -> (StdGen, Reels)
drawReels generator = (fourthGenerator, Reels first second third fourth)
  where
    (firstGenerator, first) = drawSymbol generator
    (secondGenerator, second) = drawSymbol firstGenerator
    (thirdGenerator, third) = drawSymbol secondGenerator
    (fourthGenerator, fourth) = drawSymbol thirdGenerator

-- All state changes, including RNG advancement, are pure and reproducible.
applyAction :: Action -> Game -> Game
applyAction Spin game
  | Just _ <- activeRound game = game { statusMessage = "Collect this round before spinning again." }
  | bankroll game < 1 = game { statusMessage = "No coins left. Press r to restart with the same settings." }
  | otherwise = game
      { bankroll = bankroll game - 1
      , totalSpent = totalSpent game + 1
      , activeRound = Just (Round reels (bonusLimit $ selectedMachine game) 1)
      , randomGenerator = nextGenerator
      , statusMessage = "Choose a reel to reroll, or collect the payout."
      }
  where
    (nextGenerator, reels) = drawReels (randomGenerator game)
applyAction (Reroll reelIndex) game = case activeRound game of
  Nothing -> game { statusMessage = "Spin first." }
  Just currentRound
    | reelIndex < 0 || reelIndex > 3 -> game
    | bonusRemaining currentRound == 0 -> game { statusMessage = "No bonus spins remaining. Collect your payout." }
    | bankroll game < 1 -> game { statusMessage = "A bonus spin costs 1 coin. Collect your payout." }
    | otherwise -> game
        { bankroll = bankroll game - 1
        , totalSpent = totalSpent game + 1
        , activeRound = Just currentRound
            { roundReels = replaceReel reelIndex symbol (roundReels currentRound)
            , bonusRemaining = bonusRemaining currentRound - 1
            , roundCost = roundCost currentRound + 1
            }
        , randomGenerator = nextGenerator
        , statusMessage = "Bonus spin used."
        }
  where
    (nextGenerator, symbol) = drawSymbol (randomGenerator game)
applyAction Collect game = case activeRound game of
  Nothing -> game { statusMessage = "Spin first." }
  Just currentRound -> game
    { bankroll = bankroll game + reward
    , totalWon = totalWon game + reward
    , completedRounds = completedRounds game + 1
    , activeRound = Nothing
    , lastPayout = Just reward
    , statusMessage = "Collected " ++ show reward ++ " coins (round net " ++ show (reward - roundCost currentRound) ++ ")."
    }
    where reward = payout (roundReels currentRound)
