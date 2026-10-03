module Casino.Strategy (Recommendation (..), recommend, autoplayAction) where

import Casino.Game
import Data.List (findIndex, maximumBy, sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Numeric (showFFloat)

data Recommendation = Recommendation
  { recommendedAction :: Action,
    recommendationReason :: String
  }
  deriving (Eq, Show)

-- The wiki guide resolves ties and supplies human-readable explanations. Its
-- abbreviated rules are checked against the exact expected value of the legal
-- actions, so small edge cases cannot accidentally turn a profitable roll into
-- an inferior one. Tables are pure, shared, and contain only 330 states per depth.
recommend :: Round -> Recommendation
recommend currentRound
  | bonusRemaining currentRound <= 0 = guide
  | guideValue >= bestValue - 1e-9 = guide
  | otherwise =
      Recommendation
        bestAction
        ( "Expected-value refinement of the wiki guide: "
            ++ showFFloat (Just 3) bestValue ""
            ++ " coins after future bonus costs; collecting gives "
            ++ show (payout reels)
            ++ "."
        )
  where
    guide = wikiRecommend currentRound
    reels = roundReels currentRound
    depth = min 5 (bonusRemaining currentRound)
    values = actionValues (valueTables !! (depth - 1)) reels
    (bestAction, bestValue) = maximumBy (comparing snd) (reverse values)
    guideValue = fromMaybe 0 (lookup (recommendedAction guide) values)

type ValueTable = Map.Map [Symbol] Double

valueTables :: [ValueTable]
valueTables = scanl nextTable initialTable [1 .. 5 :: Int]
  where
    boards =
      [ Reels first second third fourth
      | first <- symbols,
        second <- symbols,
        third <- symbols,
        fourth <- symbols,
        first <= second,
        second <= third,
        third <= fourth
      ]
    initialTable = Map.fromList [(sort $ reelSymbols reels, fromIntegral $ payout reels) | reels <- boards]
    nextTable previous _ =
      Map.fromList
        [(sort $ reelSymbols reels, maximum $ map snd $ actionValues previous reels) | reels <- boards]

actionValues :: ValueTable -> Reels -> [(Action, Double)]
actionValues previous reels =
  (Collect, fromIntegral $ payout reels)
    : [ ( Reroll index,
          -1
            + sum
              [ fromIntegral (symbolWeight symbol)
                  / 100
                  * Map.findWithDefault 0 (sort $ reelSymbols $ replaceReel index symbol reels) previous
              | symbol <- symbols
              ]
        )
      | index <- [0 .. 3]
      ]

-- Ordered rules from https://blueprince.wiki.gg/wiki/Casino#Slot_Machine.
-- A matching rule that says to collect terminates the search, just as the wiki
-- specifies: later rules must never override an earlier matching rule.
wikiRecommend :: Round -> Recommendation
wikiRecommend currentRound
  | remaining <= 0 = cash "No bonus spins remaining."
  | allContribute && not helperNetException = cash "Every reel contributes to the payout; collect."
  | count Crown == 3 =
      decide
        (remaining > 1 || not (has Clover))
        (except [Crown])
        "Three crowns: chase the fourth; preserve a clover on the last spin."
  | clovers >= 1 && helpers >= 3 =
      decide
        (remaining > 1 || has Snake)
        (except [Clover, Double])
        "Clover with two helpers: improve the remaining reel."
  | clovers >= 1 && helpers >= 2 =
      decide
        helperCondition
        (priority [Snake, Dash, Coin, CoinStack, Crown, Net])
        "Clover with a helper: remove the weakest symbol."
  | clovers >= 1 && (count Coin == 2 || count CoinStack == 2) =
      if has Net && remaining == 1
        then reroll (priority [Coin, CoinStack]) "Clover and a pair with a net: reroll one of the pair."
        else
          decide
            (remaining > 1 || has Snake)
            (except [Clover, pairSymbol])
            "Clover and a pair: improve the remaining reel."
  | clovers >= 1 && has Net =
      reroll
        (fst <$> listToMaybe (filter ((/= Snake) . snd) $ unpreserved [Clover, Net]))
        "Keep clover and net; reroll a remaining non-snake."
  | clovers >= 1 && has Snake =
      if keepSnake
        then reroll (spare [Clover, Snake, keptSymbol]) "Keep clover, snake and the best plain symbol; improve the other reel."
        else decide (remaining >= count Snake) (priority [Snake]) "Remove snakes only when enough spins remain to remove them all."
  | clovers >= 1 =
      decide
        (remaining > 1)
        cloverTarget
        "Clover: improve a spare reel, but preserve the payout on the last spin."
  | count Crown == 2 =
      if has Snake && has Net
        then cash "Two crowns with snake and net: collect."
        else
          if (count Coin == 2 || count CoinStack == 2) && remaining >= 2 && remaining <= 3
            then reroll (priority [Crown]) "Two crowns and a pair: reroll a crown with two or three spins left."
            else
              decide
                (remaining >= 3)
                (priority [Dash, CoinStack, Coin, Snake, Double, Net])
                "Two crowns: chase more crowns with at least three spins left."
  | (count Coin == 3 || count CoinStack == 3) && has Snake =
      reroll (priority [Snake]) "Three matching coins or stacks: remove the snake."
  | count CoinStack == 3 =
      decide
        (remaining > 1)
        (except [CoinStack])
        "Three stacks: improve the remaining reel, except on the last spin."
  | count Coin == 2 || count CoinStack == 2 =
      decide
        (not (has Snake && has Net) && (remaining > 1 || has Double || has Net))
        pairTarget
        "A pair: improve another reel; on the last spin require a double or net."
  | has Net && has Double =
      decide
        netDoubleCondition
        netDoubleTarget
        "Net and double: seek snakes or improve a spare reel."
  | has Coin && has Double =
      decide
        (remaining >= 5 || (remaining >= 4 && (has Snake || count Double >= 2 || has CoinStack)))
        (priority [Dash, Crown, Snake, Double, CoinStack])
        "Coin and double: pursue a match with enough spins left."
  | has Snake && count Double >= 2 =
      decide
        (remaining >= 5)
        (spare [Snake, Double, Double])
        "Snake and two doubles: improve the remaining reel with five spins left."
  | otherwise = cash "No wiki reroll rule applies."
  where
    reels = reelSymbols (roundReels currentRound)
    remaining = bonusRemaining currentRound
    count symbol = length (filter (== symbol) reels)
    has symbol = count symbol > 0
    clovers = count Clover
    helpers = clovers + count Double
    allContribute =
      all
        ( \index ->
            payout (replaceReel index Dash $ roundReels currentRound)
              < payout (roundReels currentRound)
        )
        [0 .. 3]
    helperNetException =
      clovers >= 1
        && helpers >= 2
        && has Net
        && has Snake
        && (remaining >= 4 || (remaining >= 3 && not (has Double)))
    priority preferences = listToMaybe (mapMaybe (\symbol -> findIndex (== symbol) reels) preferences)
    except preserved = findIndex (`notElem` preserved) reels
    -- Preserve occurrences, rather than every symbol of the same kind.
    spare preserved = fst <$> listToMaybe (unpreserved preserved)
    unpreserved = foldl remove [(index, symbol) | (index, symbol) <- zip [0 ..] reels]
    remove [] _ = []
    remove ((index, symbol) : rest) kept
      | symbol == kept = rest
      | otherwise = (index, symbol) : remove rest kept
    cash reason = Recommendation Collect reason
    reroll target reason = maybe (cash reason) (\reelIndex -> Recommendation (Reroll reelIndex) reason) target
    decide condition target reason = if condition then reroll target reason else cash reason
    helperCondition
      | has Net && has Snake = remaining >= 4 || (remaining >= 3 && not (has Double))
      | otherwise = remaining > 1 || has Net || count Snake == 1
    pairSymbol = if count Coin == 2 then Coin else CoinStack
    keptSymbol
      | has Coin = Coin
      | has CoinStack = CoinStack
      | otherwise = Dash
    keepSnake =
      any has [Coin, CoinStack, Dash]
        && (remaining >= 4 || (remaining >= 3 && has Coin))
        && not (has Coin && has CoinStack)
    cloverTarget
      | has Dash = priority [Dash]
      | count Crown == 1 = priority [Crown]
      | count Crown == 2 = except [Clover, Crown]
      | otherwise = Nothing
    pairTarget
      | count Coin == 2 && count CoinStack == 2 = priority [Coin]
      | otherwise = priority (filter (/= pairSymbol) [Dash, Coin, CoinStack, Crown, Snake, Net, Double])
    netDoubleCondition =
      remaining >= 3
        || (remaining >= 2 && (has Coin || has CoinStack))
        || count Double >= 2
        || has Snake
    netDoubleTarget =
      listToMaybe
        [ index
        | symbol <- [Dash, Net, Crown, CoinStack, Coin, Double],
          symbol /= Double || not (has Snake),
          (index, candidate) <- unpreserved [Net, Double],
          candidate == symbol
        ]

-- Affordability is handled separately from the bankroll-independent wiki rules.
autoplayAction :: Game -> Recommendation
autoplayAction game = case activeRound game of
  Nothing -> Recommendation Spin "Start a new round."
  Just currentRound
    | bankroll game < 1 -> Recommendation Collect "No coins available for another bonus spin."
    | otherwise -> recommend currentRound
