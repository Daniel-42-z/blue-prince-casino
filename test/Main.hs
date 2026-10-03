module Main (main) where

import Casino.Calculator
import Casino.Game
import Casino.Strategy
import Control.Monad (unless)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import System.Exit (die)
import System.Random (mkStdGen)

main :: IO ()
main = do
  putStrLn $
    "Wiki strategy expected net per round: regular="
      ++ show (expectedProfit 3)
      ++ ", golden="
      ++ show (expectedProfit 5)
  mapM_ (\(name, passed) -> unless passed (die $ "FAILED: " ++ name)) checks
  putStrLn $ "Passed " ++ show (length checks) ++ " checks, including all 4096 reel outcomes."

checks :: [(String, Bool)]
checks =
  payoutChecks
    ++ strategyChecks
    ++ transitionChecks
    ++ calculatorChecks
    ++ [ ("Weights sum to 100", sum (map symbolWeight symbols) == 100),
         ("Payout is independent of reel ordering", all (\reels -> payout reels == payout (reverseReels reels)) allReels),
         ( "Every strategy action is legal",
           all
             legalRecommendation
             [(reels, remaining) | reels <- allReels, remaining <- [0 .. 5]]
         ),
         ("Regular expected profit rounds to wiki's 0.43", abs (expectedProfit 3 - 0.43) < 0.005),
         ("Golden expected profit rounds to wiki's 0.80", abs (expectedProfit 5 - 0.80) < 0.005)
       ]
  where
    reverseReels (Reels first second third fourth) = Reels fourth third second first
    legalRecommendation (reels, remaining) = case recommendedAction (recommend $ Round reels remaining 1) of
      Collect -> True
      Reroll reelIndex -> remaining > 0 && reelIndex >= 0 && reelIndex < 4
      _ -> False

calculatorChecks :: [(String, Bool)]
calculatorChecks =
  [ ("Calculator uses regular bonus limit", calculatorRemaining regular == 3),
    ("Calculator uses golden bonus limit", calculatorRemaining golden == 5),
    ("Calculator edits only the selected reel", calculatorReels edited == Reels Dash Clover Dash Dash),
    ("Calculator symbol selection wraps backwards", calculatorReels backwards == Reels Crown Dash Dash Dash),
    ("Calculator symbol selection wraps forwards", calculatorReels forwards == Reels Dash Dash Dash Dash),
    ("Calculator field selection wraps", calculatorField (applyCalculatorAction (MoveSelection (-1)) regular) == 4),
    ("Calculator regular rerolls are bounded", calculatorRemaining (applyCalculatorAction (SetRemaining 5) regular) == 3),
    ("Calculator golden accepts five rerolls", calculatorRemaining (applyCalculatorAction (SetRemaining 5) golden) == 5),
    ("Calculator prevents negative rerolls", calculatorRemaining (applyCalculatorAction (AdjustRemaining (-10)) regular) == 0),
    ("Calculator cycles the reroll field", calculatorRemaining (applyCalculatorAction (CycleSelection (-1)) remainingField) == 2),
    ("Calculator ignores symbols in reroll field", calculatorReels (applyCalculatorAction (ChooseSymbol Clover) remainingField) == calculatorReels regular),
    ("Calculator reset restores initial state", applyCalculatorAction ResetCalculator edited == regular),
    ("Calculator recommends jackpot chase", recommendedAction (recommend $ calculatorRound crowns) == Reroll 3),
    ("Calculator collects with no remaining rerolls", recommendedAction (recommend $ calculatorRound $ applyCalculatorAction (SetRemaining 0) crowns) == Collect)
  ]
  where
    regular = newCalculator Regular
    golden = newCalculator Golden
    edited = applyCalculatorAction (ChooseSymbol Clover) (applyCalculatorAction (SelectField 1) regular)
    backwards = applyCalculatorAction (CycleSelection (-1)) regular
    forwards = applyCalculatorAction (CycleSelection 1) backwards
    remainingField = applyCalculatorAction (SelectField 4) regular
    crowns =
      foldl
        (flip applyCalculatorAction)
        regular
        [SelectField 0, ChooseSymbol Crown, SelectField 1, ChooseSymbol Crown, SelectField 2, ChooseSymbol Crown]

payoutChecks :: [(String, Bool)]
payoutChecks =
  [ (name, payout reels == expected)
  | (name, reels, expected) <-
      [ ("Three coins", Reels Coin Coin Coin Dash, 3),
        ("Four coins replaces three-coin prize", Reels Coin Coin Coin Coin, 5),
        ("Three stacks", Reels CoinStack CoinStack CoinStack Dash, 9),
        ("Four stacks replaces three-stack prize", Reels CoinStack CoinStack CoinStack CoinStack, 15),
        ("Crown jackpot", Reels Crown Crown Crown Crown, 100),
        ("No partial crown prize", Reels Crown Crown Crown Dash, 0),
        ("Clovers add to matches", Reels Coin Coin Coin Clover, 13),
        ("Snake overrides clover and double", Reels Clover Double Snake Coin, 0),
        ("Net disables snake before doubling", Reels Net Snake Clover Double, 26),
        ("Nets do not stack", Reels Net Net Snake Snake, 6),
        ("Doubles stack", Reels Clover Double Double Double, 80)
      ]
  ]

strategyChecks :: [(String, Bool)]
strategyChecks =
  [ (name, recommendedAction (recommend $ Round reels remaining 1) == expected)
  | (name, reels, remaining, expected) <-
      [ ("Three crowns chase jackpot", Reels Crown Coin Crown Crown, 1, Reroll 1),
        ("Three crowns protect last-spin clover", Reels Crown Clover Crown Crown, 1, Collect),
        ("Three helpers protect last-spin spare", Reels Clover Double Double Dash, 1, Collect),
        ("Three helpers remove last-spin snake", Reels Clover Double Double Snake, 1, Reroll 3),
        ("Two clovers and snake-net use three spins", Reels Clover Clover Snake Net, 3, Reroll 2),
        ("Clover-double and snake-net require four spins", Reels Clover Double Snake Net, 3, Collect),
        ("Clover-double and snake-net use four spins", Reels Clover Double Snake Net, 4, Reroll 2),
        ("Clover helper stops before later rules", Reels Clover Double Coin Crown, 1, Collect),
        ("Clover-net keeps the named pair", Reels Clover Net Dash Coin, 2, Reroll 2),
        ("Clover-net leaves contributing snakes", Reels Clover Net Snake Snake, 3, Collect),
        ("Clover pair net last-spin exception", Reels Clover Coin Coin Net, 1, Reroll 1),
        ("Clover snakes need enough spins", Reels Clover Snake Snake Snake, 2, Collect),
        ("Clover snakes removable with enough spins", Reels Clover Snake Snake Snake, 3, Reroll 1),
        ("Preserve one dash but reroll the duplicate", Reels Clover Snake Dash Dash, 4, Reroll 3),
        ("Clover rerolls lone non-crown", Reels Clover Crown Coin Crown, 2, Reroll 2),
        ("Clover with three coins collects", Reels Clover Coin Coin Coin, 3, Collect),
        ("Clover with three stacks collects", Reels Clover CoinStack CoinStack CoinStack, 3, Collect),
        ("Two crowns and a pair pivot", Reels Crown Coin Crown Coin, 2, Reroll 0),
        ("Two crowns preserve snake-net", Reels Crown Crown Snake Net, 5, Collect),
        ("Three coins remove snake", Reels Coin Coin Snake Coin, 1, Reroll 2),
        ("Three stacks stop on last spin", Reels CoinStack CoinStack CoinStack Dash, 1, Collect),
        ("Mixed pairs reroll coin", Reels CoinStack Coin CoinStack Coin, 2, Reroll 1),
        ("Pair double last-spin exception", Reels Coin Coin Double Crown, 1, Reroll 3),
        ("Pair without helper collects on last spin", Reels Coin Coin Dash Crown, 1, Collect),
        ("Net-double preserves contributing reels", Reels Net Double Snake Snake, 1, Collect),
        ("Net-double rerolls spare coin", Reels Coin Double Snake Net, 1, Reroll 0),
        ("Pair preserves paying snake-net", Reels Coin Coin Snake Net, 3, Collect),
        ("Three stacks preserve double", Reels CoinStack CoinStack CoinStack Double, 3, Collect),
        ("Three coins preserve double", Reels Coin Coin Coin Double, 5, Collect),
        ("Lone clover protects last-spin payout", Reels Dash Dash Dash Clover, 1, Collect),
        ("Coin-double requires enough spins", Reels Coin Double Dash Crown, 4, Collect),
        ("Coin-double uses five spins", Reels Coin Double Dash Crown, 5, Reroll 2),
        ("Snake and doubles reroll extra snake", Reels Snake Double Snake Double, 5, Reroll 2),
        ("No strategy without bonus spins", Reels Crown Crown Crown Dash, 0, Collect)
      ]
  ]

transitionChecks :: [(String, Bool)]
transitionChecks =
  [ ("Spin charges one coin", bankroll spun == 9 && totalSpent spun == 1),
    ("Regular starts with three bonus spins", bonusRemaining (fromJust $ activeRound spun) == 3),
    ("Golden starts with five bonus spins", bonusRemaining (fromJust $ activeRound goldenRound) == 5),
    ( "Play preserves starting money and machine",
      all
        ( \currentGame ->
            startingMoney currentGame == 10 && selectedMachine currentGame == Regular
        )
        [spun, rerolled, collected]
    ),
    ( "Reroll charges a coin and consumes a bonus",
      bankroll rerolled == 8
        && bonusRemaining (fromJust $ activeRound rerolled) == 2
    ),
    ("Collection balances accounting", bankroll collected == startingMoney collected - totalSpent collected + totalWon collected),
    ("Repeated collection cannot pay twice", bankroll (applyAction Collect collected) == bankroll collected),
    ( "A broke player can still collect",
      recommendedAction (autoplayAction brokeRound) == Collect
        && bankroll (applyAction Collect brokeRound) == 80
    ),
    ("Cannot start without money", activeRound (applyAction Spin $ newGame 0 Regular $ mkStdGen 42) == Nothing),
    ("RNG is reproducible", activeRound spun == activeRound (applyAction Spin initialGame))
  ]
  where
    initialGame = newGame 10 Regular (mkStdGen 42)
    spun = applyAction Spin initialGame
    goldenRound = applyAction Spin (newGame 10 Golden $ mkStdGen 42)
    rerolled = applyAction (Reroll 0) spun
    collected = applyAction Collect rerolled
    brokeRound = initialGame {bankroll = 0, activeRound = Just (Round (Reels Clover Double Double Double) 3 1)}

allReels :: [Reels]
allReels = [Reels first second third fourth | first <- symbols, second <- symbols, third <- symbols, fourth <- symbols]

-- Evaluate the complete wiki policy by dynamic programming, rather than Monte
-- Carlo. This independently checks the combined effects of its ordered rules.
expectedProfit :: Int -> Double
expectedProfit remaining =
  sum
    [probability reels * lookupValue policy reels | reels <- allReels]
    - 1
  where
    canonicalReels =
      [ reels
      | reels@(Reels first second third fourth) <- allReels,
        first <= second,
        second <= third,
        third <= fourth
      ]
    key = sort . reelSymbols
    lookupValue table reels = Map.findWithDefault 0 (key reels) table
    initialTable = Map.fromList [(key reels, fromIntegral $ payout reels) | reels <- canonicalReels]
    policy = foldl buildTable initialTable [1 .. remaining]
    buildTable previous bonus =
      Map.fromList
        [(key reels, value previous bonus reels) | reels <- canonicalReels]
    value previous bonus reels = case recommendedAction (recommend $ Round reels bonus 1) of
      Reroll reelIndex ->
        -1
          + sum
            [ fromIntegral (symbolWeight symbol) / 100 * lookupValue previous (replaceReel reelIndex symbol reels)
            | symbol <- symbols
            ]
      _ -> fromIntegral (payout reels)
    probability reels = product [fromIntegral (symbolWeight symbol) / 100 | symbol <- reelSymbols reels]
