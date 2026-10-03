module Casino.Calculator
  ( Calculator (..),
    CalculatorAction (..),
    newCalculator,
    applyCalculatorAction,
    calculatorRound,
  )
where

import Casino.Game

data Calculator = Calculator
  { calculatorMachine :: Machine,
    calculatorReels :: Reels,
    calculatorRemaining :: Int,
    calculatorField :: Int -- Four reels (0-3), followed by remaining rerolls (4).
  }
  deriving (Eq, Show)

data CalculatorAction
  = SelectField Int
  | MoveSelection Int
  | CycleSelection Int
  | ChooseSymbol Symbol
  | SetRemaining Int
  | AdjustRemaining Int
  | ResetCalculator
  deriving (Eq, Show)

newCalculator :: Machine -> Calculator
newCalculator machine = Calculator machine (Reels Dash Dash Dash Dash) (bonusLimit machine) 0

calculatorRound :: Calculator -> Round
calculatorRound calculator = Round (calculatorReels calculator) (calculatorRemaining calculator) 0

-- Editing a hypothetical state does not roll symbols, spend money, or use IO.
applyCalculatorAction :: CalculatorAction -> Calculator -> Calculator
applyCalculatorAction action calculator = case action of
  SelectField selectedField -> calculator {calculatorField = max 0 (min 4 selectedField)}
  MoveSelection direction -> calculator {calculatorField = (field + direction) `mod` 5}
  CycleSelection direction
    | field == 4 -> applyCalculatorAction (AdjustRemaining direction) calculator
    | otherwise -> case drop field (reelSymbols $ calculatorReels calculator) of
        symbol : _ ->
          applyCalculatorAction
            (ChooseSymbol $ toEnum $ (fromEnum symbol + direction) `mod` length symbols)
            calculator
        [] -> calculator
  ChooseSymbol symbol
    | field < 4 -> calculator {calculatorReels = replaceReel field symbol (calculatorReels calculator)}
    | otherwise -> calculator
  SetRemaining remaining ->
    calculator
      { calculatorRemaining = max 0 (min (bonusLimit $ calculatorMachine calculator) remaining)
      }
  AdjustRemaining direction -> applyCalculatorAction (SetRemaining $ calculatorRemaining calculator + direction) calculator
  ResetCalculator -> newCalculator (calculatorMachine calculator)
  where
    field = calculatorField calculator
