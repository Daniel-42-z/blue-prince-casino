module Main (main) where

import Brick hiding (get, modify)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Center (center, hCenter)
import Casino.Calculator
import Casino.Game
import Casino.Strategy
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.State.Strict (get, modify)
import Data.Maybe (isNothing)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Icons (symbolIcon)
import System.Environment (getArgs)
import System.Exit (die)
import System.Random (mkStdGen, newStdGen)
import Text.Read (readMaybe)

data Name = Screen deriving (Eq, Ord, Show)

data Tick = Tick

data SessionMode = PlayMode | CalculatorMode deriving (Eq)

data SetupField = ModeField | MoneyField | MachineField | StartField deriving (Eq)

data Setup = Setup
  { setupMoney :: String,
    setupMachine :: Machine,
    setupMode :: SessionMode,
    setupField :: SetupField,
    setupError :: String
  }

data Ui = Ui
  { game :: Game,
    autoEnabled :: Bool,
    setupScreen :: Maybe Setup,
    calculatorScreen :: Maybe Calculator,
    helpVisible :: Bool,
    strategyVisible :: Bool,
    stepDelay :: Int,
    pendingTicks :: Int
  }

data Options = Options
  { initialMoney :: Integer,
    initialMachine :: Machine,
    initialAuto :: Bool,
    initialDelay :: Int,
    initialSeed :: Maybe Int
  }

defaultOptions :: Options
defaultOptions = Options 100 Regular False 300 Nothing

usage :: String
usage =
  unlines
    [ "Blue Prince Casino",
      "Usage: blue-prince-casino [--money COINS] [--machine regular|golden]",
      "                         [--auto] [--delay MILLISECONDS] [--seed INTEGER]",
      "",
      "Defaults: 100 coins, regular machine, manual play, 300 ms per action.",
      "Money and machine options prefill setup; choose Game or Calculator.",
      "Use --help for this message. Press ? inside the TUI for controls."
    ]

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go options [] = Right options
    go options ("--auto" : rest) = go options {initialAuto = True} rest
    go options ("--money" : value : rest) = case readMaybe value of
      Just amount | amount >= 0 -> go options {initialMoney = amount} rest
      _ -> Left "--money requires a nonnegative whole number."
    go options ("--machine" : value : rest) = case value of
      "regular" -> go options {initialMachine = Regular} rest
      "golden" -> go options {initialMachine = Golden} rest
      _ -> Left "--machine must be regular or golden."
    go options ("--delay" : value : rest) = case readMaybe value of
      Just delay | delay >= 50 && delay <= 5000 -> go options {initialDelay = delay} rest
      _ -> Left "--delay requires 50 to 5000 milliseconds."
    go options ("--seed" : value : rest) = case readMaybe value of
      Just seed -> go options {initialSeed = Just seed} rest
      _ -> Left "--seed requires a machine-sized integer."
    go _ arguments = Left ("Unrecognized or incomplete arguments: " ++ unwords arguments)

main :: IO ()
main = do
  arguments <- getArgs
  if "--help" `elem` arguments
    then putStr usage
    else case parseOptions arguments of
      Left message -> die (message ++ "\n\n" ++ usage)
      Right options -> do
        generator <- maybe newStdGen (pure . mkStdGen) (initialSeed options)
        eventChannel <- newBChan 8
        let buildVty = mkVty V.defaultConfig
            initialUi =
              Ui
                { game = newGame (initialMoney options) (initialMachine options) generator,
                  autoEnabled = initialAuto options,
                  setupScreen = Just (Setup (show $ initialMoney options) (initialMachine options) PlayMode MoneyField ""),
                  calculatorScreen = Nothing,
                  helpVisible = False,
                  strategyVisible = True,
                  stepDelay = initialDelay options,
                  pendingTicks = 0
                }
            timer = forever (threadDelay 50000 >> writeBChan eventChannel Tick)
        bracket (forkIO timer) killThread $ \_ -> do
          initialVty <- buildVty
          void $ customMain initialVty buildVty (Just eventChannel) app initialUi

app :: App Ui Tick Name
app =
  App
    { appDraw = drawUi,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (attrName "title", fg V.cyan `V.withStyle` V.bold),
              (attrName "gold", fg V.yellow `V.withStyle` V.bold),
              (attrName "good", fg V.green),
              (attrName "muted", fg V.brightBlack),
              (attrName "selected", fg V.black `V.withBackColor` V.yellow),
              (attrName "snake", fg V.red)
            ]
    }

-- Brick's event boundary only selects pure transformations of Ui.
handleEvent :: BrickEvent Name Tick -> EventM Name Ui ()
handleEvent event = do
  currentUi <- get
  case event of
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) -> halt
    VtyEvent (V.EvKey (V.KChar 'q') []) -> halt
    _ | Just _ <- calculatorScreen currentUi -> modify (handleCalculatorEvent event)
    VtyEvent (V.EvKey V.KEsc []) | Just _ <- setupScreen currentUi -> halt
    _ | Just _ <- setupScreen currentUi -> modify (handleSetupEvent event)
    VtyEvent (V.EvKey V.KEsc []) -> modify (\ui -> ui {autoEnabled = False, helpVisible = False})
    VtyEvent (V.EvKey (V.KChar '?') []) -> modify (\ui -> ui {helpVisible = not (helpVisible ui)})
    VtyEvent (V.EvKey (V.KChar 'h') []) -> modify (\ui -> ui {strategyVisible = not (strategyVisible ui)})
    VtyEvent (V.EvKey (V.KChar 'a') []) -> modify (\ui -> ui {autoEnabled = not (autoEnabled ui), pendingTicks = 0})
    VtyEvent (V.EvKey (V.KChar 'r') []) -> modify restart
    VtyEvent (V.EvKey (V.KChar '+') []) -> modify (\ui -> ui {stepDelay = max 50 (stepDelay ui - 50)})
    VtyEvent (V.EvKey (V.KChar '-') []) -> modify (\ui -> ui {stepDelay = min 5000 (stepDelay ui + 50)})
    VtyEvent (V.EvKey (V.KChar 'o') []) ->
      modify
        ( \ui ->
            manualAction (recommendedAction $ autoplayAction $ game ui) ui
        )
    VtyEvent (V.EvKey (V.KChar 's') []) -> modify (manualAction Spin)
    VtyEvent (V.EvKey (V.KChar ' ') []) ->
      modify
        ( \ui ->
            manualAction
              (if isNothing (activeRound $ game ui) then Spin else Collect)
              ui
        )
    VtyEvent (V.EvKey V.KEnter []) -> modify (manualAction Collect)
    VtyEvent (V.EvKey (V.KChar digit) [])
      | digit `elem` ['1' .. '4'] ->
          modify (manualAction $ Reroll $ fromEnum digit - fromEnum '1')
    AppEvent Tick -> modify advanceTimer
    _ -> pure ()

manualAction :: Action -> Ui -> Ui
manualAction action ui = ui {game = applyAction action (game ui), autoEnabled = False, pendingTicks = 0}

restart :: Ui -> Ui
restart ui =
  ui
    { game = newGame (startingMoney currentGame) (selectedMachine currentGame) (randomGenerator currentGame),
      autoEnabled = False,
      pendingTicks = 0
    }
  where
    currentGame = game ui

advanceTimer :: Ui -> Ui
advanceTimer ui
  | not (autoEnabled ui)
      || helpVisible ui
      || not (isNothing $ setupScreen ui)
      || not (isNothing $ calculatorScreen ui) =
      ui
  | pendingTicks ui + 50 < stepDelay ui = ui {pendingTicks = pendingTicks ui + 50}
  | bankroll currentGame == 0 && isNothing (activeRound currentGame) =
      ui
        { autoEnabled = False,
          pendingTicks = 0,
          game = currentGame {statusMessage = "Autoplay stopped: no coins remain."}
        }
  | otherwise =
      ui
        { game = applyAction (recommendedAction $ autoplayAction currentGame) currentGame,
          pendingTicks = 0
        }
  where
    currentGame = game ui

-- Setup editing and the transition into play are pure state transformations.
handleSetupEvent :: BrickEvent Name Tick -> Ui -> Ui
handleSetupEvent event ui = case event of
  VtyEvent (V.EvKey V.KEnter []) -> startSession ui
  VtyEvent (V.EvKey (V.KChar ' ') [])
    | Just setup <- setupScreen ui, setupField setup == StartField -> startSession ui
  VtyEvent (V.EvKey key modifiers) -> ui {setupScreen = fmap (editSetup key modifiers) (setupScreen ui)}
  _ -> ui

editSetup :: V.Key -> [V.Modifier] -> Setup -> Setup
editSetup key modifiers setup = case (key, modifiers) of
  (V.KChar '\t', []) -> setup {setupField = nextField (setupMode setup) (setupField setup)}
  (V.KDown, []) -> setup {setupField = nextField (setupMode setup) (setupField setup)}
  (V.KBackTab, []) -> setup {setupField = previousField (setupMode setup) (setupField setup)}
  (V.KUp, []) -> setup {setupField = previousField (setupMode setup) (setupField setup)}
  (V.KChar 'c', []) -> changeMode
  (V.KLeft, []) | setupField setup == ModeField -> changeMode
  (V.KRight, []) | setupField setup == ModeField -> changeMode
  (V.KChar ' ', []) | setupField setup == ModeField -> changeMode
  (V.KChar 'g', []) -> changeMachine
  (V.KLeft, []) | setupField setup == MachineField -> changeMachine
  (V.KRight, []) | setupField setup == MachineField -> changeMachine
  (V.KChar ' ', []) | setupField setup == MachineField -> changeMachine
  (V.KBS, []) | setupField setup == MoneyField -> changeMoney (if null input then "" else init input)
  (V.KChar 'u', [V.MCtrl]) | setupField setup == MoneyField -> changeMoney ""
  (V.KChar digit, [])
    | setupField setup == MoneyField && digit >= '0' && digit <= '9' && length input < 30 ->
        changeMoney (input ++ [digit])
  _ -> setup
  where
    input = setupMoney setup
    changeMoney value = setup {setupMoney = value, setupError = ""}
    changeMachine = setup {setupMachine = toggleMachine (setupMachine setup)}
    changeMode =
      setup
        { setupMode = if setupMode setup == PlayMode then CalculatorMode else PlayMode,
          setupField = if setupField setup == MoneyField then ModeField else setupField setup,
          setupError = ""
        }

nextField :: SessionMode -> SetupField -> SetupField
nextField mode ModeField = if mode == PlayMode then MoneyField else MachineField
nextField _ MoneyField = MachineField
nextField _ MachineField = StartField
nextField _ StartField = ModeField

previousField :: SessionMode -> SetupField -> SetupField
previousField _ ModeField = StartField
previousField _ MoneyField = ModeField
previousField mode MachineField = if mode == PlayMode then MoneyField else ModeField
previousField _ StartField = MachineField

startSession :: Ui -> Ui
startSession ui = case setupScreen ui of
  Nothing -> ui
  Just setup
    | setupMode setup == CalculatorMode ->
        ui
          { calculatorScreen = Just (newCalculator $ setupMachine setup),
            pendingTicks = 0
          }
  Just setup -> case readMaybe (setupMoney setup) of
    Just amount
      | amount >= 0 ->
          ui
            { game = newGame amount (setupMachine setup) (randomGenerator $ game ui),
              setupScreen = Nothing,
              pendingTicks = 0
            }
    _ ->
      ui
        { setupScreen =
            Just
              setup
                { setupField = MoneyField,
                  setupError = "Enter a nonnegative whole number of coins."
                }
        }

handleCalculatorEvent :: BrickEvent Name Tick -> Ui -> Ui
handleCalculatorEvent event ui = case event of
  VtyEvent (V.EvKey V.KEsc []) -> ui {calculatorScreen = Nothing, pendingTicks = 0}
  VtyEvent (V.EvKey key []) ->
    ui
      { calculatorScreen = fmap (editCalculator key) (calculatorScreen ui)
      }
  _ -> ui

editCalculator :: V.Key -> Calculator -> Calculator
editCalculator key calculator = case key of
  V.KChar 'r' -> change ResetCalculator
  V.KChar '+' -> change (AdjustRemaining 1)
  V.KChar '-' -> change (AdjustRemaining (-1))
  V.KChar '\t' -> change (MoveSelection 1)
  V.KBackTab -> change (MoveSelection (-1))
  V.KRight -> change (MoveSelection 1)
  V.KLeft -> change (MoveSelection (-1))
  V.KUp -> change (CycleSelection 1)
  V.KDown -> change (CycleSelection (-1))
  V.KChar ' ' -> change (CycleSelection 1)
  V.KChar digit
    | calculatorField calculator == 4 && digit >= '0' && digit <= '5' ->
        change (SetRemaining $ fromEnum digit - fromEnum '0')
    | digit >= '1' && digit <= '5' -> change (SelectField $ fromEnum digit - fromEnum '1')
    | Just symbol <- lookup digit calculatorKeys -> change (ChooseSymbol symbol)
  _ -> calculator
  where
    change action = applyCalculatorAction action calculator

calculatorKeys :: [(Char, Symbol)]
calculatorKeys =
  [ ('d', Dash),
    ('c', Coin),
    ('s', CoinStack),
    ('l', Clover),
    ('x', Double),
    ('n', Snake),
    ('t', Net),
    ('k', Crown)
  ]

drawCalculator :: Calculator -> Widget Name
drawCalculator calculator =
  center $
    hLimit 76 $
      borderWithLabel (withAttr (attrName "title") $ str " BLUE PRINCE / STRATEGY CALCULATOR ") $
        padAll 1 $
          vBox
            [ str $ "Machine: " ++ machineLabel (calculatorMachine calculator),
              str "Select a field and enter its value. The result updates automatically.",
              padTop (Pad 1) $
                hBox $
                  zipWith
                    (\index symbol -> reelWidget index (Just symbol) (calculatorField calculator == index))
                    [0 .. 3]
                    (reelSymbols $ calculatorReels calculator),
              padTop (Pad 1) $
                withAttr remainingAttr $
                  str $
                    "Rerolls remaining: "
                      ++ show (calculatorRemaining calculator)
                      ++ " (0-"
                      ++ show (bonusLimit $ calculatorMachine calculator)
                      ++ ")",
              str $ "Editing: " ++ editingLabel,
              hBorder,
              str $ "Current payout: " ++ show (payout $ calculatorReels calculator) ++ " coins",
              withAttr (attrName "good") $ str $ "Optimal strategy: " ++ actionLabel (recommendedAction result),
              strWrap (recommendationReason result),
              str "Assumes you can afford all remaining bonus spins.",
              hBorder,
              str "1-4: select reel    5: select rerolls    Tab/Left/Right: select",
              str "Up/Down/Space: change value    +/-: adjust remaining rerolls",
              str "In the rerolls field, type a number from 0 to the machine limit.",
              strWrap $
                unwords
                  [[key] ++ ": " ++ symbolIcon symbol ++ " " ++ symbolName symbol | (key, symbol) <- calculatorKeys],
              str "r: reset calculator    Esc: back to setup    q: quit"
            ]
  where
    result = recommend (calculatorRound calculator)
    remainingAttr = attrName $ if calculatorField calculator == 4 then "selected" else "title"
    editingLabel = case drop (calculatorField calculator) (reelSymbols $ calculatorReels calculator) of
      symbol : _ -> "Reel " ++ show (calculatorField calculator + 1) ++ " (" ++ symbolName symbol ++ ")"
      [] -> "Rerolls remaining"

drawSetup :: Setup -> Widget Name
drawSetup setup =
  center $
    hLimit 64 $
      borderWithLabel (withAttr (attrName "title") $ str " BLUE PRINCE / SETUP ") $
        padAll 1 $
          vBox
            [ str "Choose Game or Calculator, then select your machine.",
              padTop (Pad 1) $
                field ModeField $
                  str $
                    "Mode: "
                      ++ if setupMode setup == PlayMode then "Game" else "Calculator",
              if setupMode setup == PlayMode
                then
                  padTop (Pad 1) $
                    field MoneyField $
                      str $
                        "Starting money: "
                          ++ setupMoney setup
                          ++ if setupField setup == MoneyField then "_" else ""
                else emptyWidget,
              padTop (Pad 1) $ field MachineField $ str $ "Machine: " ++ machineLabel (setupMachine setup),
              padTop (Pad 1) $
                field StartField $
                  str $
                    if setupMode setup == PlayMode then "[ Start casino ]" else "[ Open calculator ]",
              padTop (Pad 1) $ withAttr (attrName "snake") $ strWrap (setupError setup),
              padTop (Pad 1) $
                strWrap $
                  if setupMode setup == PlayMode
                    then "Starting money and machine are locked once play starts."
                    else "Enter hypothetical reel states to calculate the optimal action.",
              padTop (Pad 1) $ str "Tab / Up / Down: select a field",
              str "c: Game / Calculator    g: choose machine",
              str "Left / Right / Space: change selected option",
              if setupMode setup == PlayMode then str "Backspace: delete money digit    Ctrl-U: clear money" else emptyWidget,
              str "Enter: start    q / Esc: quit"
            ]
  where
    field selected widget =
      withAttr
        (attrName $ if setupField setup == selected then "selected" else "title")
        widget

drawUi :: Ui -> [Widget Name]
drawUi ui
  | Just calculator <- calculatorScreen ui = [drawCalculator calculator]
  | Just setup <- setupScreen ui = [drawSetup setup]
  | otherwise = overlays ++ [center $ hLimit 76 $ borderWithLabel (withAttr (attrName "title") $ str " BLUE PRINCE / CASINO ") $ padAll 1 body]
  where
    currentGame = game ui
    currentRound = activeRound currentGame
    recommendation = autoplayAction currentGame
    overlays
      | helpVisible ui = [center $ hLimit 72 $ borderWithLabel (str " Rules and controls ") $ padAll 1 helpWidget]
      | otherwise = []
    body =
      vBox
        [ withAttr (attrName "gold") $ str $ "BANKROLL  " ++ show (bankroll currentGame) ++ " coins",
          str $
            "Start: "
              ++ show (startingMoney currentGame)
              ++ "   Net: "
              ++ signed (bankroll currentGame - startingMoney currentGame)
              ++ "   Completed rounds: "
              ++ show (completedRounds currentGame),
          str $
            "Spent: "
              ++ show (totalSpent currentGame)
              ++ "   Won: "
              ++ show (totalWon currentGame)
              ++ "   Last payout: "
              ++ maybe "-" show (lastPayout currentGame),
          padTop (Pad 1) $ str $ "Machine: " ++ machineLabel (selectedMachine currentGame),
          str $
            "[a] Mode: "
              ++ (if autoEnabled ui then "AUTOPLAY" else "MANUAL")
              ++ "   [+/-] Delay: "
              ++ show (stepDelay ui)
              ++ " ms",
          padTop (Pad 1) hBorder,
          padTop (Pad 1) $ hBox $ case currentRound of
            Nothing -> map (\reelIndex -> reelWidget reelIndex Nothing False) [0 .. 3]
            Just roundState ->
              zipWith
                ( \reelIndex symbol ->
                    reelWidget
                      reelIndex
                      (Just symbol)
                      (strategyVisible ui && recommendedAction recommendation == Reroll reelIndex)
                )
                [0 .. 3]
                (reelSymbols $ roundReels roundState),
          padTop (Pad 1) $ str $ case currentRound of
            Nothing -> "Ready for a new round. Each spin costs 1 coin."
            Just roundState ->
              "Payout: "
                ++ show (payout $ roundReels roundState)
                ++ "   Bonus spins left: "
                ++ show (bonusRemaining roundState)
                ++ "   Round cost: "
                ++ show (roundCost roundState),
          if strategyVisible ui
            then
              vBox
                [ padTop (Pad 1) $ withAttr (attrName "good") $ str $ "Wiki strategy: " ++ actionLabel (recommendedAction recommendation),
                  strWrap (recommendationReason recommendation)
                ]
            else emptyWidget,
          padTop (Pad 1) hBorder,
          padTop (Pad 1) $ strWrap (statusMessage currentGame),
          padTop (Pad 1) $ str "Space: spin/collect  1-4: reroll  Enter: collect  o: strategy step",
          str "a: autoplay  h: show/hide strategy  r: restart",
          str "+/-: faster/slower  ?: rules  q: quit"
        ]

reelWidget :: Int -> Maybe Symbol -> Bool -> Widget Name
reelWidget reelIndex symbol suggested =
  padRight (Pad 1) $
    hLimit 16 $
      borderWithLabel (str $ " " ++ show (reelIndex + 1) ++ " ") $
        padTopBottom 1 $
          hCenter $
            withAttr reelAttr $
              str $
                maybe (symbolIcon Dash) symbolIcon symbol
  where
    reelAttr
      | suggested = attrName "selected"
      | symbol == Just Snake = attrName "snake"
      | symbol == Just Clover = attrName "good"
      | symbol `elem` [Nothing, Just Dash] = attrName "muted"
      | symbol `elem` [Just Coin, Just CoinStack, Just Crown, Just Double] = attrName "gold"
      | otherwise = attrName "title"

machineLabel :: Machine -> String
machineLabel machine = show machine ++ " (" ++ show (bonusLimit machine) ++ " bonus spins)"

actionLabel :: Action -> String
actionLabel Spin = "SPIN"
actionLabel Collect = "COLLECT"
actionLabel (Reroll reelIndex) = "REROLL REEL " ++ show (reelIndex + 1)

signed :: Integer -> String
signed amount = (if amount >= 0 then "+" else "") ++ show amount

helpWidget :: Widget Name
helpWidget =
  vBox
    [ withAttr (attrName "title") $ str "Payouts",
      str "3/4 coins: 3/5     3/4 stacks: 9/15     4 crowns: 100",
      str "Clover: +10 each. Net: +3 per snake, neutralizes snakes.",
      str "Any snake without a net makes the entire payout zero.",
      str "Doubles multiply the final payout by 2 each (they stack).",
      padTop (Pad 1) $ str "Each initial spin and single-reel bonus spin costs 1 coin.",
      str "Regular: 3 bonus spins. Golden: 5 bonus spins.",
      padTop (Pad 1) $ withAttr (attrName "title") $ str "Independent symbol odds",
      strWrap $ unwords [symbolIcon symbol ++ " " ++ symbolName symbol ++ ":" ++ show (symbolWeight symbol) ++ "%" | symbol <- symbols],
      padTop (Pad 1) $ str "Autoplay follows the ordered rules on the Casino wiki.",
      str "The strategy hint highlights the reel it recommends rerolling.",
      str "h: show/hide the strategy text and reel highlight.",
      str "Autoplay collects when no bonus spin is affordable.",
      str "Manual play keys pause autoplay. Help also pauses the timer.",
      str "Starting money and machine are locked for this session.",
      str "Restart resets statistics and reuses the same settings.",
      padTop (Pad 1) $ str "?: close help    Esc: close help and pause    q: quit"
    ]
