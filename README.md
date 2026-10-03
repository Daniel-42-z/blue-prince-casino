# Blue Prince Casino

A Haskell / Brick terminal recreation of the Casino slot machines in
[Blue Prince](https://blueprince.wiki.gg/wiki/Casino#Slot_Machine).
Includes manual play, autoplay using the wiki's ordered strategy, and a setup
screen for choosing starting money and the regular machine (three bonus spins)
or golden machine (five). Both settings are locked after starting play.
Setup also offers a calculator for finding the optimal action for a hypothetical
four-reel state and remaining reroll count.

## Run

With locally installed GHC 9.10 or newer and cabal-install:

```sh
cabal update
cabal run blue-prince-casino -- --money 250 --machine golden
```

The Cabal components use `default-language: GHC2024`, the compiler's name for
the 2024 language edition. There is no `Haskell2024` language setting.

Enable autoplay after confirming setup, or set an RNG seed for reproducible actions:

```sh
cabal run blue-prince-casino -- --money 100 --auto --delay 100 --seed 42
```

`--money` accepts nonnegative whole numbers (default 100), `--machine` accepts
`regular` or `golden`, `--delay` accepts 50–5000 milliseconds (default 300), and
`--seed` accepts a machine-sized integer. `--help` lists these options.
Money and machine options prefill the setup screen. Edit money with digit keys,
Backspace, and Ctrl-U; select fields with Tab or Up/Down. Choose the machine
with `g`, or Left/Right/Space when its field is selected. Press `c` to switch
between Game and Calculator, or change the selected Mode field with
Left/Right/Space. Press Enter to open the selected mode. Starting money applies
only to Game mode.
Autoplay stays paused during setup, including when `--auto` is supplied.
Run in an interactive terminal; a window of at least 76 columns by 30 rows is
recommended. Select a **Nerd Font v3 or newer** in your terminal settings
(preferably its `Nerd Font Mono` variant) so the reel icons render correctly.
Press `?` for an icon legend with the probabilities.

## Controls

| Key | Action |
| --- | --- |
| Space | Start a round, or collect an active round |
| s | Start a round |
| 1–4 | Pay one coin to reroll that reel |
| Enter | Collect the displayed payout |
| o | Perform one wiki strategy action |
| a | Toggle autoplay |
| h | Show / hide the wiki strategy text and recommended reel highlight |
| r | Restart with the configured starting money and machine |
| + / - | Increase / decrease autoplay speed |
| ? | Show rules and probabilities; pauses autoplay while open |
| Esc | Pause autoplay and close help |
| q / Ctrl-C | Quit |

Manual spin, reroll, collection, and strategy actions pause autoplay. Starting
money and machine cannot be changed in the main UI. Restarting reuses those
settings and preserves the current RNG stream rather than replaying results.
Quit and launch the app again to choose different settings.
The strategy hint is shown initially. Its visibility toggle preserves your game
and autoplay state, and the preference remains in effect when you restart.

## Calculator

Select Calculator in setup and choose the regular or golden machine. The
calculator uses the same pure optimal-strategy calculation as autoplay and
updates its action and explanation after every edit. It shows the current
payout and assumes all remaining bonus spins are affordable.

| Key | Action |
| --- | --- |
| 1–4 | Select a reel when editing reels |
| 5 | Select remaining rerolls when editing reels |
| Tab / Shift-Tab / Left / Right | Move between the four reels and reroll count |
| Up / Down / Space | Cycle the selected symbol or adjust the selected count |
| d / c / s / l / x / n / t / k | Set Dash / Coin / Stack / Clover / Double / Snake / Net / Crown |
| 0–5 in the rerolls field | Set remaining rerolls, limited to 3 or 5 for the selected machine |
| + / - | Increase / decrease remaining rerolls |
| r | Reset to four dashes and the machine's full reroll allowance |
| Esc | Return to setup with your previous settings |
| q / Ctrl-C | Quit |

The calculator does not spend coins or make random draws. To switch machines,
return to setup with Esc, choose the machine, and open Calculator again.

## Rules and strategy

Four reels are sampled independently, including on rerolls. The probabilities
are dash 27%, coin 30%, stack 10%, clover 1%, double 10%, snake 10%, net 4%, and
crown 8%. Both the initial spin and each single-reel bonus spin cost one coin.
Payouts and modifier ordering follow the wiki, including non-stacking nets and
stacking doubles.

Autoplay uses the wiki's ordered conditions, including its exceptions for the
last bonus spin and the five-spin machine. A small, pure dynamic program checks
those recommendations against the exact expected value of all legal actions
using the wiki's probabilities and payouts. It resolves abbreviated edge cases
in favor of the best expected return, preferring the wiki recommendation when
actions tie. Equal symbols are resolved by choosing the leftmost reel. The TUI
displays the rule or expected-value refinement behind its recommendation.
It collects when there is no money for a
bonus spin and stops when a completed round leaves no money for a new spin.
This is the wiki's expected-value strategy; it is not a strategy optimized for
survival with a small bankroll.

Source: [Casino / Slot Machine](https://blueprince.wiki.gg/wiki/Casino#Slot_Machine),
consulted October 3, 2026 through indexed page content (the live page blocked
direct access).

## Structure

`Casino.Game` contains the symbols, payout calculation, immutable game state,
weighted random draws, and pure action reducer. `Casino.Strategy` contains the
ordered wiki rules as pure recommendations. `Casino.Calculator` contains the
pure calculator editing model. `app/Main.hs` draws the interface
and translates input and timer events into state transformations. IO is limited
to startup, argument handling, obtaining the initial RNG, and the Brick timer /
terminal lifecycle. The timer thread is cleaned up when the TUI exits.
`app/Icons.hs` maps each symbol to its display text or Nerd Font glyph. The code uses ordinary
algebraic data types, pattern matching, lists, and pure functions; it does not
require lenses, Template Haskell, or custom language extensions.

## Reel icons

Dash uses the ordinary minus sign `-`, and double uses the literal text `×2`.
The other glyphs come from the official
[Nerd Fonts catalog](https://github.com/ryanoasis/nerd-fonts/blob/master/glyphnames.json).
The circle represents a single coin and the spider web represents the snake-catching
net.

| Symbol | Display text / Nerd Font glyph | Codepoint |
| --- | --- | --- |
| Dash | `-` | U+002D |
| Coin | `nf-md-circle_outline` | U+F0766 |
| Coin stack | `nf-fa-coins` | U+EDE8 |
| Clover | `nf-md-clover` | U+F0816 |
| Double | `×2` | U+00D7 + U+0032 |
| Snake | `nf-md-snake` | U+F150E |
| Net | `nf-md-spider_web` | U+F0BCA |
| Crown | `nf-md-crown` | U+F01A5 |

## Checks

```sh
cabal test
```

The checks cover payout modifiers, strategy exceptions, state accounting,
recommendation legality over every reel combination, and exact expected profit
of the wiki policy for both machines (using dynamic programming), plus calculator
editing, symbol cycling, reroll bounds, reset, and recommendations.
