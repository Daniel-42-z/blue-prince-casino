module Icons (symbolIcon) where

import Casino.Game (Symbol (..))

-- Standard text for dash and double; Nerd Fonts v3 for the other icons.
-- Keep presentation choices separate from the game's symbols and rules.
symbolIcon :: Symbol -> String
symbolIcon Dash = "-"
symbolIcon Coin = "\xf0766" -- 󰝦 nf-md-circle_outline: a single coin; U+F0766
symbolIcon CoinStack = "\xede8" --  nf-fa-coins; U+EDE8
symbolIcon Clover = "\xf0816" -- 󰠖 nf-md-clover; U+F0816
symbolIcon Double = "×2"
symbolIcon Snake = "\xf150e" -- 󱔎 nf-md-snake; U+F150E
symbolIcon Net = "\xf0bca" -- 󰯊 nf-md-spider_web; U+F0BCA
symbolIcon Crown = "\xf01a5" -- 󰆥 nf-md-crown; U+F01A5
