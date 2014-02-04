module TicTacToe where

import Prelude
import Yesod

data Player = PlayerX | PlayerO deriving (Eq, Show, Read)
derivePersistField "Player"
