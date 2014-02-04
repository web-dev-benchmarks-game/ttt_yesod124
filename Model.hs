module Model where

import Prelude
import Yesod
import Data.Text (Text, pack)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time
import TicTacToe
import Data.List

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

hasCoords :: Int -> Int -> TicTacToeSpace -> Bool
hasCoords x y (TicTacToeSpace x0 y0 _ _) = x == x0 && y == y0

board gameId = do
    spaces <- selectList [TicTacToeSpaceGame ==. gameId] []
    return [[find (hasCoords x y) $ map entityVal spaces | x <- [0..2]] | y <- [0..2]]

ticTacToeSpaceValueHtml :: TicTacToeSpace -> Html
ticTacToeSpaceValueHtml space = toHtml $ pack $ if ticTacToeSpaceValue space == PlayerX then "X" else "O"
