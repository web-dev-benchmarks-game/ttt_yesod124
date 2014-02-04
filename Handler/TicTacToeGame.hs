{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.TicTacToeGame where

import Import
import Data.Text as T

getTicTacToeListR :: Handler Html
getTicTacToeListR = do
    defaultLayout $ do
        setTitle "TicTacToe - Game list"
        [whamlet|<h1>Hi|]

postTicTacToeGameR :: TicTacToeGameId -> Handler Html
postTicTacToeGameR gameId = do
    -- some stuff
    redirect (TicTacToeGameR gameId)

getTicTacToeGameR :: TicTacToeGameId -> Handler Html
getTicTacToeGameR gameId = do
    defaultLayout $ do
        setTitle $ toHtml $ (T.append . T.pack) "TicTacToe - Game " $ toPathPiece (unKey gameId)
        [whamlet|<h1>Hi|]
