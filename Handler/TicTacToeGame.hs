{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.TicTacToeGame where

import Import

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
        setTitle $ toHtml ("TicTacToe - Game " ++ show (unKey gameId))
        [whamlet|<h1>Hi|]
