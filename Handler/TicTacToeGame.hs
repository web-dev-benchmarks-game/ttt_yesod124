{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.TicTacToeGame where

import Import
import Data.Text as T
import Data.Maybe

toHuman :: KeyBackend backend entity -> T.Text
toHuman = toPathPiece . unKey

entityPlayerName :: Maybe (Entity User) -> Text
entityPlayerName = maybe (T.pack "Unknown Player") (userIdent . entityVal)

maybePlayerName :: Maybe User -> Text
maybePlayerName = maybe (T.pack "Unknown Player") userIdent

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
    game <- runDB $ get404 gameId
    gameBoard <- runDB $ board gameId
    maybePlayer1 <- runDB $ get (ticTacToeGamePlayer1 game)
    maybePlayer2 <- runDB $ get (ticTacToeGamePlayer2 game)
    defaultLayout $ do
        setTitle $ toHtml $ (T.append . T.pack) "TicTacToe - Game " $ toHuman gameId
        $(widgetFile "tictactoe/detail")
