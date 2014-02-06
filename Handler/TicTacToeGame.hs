{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.TicTacToeGame where

import Import
import qualified Data.Text as T
import Yesod.Auth
import Data.List.Split

toHuman :: KeyBackend backend entity -> T.Text
toHuman = toPathPiece . unKey

entityPlayerName :: Maybe (Entity User) -> Text
entityPlayerName = maybe (T.pack "Unknown Player") (userIdent . entityVal)

maybePlayerName :: Maybe User -> Text
maybePlayerName = maybe (T.pack "Unknown Player") userIdent

titleForGameId gameId = setTitle $ toHtml $ (T.append . T.pack) "TicTacToe - Game " $ toHuman gameId

getTicTacToeListR :: Handler Html
getTicTacToeListR = do
    defaultLayout $ do
        setTitle "TicTacToe - Game list"
        [whamlet|<h1>Hi|]

postTicTacToeGameR :: TicTacToeGameId -> Handler Html
postTicTacToeGameR gameId = do
    defaultLayout $ do
        titleForGameId gameId
        [whamlet|<h1>You made a move:|]
    -- some stuff
    -- redirect (TicTacToeGameR gameId)


boardField :: [[Either (Int, Int) TicTacToeSpace]] -> Field Handler (Int, Int)
boardField gameBoard = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            [space] -> let chunks = splitOn "," (T.unpack space) in
                if length chunks == 2
                then
                    let firstChunk = chunks !! 0
                        secondChunk = chunks !! 1
                    in return $ Right $ Just (read firstChunk, read secondChunk)
                else
                    return $ Left $ SomeMessage $ T.append "Invalid space value " space
            [] -> return $ Right Nothing
            _ -> return $ Left $ "Didn't get any space"
    , fieldView = \idAttr nameAttr otherAttrs eResult isReq ->
        $(widgetFile "tictactoe/board-form")
    , fieldEnctype = UrlEncoded
    }

getTicTacToeGameR :: TicTacToeGameId -> Handler Html
getTicTacToeGameR gameId = do
    game <- runDB $ get404 gameId
    gameBoard <- runDB $ board gameId
    maybePlayer1 <- runDB $ get (ticTacToeGamePlayer1 game)
    maybePlayer2 <- runDB $ get (ticTacToeGamePlayer2 game)
    let nextPlayerId = ticTacToeGameNextPlayerId game
    viewer <- maybeAuthId
    ((res, boardWidget), enctype) <- runFormPost $ renderDivs $ areq (boardField gameBoard) "Make a move!" Nothing
    defaultLayout $ do
        titleForGameId gameId
        $(widgetFile "tictactoe/detail")
