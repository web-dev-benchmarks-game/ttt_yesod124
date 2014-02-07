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

type TicTacToeGameBoard = [[Either (Int, Int) TicTacToeSpace]]

boardField :: TicTacToeGameBoard -> Field Handler (Int, Int)
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


-- Ugh, what the hell is the type of boardWidget??  There's probably a
-- better way to refactor this using template haskell or
-- something. Define message as a Maybe Text or something and splice
-- it. I dunno.
-- renderTicTacToeGame :: Entity TicTacToeGame -> TicTacToeGameBoard -> FormRender Handler (Int, Int) -> Text -> Handler Html
renderTicTacToeGame gameE gameBoard boardWidget message = do
    let gameId = entityKey gameE
    let game = entityVal gameE
    viewer <- maybeAuthId
    maybePlayer1 <- runDB $ get (ticTacToeGamePlayer1 game)
    maybePlayer2 <- runDB $ get (ticTacToeGamePlayer2 game)
    let nextPlayerId = ticTacToeGameNextPlayerId game
    defaultLayout $ do
        titleForGameId gameId
        $(widgetFile "tictactoe/detail")


postTicTacToeGameR :: TicTacToeGameId -> Handler Html
postTicTacToeGameR gameId = do
    game <- runDB $ get404 gameId
    let gameE = Entity gameId game
    gameBoard <- runDB $ board gameId
    ((res, boardWidget), enctype) <- runFormPost $ renderDivs $ areq (boardField gameBoard) "Make a move!" Nothing
    viewer <- maybeAuthId
    let nextPlayerId = ticTacToeGameNextPlayerId game
    case viewer of
      Just viewerId | viewerId == nextPlayerId -> case res of
        FormSuccess (x, y) -> do
          runDB $ insert $ TicTacToeSpace x y gameId (ticTacToeGameNextPlayer game)
          runDB $ update gameId [TicTacToeGameNextPlayer =. ticTacToeGameSuccPlayer game]
          redirect (TicTacToeGameR gameId)
        FormMissing -> renderTicTacToeGame gameE gameBoard boardWidget "Please select the space in which to move."
        FormFailure errors -> renderTicTacToeGame gameE gameBoard boardWidget $ T.concat $ (T.pack "Your move had errors. Please correct: "):errors
      Nothing -> renderTicTacToeGame gameE gameBoard boardWidget "Please log in to move."
      _ -> renderTicTacToeGame gameE gameBoard boardWidget "It's not your turn!"

getTicTacToeGameR :: TicTacToeGameId -> Handler Html
getTicTacToeGameR gameId = do
    game <- runDB $ get404 gameId
    gameBoard <- runDB $ board gameId
    let nextPlayerId = ticTacToeGameNextPlayerId game
    viewer <- maybeAuthId
    ((res, boardWidget), enctype) <- runFormPost $ renderDivs $ areq (boardField gameBoard) "Make a move!" Nothing
    renderTicTacToeGame (Entity gameId game) gameBoard boardWidget ""
