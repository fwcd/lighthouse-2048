{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Lighthouse.Connection
import Lighthouse.Display
import Lighthouse.Options
import Lighthouse.Protocol (InputEvent (..), Input (..))
import Lighthouse.Utils.Color
import Lighthouse.Utils.Logging
import System.Environment (getEnv)

-- TODO: Use Data.Vector for efficient random access?

newtype Piece = Piece Int
data Board = Board [[Maybe Piece]]

boardWidth :: Int
boardWidth = 4

boardHeight :: Int
boardHeight = 4

pieceAt :: Int -> Int -> Board -> Maybe Piece
pieceAt x y (Board b)
  | x >= 0 && x < boardWidth && y >= 0 && y < boardHeight = ((b !! y) !! x)
  | otherwise                                             = Nothing

pieceColor :: Piece -> Color
pieceColor (Piece 2)  = white
pieceColor (Piece 4)  = Color 255 231 150 -- light yellow
pieceColor (Piece 8)  = Color 255 144 25  -- orange
pieceColor (Piece 16) = Color 189 88 0    -- darker orange
pieceColor (Piece 32) = Color 255 88 66   -- light red
pieceColor (Piece 64) = Color 171 20 0    -- dark red
pieceColor (Piece _)  = yellow

sampleBoard :: Board
sampleBoard = Board
  [ [Nothing, Nothing, Nothing, Just (Piece 2)]
  , [Just (Piece 2), Just (Piece 4), Nothing, Just (Piece 2)]
  , [Nothing, Just (Piece 8), Just (Piece 16), Just (Piece 16)]
  , [Nothing, Nothing, Nothing, Just (Piece 2)]
  , [Nothing, Nothing, Nothing, Just (Piece 64)]
  ]

boardToDisplay :: Board -> Display
boardToDisplay b = generateDisplay pixAt
  where pixAt x y = maybe black pieceColor $ pieceAt ((x - 4) `div` 5) ((y - 1) `div` 3) b

app :: Listener Board
app = mempty
  { onConnect = do
      logInfo "app" "Connected!"

      -- Send initial board
      board <- getUserState
      sendDisplay $ boardToDisplay board

      -- Request input events
      requestStream

  , onInput = \e -> do
      -- Handle key down events
      when (keIsDown e) $ do
        case keInput e of
          KeyInput 37 -> do
            logInfo "app" "Pressed left"
          KeyInput 38 -> do
            logInfo "app" "Pressed up"
          KeyInput 39 -> do
            logInfo "app" "Pressed right"
          KeyInput 40 -> do
            logInfo "app" "Pressed down"
          _           -> return ()

        -- Update board
        board <- getUserState
        sendDisplay $ boardToDisplay board
  }

main :: IO ()
main = do
  username <- T.pack <$> getEnv "LIGHTHOUSE_USERNAME"
  token    <- T.pack <$> getEnv "LIGHTHOUSE_TOKEN"
  let opts  = Options { optAuthentication = Authentication { authUsername = username, authToken = token }
                      , optLogHandler = simpleLogHandler infoLevel
                      , optInitialState = sampleBoard
                      }

  runLighthouseApp app opts
