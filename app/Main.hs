{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (guard, void)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
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
newtype Board = Board [[Maybe Piece]]
data Dir = DirLeft | DirRight | DirUp | DirDown

-- | The (constant) board width.
boardWidth :: Int
boardWidth = 4

-- | The (constant) board height.
boardHeight :: Int
boardHeight = 4

-- | Fetches the piece at the given position.
pieceAt :: Int -> Int -> Board -> Maybe Piece
pieceAt x y (Board rs)
  | x >= 0 && x < boardWidth && y >= 0 && y < boardHeight = ((rs !! y) !! x)
  | otherwise                                             = Nothing

-- | Generates a board from a function that maps positions to pieces.
generateBoard :: (Int -> Int -> Maybe Piece) -> Board
generateBoard gen = Board $ (\y -> (\x -> gen x y) <$> [0..(boardWidth - 1)]) <$> [0..(boardHeight - 1)]

-- | Transposes the board.
transposeBoard :: Board -> Board
transposeBoard b = generateBoard $ \x y -> pieceAt y x b

-- | Flips the board vertically.
flipBoard :: Board -> Board
flipBoard (Board rs) = Board $ reverse rs

step :: Dir -> Board -> Board
step dir (Board rs) = Board $ updateRow [] <$> rs
  -- TODO: Merge tiles
  where updateRow acc []            = acc
        updateRow acc (Nothing : ps) = updateRow (Nothing : acc) ps
        updateRow acc (Just p : ps)  = Just p : updateRow acc ps

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

  , onInput = \e -> void . runMaybeT $ do
      -- Handle only key down events
      guard (keIsDown e)

      -- Convert key input to direction
      dir <- MaybeT . return $ case keInput e of
        KeyInput 37 -> Just DirLeft
        KeyInput 38 -> Just DirUp
        KeyInput 39 -> Just DirRight
        KeyInput 40 -> Just DirDown
        _           -> Nothing

      -- Update board and send it
      lift $ do
        modifyUserState (step dir)
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
