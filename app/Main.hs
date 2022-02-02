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

newtype Tile = Tile Int
newtype Board = Board [[Maybe Tile]]
data Dir = DirLeft | DirRight | DirUp | DirDown

-- | The (constant) board width.
boardWidth :: Int
boardWidth = 4

-- | The (constant) board height.
boardHeight :: Int
boardHeight = 4

-- | Fetches the tile at the given position.
tileAt :: Int -> Int -> Board -> Maybe Tile
tileAt x y (Board rs)
  | x >= 0 && x < boardWidth && y >= 0 && y < boardHeight = ((rs !! y) !! x)
  | otherwise                                             = Nothing

-- | Generates a board from a function that maps positions to tiles.
generateBoard :: (Int -> Int -> Maybe Tile) -> Board
generateBoard gen = Board $ (\y -> (\x -> gen x y) <$> [0..(boardWidth - 1)]) <$> [0..(boardHeight - 1)]

-- TODO: Add optimization rule transpose . transpose = id?

-- | Transposes the board.
transpose :: Board -> Board
transpose b = generateBoard $ \x y -> tileAt y x b

-- | Flips the board vertically.
flipV :: Board -> Board
flipV (Board rs) = Board $ reverse rs

-- | Flips the board horizontally.
flipH :: Board -> Board
flipH = transpose . flipV . transpose

-- | Rotates the board left by 90 degrees.
rotL :: Board -> Board
rotL = flipV . transpose

-- | Rotates the board right by 90 degrees.
rotR :: Board -> Board
rotR = flipH . transpose

step :: Dir -> Board -> Board
step dir = case dir of
    DirLeft  -> stepLeft
    DirRight -> stepRight
    DirUp    -> rotR . stepLeft . rotL
    DirDown  -> rotR . stepRight . rotL
  -- TODO: Merge tiles
  where stepLeft (Board rs) = Board $ updateRow [] <$> rs
        stepRight           = flipH . stepLeft . flipH
        updateRow acc []             = acc
        updateRow acc (Nothing : ps) = updateRow (Nothing : acc) ps
        updateRow acc (Just p : ps)  = Just p : updateRow acc ps

tileColor :: Tile -> Color
tileColor (Tile 2)  = white
tileColor (Tile 4)  = Color 255 231 150 -- light yellow
tileColor (Tile 8)  = Color 255 144 25  -- orange
tileColor (Tile 16) = Color 189 88 0    -- darker orange
tileColor (Tile 32) = Color 255 88 66   -- light red
tileColor (Tile 64) = Color 171 20 0    -- dark red
tileColor (Tile _)  = yellow

sampleBoard :: Board
sampleBoard = Board
  [ [Nothing, Nothing, Nothing, Just (Tile 2)]
  , [Just (Tile 2), Just (Tile 4), Nothing, Just (Tile 2)]
  , [Nothing, Just (Tile 8), Just (Tile 16), Just (Tile 16)]
  , [Nothing, Nothing, Nothing, Just (Tile 2)]
  , [Nothing, Nothing, Nothing, Just (Tile 64)]
  ]

boardToDisplay :: Board -> Display
boardToDisplay b = generateDisplay pixAt
  where pixAt x y = maybe black tileColor $ tileAt ((x - 4) `div` 5) ((y - 1) `div` 3) b

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
