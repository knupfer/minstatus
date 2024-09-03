{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import System.Process
import System.IO
import Data.ByteString.Builder
import Control.Monad
import Data.Time

data State = State {time :: Time, sound :: Sound}
data Time = Time {year :: Int, month :: Int, day :: Int, hour :: Int, minute :: Int}
data Sound = Sound {volume :: Int, muted :: Bool}

main :: IO ()
main = do
  zone <- getTimeZone =<< getCurrentTime
  hSetBuffering stdout LineBuffering
  forever $ do
    t <- getTime zone
    s <- getSound
    hPutBuilder stdout . prettyState $ State t s
    threadDelay 1000000

getTime :: TimeZone -> IO Time
getTime zone = do
  now <- getCurrentTime
  let (LocalTime x (TimeOfDay h m _)) = utcToLocalTime zone now
      (y,mo,d) = toGregorian x
  pure $ Time (fromIntegral y) mo d h m

getSound :: IO Sound
getSound = do
  out <- readProcess "amixer" ["sget","Master"] ""
  let [active, _db, vol] = take 3 . reverse . words $ out
  pure $ Sound (read . takeWhile (/='%') $ drop 1 vol) (active == "[on]")

prettyTime :: Time -> Builder
prettyTime Time{..} = intDec year <> char7 '-' <> f month <> char7 '-' <> f day <> char7 'T' <> f hour <> char7 ':' <> f minute
  where f n = let (a,b) = n `divMod ` 10 in intDec a <> intDec b

prettySound :: Sound -> Builder
prettySound Sound{..} | muted = "[mute]" <> char7 ' ' <> vol
                      | otherwise = vol
  where vol = intDec volume <> char7 '%'

prettyState :: State -> Builder
prettyState (State{..}) = prettySound sound <> char7 ' ' <> prettyTime time <> char7 '\n'

  
