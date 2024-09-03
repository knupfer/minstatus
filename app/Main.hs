{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import System.Process
import System.IO
import Data.ByteString.Builder
import Control.Monad
import Data.Time
import Data.List

data State = State {time :: Time, sound :: Sound, battery :: Battery}
data Time  = Time {year :: Int, month :: Int, day :: Int, hour :: Int, minute :: Int}
data Sound = Sound {volume :: Int, muted :: Bool}
data Battery = Battery {capacity :: Int, charging :: Bool}

main :: IO ()
main = do
  zone <- getTimeZone =<< getCurrentTime
  hSetBuffering stdout LineBuffering
  forever $ do
    t <- getTime zone
    s <- getSound
    b <- getBattery
    hPutBuilder stdout . prettyState $ State t s b
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

getBattery :: IO Battery
getBattery = do
  c <- readFile "/sys/class/power_supply/BAT0/capacity"
  s <- readFile "/sys/class/power_supply/BAT0/status"
  pure $ Battery (read c) (not $ "Discharging" `isPrefixOf` s)

prettyTime :: Time -> Builder
prettyTime Time{..} = intDec year <> char7 '.' <> intDec month <> char7 '.' <> intDec day <> char7 ' ' <> intDec hour <> char7 ':' <> padNum '0' 2 minute

padNum :: Char -> Int -> Int -> Builder
padNum c len i | len > 1 && 10^(len-1) > i = char7 c <> padNum c (len-1) i
padNum _ _ i = intDec i

prettySound :: Sound -> Builder
prettySound Sound{..} = text <> num
                      where num = padNum ' ' 3 volume <> char7 '%'
                            text | muted     = " Muted:"
                                 | otherwise = "Volume:"

prettyBattery :: Battery -> Builder
prettyBattery Battery{..} = text <> num
  where num = padNum ' ' 3 capacity <> char7 '%'
        text | charging  = "Charging:"
             | otherwise = "Capacity:"

prettyState :: State -> Builder
prettyState (State{..}) = prettyBattery battery <> "  │  " <>  prettySound sound <> "  │  " <> prettyTime time <> char7 '\n'


