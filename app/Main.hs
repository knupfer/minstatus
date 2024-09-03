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
  initialState <- State <$> getTime zone <*> getSound <*> getBattery
  updater <- newEmptyMVar :: IO (MVar (State -> State))
  void $ forkIO (monitorBattery updater)
  void $ forkIO (monitorSound updater)
  void $ forkIO (monitorTime zone updater)
  hSetBuffering stdout LineBuffering
  let printer state = do
        hPutBuilder stdout $ prettyState state
        f <- takeMVar updater
        printer (f state)
  printer initialState

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

monitorTime :: TimeZone -> MVar (State -> State) -> IO ()
monitorTime zone mv = forever $ do
  threadDelay 60000000
  newTime <- getTime zone
  newBattery <- getBattery
  putMVar mv (\s -> s{time = newTime, battery = newBattery})

monitorBattery :: MVar (State -> State) -> IO ()
monitorBattery mv = do
  (_, Just hout, _, _) <- createProcess (proc "udevadm" ["monitor", "-k", "-s", "usb_power_delivery"]) {std_out = CreatePipe}
  forever $ do
    void $ hGetLine hout
    threadDelay 1000000
    newBattery <- getBattery
    putMVar mv (\s -> s{battery = newBattery})

monitorSound :: MVar (State -> State) -> IO ()
monitorSound mv = do
  (_, Just hout, _, _) <- createProcess (proc "alsactl" ["monitor"]) {std_out = CreatePipe}
  forever $ do
    void $ hGetLine hout
    newSound <- getSound
    putMVar mv (\s -> s{sound = newSound})

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
                            text | muted     =  "Muted:"
                                 | otherwise = "Volume:"

prettyBattery :: Battery -> Builder
prettyBattery Battery{..} = text <> num
  where num = padNum ' ' 3 capacity <> char7 '%'
        text | charging  = "Charging:"
             | otherwise = "Capacity:"

prettyState :: State -> Builder
prettyState (State{..}) = prettySound sound <> separator <> prettyBattery battery <> separator <> prettyTime time <> char7 '\n'

separator :: Builder
separator = "  │  "

