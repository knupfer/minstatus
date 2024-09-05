{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent
import System.Process
import System.IO
import Data.ByteString.Builder

import qualified Data.ByteString.Char8 as B8

import Control.Monad
import Data.Time
import Data.List

data State = State {time :: Time, sound :: Sound, battery :: Battery}
data Time  = Time {year :: Int, month :: Int, day :: Int, hour :: Int, minute :: Int}
data Sound = Sound {volume :: Int, muted :: Bool}
data Battery = Battery {capacity :: B8.ByteString, charging :: Bool}

main :: IO ()
main = do
  zone <- getTimeZone =<< getCurrentTime
  initialState <- State <$> getTime zone <*> getSound <*> getBattery
  updater <- newEmptyMVar :: IO (MVar (State -> State))
  void $ forkIO (monitorCharging zone updater)
  void $ forkIO (monitorCapacity updater)
  void $ forkIO (monitorSound updater)
  void $ forkIO (monitorTime updater)
  hSetBuffering stdout LineBuffering
  let printer state = do
        hPutBuilder stdout $ prettyState state
        f <- takeMVar updater
        let new = f state
        if minute (time new) > 59
          then do
          t <- getTime zone
          printer (new {time = t})
          else
          printer new
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
  pure $ Sound (read . takeWhile (/='%') $ drop 1 vol) (active == "[off]")

monitorTime :: MVar (State -> State) -> IO ()
monitorTime mv = forever $ do
  threadDelay 60000000
  putMVar mv (\s -> s{time = (time s) {minute = succ . minute $ time s}})

monitorCapacity :: MVar (State -> State) -> IO ()
monitorCapacity mv = withFile "/sys/class/power_supply/BAT0/capacity" ReadMode $ \fh -> forever $ do
  threadDelay 300000000  -- 5 min
  newCapacity <- B8.hGetLine fh
  putMVar mv (\s -> s{battery = (battery s) {capacity = newCapacity}})
  hSeek fh AbsoluteSeek 0

monitorCharging :: TimeZone -> MVar (State -> State) -> IO ()
monitorCharging zone mv = do
  (_, Just hout, _, _) <- createProcess (proc "udevadm" ["monitor", "-k", "-s", "usb_power_delivery", "-s", "nvme"]) {std_out = CreatePipe}
  xs <- words <$> hGetContents hout
  forM_ xs $ \case
    "add" -> update True
    "remove" -> update False
    "change" -> do
      t <- getTime zone
      putMVar mv (\s -> s{time = t})
    _ -> pure ()
    where update b = putMVar mv (\s -> s{battery = (battery s) {charging = b}})

monitorSound :: MVar (State -> State) -> IO ()
monitorSound mv = do
  (_, Just hout, _, _) <- createProcess (proc "alsactl" ["monitor"]) {std_out = CreatePipe}
  forever $ do
    void $ hGetLine hout
    newSound <- getSound
    putMVar mv (\s -> s{sound = newSound})

getCapacity :: IO B8.ByteString
getCapacity = B8.dropEnd 1 <$> B8.readFile "/sys/class/power_supply/BAT0/capacity"

getStatus :: IO Bool
getStatus = not . isPrefixOf "Discharging" <$> readFile "/sys/class/power_supply/BAT0/status"

getBattery :: IO Battery
getBattery = Battery <$> getCapacity <*> getStatus

prettyTime :: Time -> Builder
prettyTime Time{..} = intDec year <> char7 '.' <> intDec month <> char7 '.' <> intDec day <> char7 ' ' <> intDec hour <> char7 ':' <> pad minute
  where {-# INLINE pad #-}
        pad i | i < 10 = char7 '0' <> intDec i
        pad i = intDec i

prettySound :: Sound -> Builder
prettySound Sound{..} = text <> num
  where num = pad volume <> char7 '%'
        text | muted     = " Muted:"
             | otherwise = "Volume:"
        {-# INLINE pad #-}
        pad i | i < 10 = char7 ' ' <> char7 ' ' <> intDec i
              | i < 100 = char7 ' ' <> intDec i
              | otherwise = intDec i

prettyBattery :: Battery -> Builder
prettyBattery Battery{..} = text <> pad num
  where num = byteString capacity <> char7 '%'
        text | charging  = "Charging:"
             | otherwise = "Capacity:"
        pad = case B8.length capacity of
          1 -> ("  " <>)
          2 -> (" " <>)
          _ -> id

prettyState :: State -> Builder
prettyState (State{..}) = prettySound sound <> separator <> prettyBattery battery <> separator <> prettyTime time <> char7 '\n'

separator :: Builder
separator = "  │  "

