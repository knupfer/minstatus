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

import qualified Sound.ALSA.Mixer as A

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
getSound = A.withMixer "default" $ \mixer -> do
    Just control <- A.getControlByName mixer "Master"
    let Just playbackSwitch = A.playback $ A.switch control
        Just playbackVolume = A.playback $ A.volume control
    Just sw <- A.getChannel A.FrontLeft playbackSwitch
    (_, hi) <- A.getRange playbackVolume
    Just vol <- A.getChannel A.FrontLeft $ A.value $ playbackVolume
    pure $ Sound (fromIntegral $ vol*100 `div` hi) (not sw)

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
monitorCharging zone mv = withCreateProcess (proc "udevadm" ["monitor", "-k", "-s", "usb_power_delivery", "-s", "nvme"]) {std_out = CreatePipe} $ \_ (Just hout) _ _ -> do
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
monitorSound mv = withCreateProcess (proc "alsactl" ["monitor"]) {std_out = CreatePipe} $ \_ (Just hout) _ _ ->
  forever $ do
    void $ B8.hGetLine hout
    x <- getSound
    putMVar mv (\s -> s{sound = x})

getCapacity :: IO B8.ByteString
getCapacity = B8.dropEnd 1 <$> B8.readFile "/sys/class/power_supply/BAT0/capacity"

getStatus :: IO Bool
getStatus = not . isPrefixOf "Discharging" <$> readFile "/sys/class/power_supply/BAT0/status"

getBattery :: IO Battery
getBattery = Battery <$> getCapacity <*> getStatus

{-# INLINE prettyTime #-}
prettyTime :: Time -> Builder
prettyTime Time{..} = intDec year <> char7 '.' <> intDec month <> char7 '.' <> intDec day <> char7 ' ' <> intDec hour <> char7 ':' <> pad minute
  where {-# INLINE pad #-}
        pad i | i < 10 = char7 '0' <> intDec i
        pad i = intDec i

{-# INLINE prettySound #-}
prettySound :: Sound -> Builder
prettySound Sound{..} = text <> pad (intDec volume) <> char7 '%'
  where text | muted     = byteString " Muted:"
             | otherwise = byteString "Volume:"
        pad | volume < 10  = (char7 ' ' <>) . (char7 ' ' <>)
            | volume < 100 = (char7 ' ' <>)
            | otherwise = id

{-# INLINE prettyBattery #-}
prettyBattery :: Battery -> Builder
prettyBattery Battery{..} = text <> pad num
  where num = byteString capacity <> char7 '%'
        text | charging  = byteString "Charging:"
             | otherwise = byteString "Capacity:"
        pad = case B8.length capacity of
          1 -> (char7 ' ' <>) . (char7 ' ' <>)
          2 -> (char7 ' ' <>)
          _ -> id

prettyState :: State -> Builder
prettyState (State{..}) = prettySound sound <> separator <> prettyBattery battery <> separator <> prettyTime time <> char7 '\n'

separator :: Builder
separator = "  │  "
