import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Dynamic
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Random
import Time.System
import Time.Types

import Opc
import Patterns
import WiringPi

buttonPin = 2

nLights = 512                   -- max number of lights on a FadeCandy

framesPerSecond = 60

black = Pixel 0 0 0
blue = Pixel 0 0 255
cyan = Pixel 0 255 255
green = Pixel 0 255 0
magenta = Pixel 255 0 255
orange = Pixel 255 128 0
purple = Pixel 192 0 255
red = Pixel 255 0 0
white = Pixel 255 255 255
yellow = Pixel 255 255 0

currentMicros :: IO Microseconds
currentMicros = do
  now <- timeCurrentP
  let (ElapsedP (Elapsed (Seconds s)) (NanoSeconds ns)) = now
  return $ s * 1000000 + ns `div` 1000

sendPixels :: Socket -> [Pixel] -> IO ()
sendPixels sock pix = do
  let frame = SetColors 0 pix
      bs = encode frame
  sendMany sock (L.toChunks bs)

runPixels :: Socket -> [(Dynamic, PixFunc)] -> IO ()
runPixels sock pix1 = currentMicros >>= rPix pix1
  where rp goal (dyn, func) = func goal dyn
        rPix pix goal = do
          let (colors, dyn') = unzip $ map (rp goal) pix
          now <- currentMicros
          when (goal > now) $ threadDelay $ fromIntegral $ goal - now
          sendPixels sock colors
          let goal' = goal + 1000000 `div` framesPerSecond
              (_ , funcs) = unzip pix
          pressed <- buttonPressed
          unless pressed $ rPix (zip dyn' funcs) goal'

buttonPressed :: IO Bool
buttonPressed = do
  button <- digitalRead buttonPin
  case button of
    HIGH -> return False
    LOW -> do
      buttonWait
      return True
  where buttonWait = do
          threadDelay $ 1000000 `div` 20
          button <- digitalRead buttonPin
          when (button == LOW) buttonWait

gen :: StdGen
gen = mkStdGen 12345

main = do
  -- set up GPIO for button
  wiringPiSetup
  pinMode buttonPin INPUT
  pullUpDnControl buttonPin PUD_UP

  -- connect to Open Pixel Control server
  ai <- getAddrInfo Nothing (Just "127.0.0.1" {- "localhost" -}) (Just "7890")
  let addr = addrAddress $ head ai
      fam (SockAddrInet {}) = AF_INET
      fam (SockAddrInet6 {}) = AF_INET6
      fam sa = error $ "Unexpected socket family: " ++ show sa
  s <- socket (fam addr) Stream defaultProtocol
  connect s addr

  -- cycle through different patterns as button is pressed
  forever $ do
    now1 <- currentMicros
    let pix1 = initBlink nLights gen now1 1000000 10000000
    runPixels s pix1

    now2 <- currentMicros
    let pix2 = initTwinkle nLights gen now2 blue white
    runPixels s pix2

    now3 <- currentMicros
    let pix3 = initBlinkTwo nLights gen now3 red green
    runPixels s pix3
