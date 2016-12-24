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
          rPix (zip dyn' funcs) goal'

gen :: StdGen
gen = mkStdGen 12345

main = do
  ai <- getAddrInfo Nothing (Just "127.0.0.1" {- "localhost" -}) (Just "7890")
  let addr = addrAddress $ head ai
      fam (SockAddrInet {}) = AF_INET
      fam (SockAddrInet6 {}) = AF_INET6
      fam sa = error $ "Unexpected socket family: " ++ show sa
  s <- socket (fam addr) Stream defaultProtocol
  connect s addr
  now <- currentMicros
  let pix = initBlink nLights gen now 1000000 10000000
  runPixels s pix
