import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Environment
import System.Exit

import Opc

nLights = 512                   -- max number of lights on a FadeCandy

mkFrame :: Pixel -> Frame
mkFrame c = SetColors 0 $ replicate nLights c

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

sendFrame :: Socket -> Frame -> IO ()
sendFrame s f = sendMany s $ L.toChunks $ encode f

main = do
  args <- getArgs
  color <- case args of
             []          -> return white
             ["black"]   -> return black
             ["blue"]    -> return blue
             ["cyan"]    -> return cyan
             ["green"]   -> return green
             ["magenta"] -> return magenta
             ["orange"]  -> return orange
             ["purple"]  -> return purple
             ["red"]     -> return red
             ["white"]   -> return white
             ["yellow"]  -> return yellow
             [r, g, b]   -> return $ Pixel (read r) (read g) (read b)
             _ -> do
               putStrLn "Argument must be one of:"
               putStrLn "    black, blue, cyan, green, magenta, orange, purple, red, white, yellow"
               putStrLn "Or three integer arguments between 0-255"
               exitFailure
  ai <- getAddrInfo Nothing (Just "127.0.0.1" {- "localhost" -}) (Just "7890")
  let addr = addrAddress $ head ai
      fam (SockAddrInet {}) = AF_INET
      fam (SockAddrInet6 {}) = AF_INET6
      fam sa = error $ "Unexpected socket family: " ++ show sa
  s <- socket (fam addr) Stream defaultProtocol
  connect s addr
  let f = mkFrame color
  sendFrame s f
  -- send frame twice to defeat FadeCandy's fade logic; we want immediate results!
  sendFrame s f
