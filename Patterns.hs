{-# LANGUAGE MultiWayIf, DeriveDataTypeable #-}

module Patterns
  ( Microseconds
  , PixFunc
  , initBlink
  , initTwinkle
  , initBlinkTwo
  ) where

import Data.Dynamic
import Data.Fixed
import Data.Int
import Data.Word
import System.Random

import Opc

type Microseconds = Int64
type PixFunc = Microseconds -> Dynamic -> (Pixel, Dynamic)

data ColorBlink =
  ColorBlink
  { cbNextTime :: Microseconds
  , cbRand :: StdGen
  , cbCurrentPixel :: Pixel
  } deriving (Typeable, Show)

-- https://en.wikipedia.org/wiki/HSL_and_HSV#From_HSV
fromHSB :: Double -> Double -> Double -> Pixel
fromHSB h s v =
  let c = v * s
      h' = h / 60
      x = c * (1 - abs (h' `mod'` 2 - 1))
      (r1, g1, b1) = if | h' < 1 -> (c, x, 0)
                        | h' < 2 -> (x, c, 0)
                        | h' < 3 -> (0, c, x)
                        | h' < 4 -> (0, x, c)
                        | h' < 5 -> (x, 0, c)
                        | otherwise -> (c, 0, x)
      m = v - c
      (r, g, b) = (r1 + m, g1 + m, b1 + m)
      mk255 n = round $ n * 255
  in Pixel (mk255 r) (mk255 g) (mk255 b)

colorBlink :: Microseconds -> Microseconds -> PixFunc
colorBlink minBlink maxBlink us dyn =
  let cb = fromDyn dyn $ error "fromDyn failed"
      cb' = if us < cbNextTime cb
            then cb
            else let (inc, g) = randomR (minBlink, maxBlink) (cbRand cb)
                     (hue, g') = randomR (0, 360) g
                     nt = inc + cbNextTime cb
                 in ColorBlink
                    { cbNextTime = nt
                    , cbRand = g'
                    , cbCurrentPixel = fromHSB hue 1.0 1.0
                    }
  in (cbCurrentPixel cb', toDyn cb')

initBlink :: Int
           -> StdGen
           -> Microseconds
           -> Microseconds
           -> Microseconds
           -> [(Dynamic, PixFunc)]
initBlink 0 _ _ _ _ = []
initBlink n g now minBlink maxBlink =
  (dyn, colorBlink minBlink maxBlink) : initBlink (n - 1) g2 now minBlink maxBlink
  where (g1, g2) = split g
        cb = ColorBlink
             { cbNextTime = now
             , cbRand = g1
             , cbCurrentPixel = Pixel 0 0 0
             }
        dyn = toDyn cb

data Twinkle =
  Twinkle
  { twRand :: StdGen
  , twNextTwinkle :: Microseconds
  , twPrevTwinkle :: Microseconds
  } deriving (Typeable, Show)

hump :: Double -> Double
hump x = sin (pi * min x 1)

mix' :: Double -> Word8 -> Word8 -> Word8
mix' rat bg fg =
  let bg' = fromIntegral bg
      fg' = fromIntegral fg
      x = bg' * (1 - rat) + fg' * rat
  in round x

mix :: Double -> Pixel -> Pixel -> Pixel
mix rat (Pixel r1 g1 b1) (Pixel r2 g2 b2) = Pixel (mix' rat r1 r2) (mix' rat g1 g2) (mix' rat b1 b2)

twinkleTime :: Microseconds
twinkleTime = 250000

twinkle :: Pixel -> Pixel -> PixFunc
twinkle bg fg us dyn =
  let tw = fromDyn dyn $ error "fromDyn failed"
      tw' = if us < twNextTwinkle tw
            then tw
            else let (inc, g) = randomR (500000, 3000000) (twRand tw)
                 in Twinkle
                    { twRand = g
                    , twNextTwinkle = inc + twNextTwinkle tw
                    , twPrevTwinkle = twNextTwinkle tw
                    }
      t = us - twPrevTwinkle tw'
      n = fromIntegral t / fromIntegral twinkleTime
      p = mix (hump n) bg fg
  in (p, toDyn tw')

initTwinkle :: Int
            -> StdGen
            -> Microseconds
            -> Pixel
            -> Pixel
            -> [(Dynamic, PixFunc)]
initTwinkle 0 _ _ _ _ = []
initTwinkle n g now bg fg =
  (dyn, (twinkle bg fg)) : initTwinkle (n - 1) g2 now bg fg
  where (inc, g') = randomR (0, 3000000) g
        (g1, g2) = split g'
        tw = Twinkle
             { twRand = g1
             , twNextTwinkle = now + inc
             , twPrevTwinkle = now - twinkleTime
             }
        dyn = toDyn tw

data BlinkTwo =
  BlinkTwo
  { btRand :: StdGen
  , btNextBlink :: Microseconds
  , btIndex :: Int
  } deriving (Typeable, Show)

blinkTwo :: Pixel -> Pixel -> PixFunc
blinkTwo c1 c2 us dyn =
  let bt = fromDyn dyn $ error "fromDyn failed"
      bt' = if us < btNextBlink bt
            then bt
            else let (inc, g) = randomR (500000, 3000000) (btRand bt)
                 in BlinkTwo
                    { btRand = g
                    , btNextBlink = inc + btNextBlink bt
                    , btIndex = abs (btIndex bt - 1)
                    }
      c = [c1, c2] !! btIndex bt'
  in (c, toDyn bt')

initBlinkTwo :: Int
             -> StdGen
             -> Microseconds
             -> Pixel
             -> Pixel
             -> [(Dynamic, PixFunc)]
initBlinkTwo 0 _ _ _ _ = []
initBlinkTwo n g now c1 c2 =
  (dyn, (blinkTwo c1 c2)) : initBlinkTwo (n - 1) g2 now c1 c2
  where (inc, g') = randomR (0, 3000000) g
        (idx, g'') = randomR (0, 1) g'
        (g1, g2) = split g''
        bt = BlinkTwo
             { btRand = g1
             , btNextBlink = now + inc
             , btIndex = idx
             }
        dyn = toDyn bt
