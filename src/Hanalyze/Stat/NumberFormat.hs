{-# LANGUAGE OverloadedStrings #-}
-- | Number-formatting helpers for reports and CLI output.
--
-- A single function chooses fixed-point or exponential notation based on
-- magnitude:
--
-- >>> fmtNum 0
-- "0.00"
-- >>> fmtNum 0.91
-- "0.91"
-- >>> fmtNum 12.34
-- "12.34"
-- >>> fmtNum 1.10e13
-- "1.10E+13"
-- >>> fmtNum 3.057e-24
-- "3.06E-24"
-- >>> fmtNum 1234.5
-- "1.23E+03"
--
-- Threshold: values with @|x|@ outside @[0.01, 999]@ use exponential
-- notation; inside the range, two decimal digits. Zero and non-finite
-- values (@NaN@ / @Infinity@) get dedicated fallbacks.
module Hanalyze.Stat.NumberFormat
  ( fmtNum
  , fmtNumT
  , fmtNumWith
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

-- | Default-threshold numeric formatting (String).
fmtNum :: Double -> String
fmtNum = fmtNumWith 0.01 999

-- | Default-threshold numeric formatting (Text).
fmtNumT :: Double -> Text
fmtNumT = T.pack . fmtNum

-- | Custom-threshold formatter.
--
-- @fmtNumWith lo hi x@ formats @x@ with @\"%.2f\"@ when @|x|@ is inside
-- @[lo, hi]@, otherwise @\"%.2E\"@. Zero, @NaN@ and @Infinity@ get
-- dedicated representations.
fmtNumWith :: Double -> Double -> Double -> String
fmtNumWith lo hi x
  | isNaN x         = "NaN"
  | isInfinite x    = if x > 0 then "+Inf" else "-Inf"
  | x == 0          = "0.00"
  | a >= hi || a < lo = formatSci x
  | otherwise       = printf "%.2f" x
  where
    a = abs x

-- | "M.MME+NN" / "M.MME-NN" 形式の指数表記。
-- printf "%.2E" は実装依存で "+" の有無が変わるため、自前で組む。
formatSci :: Double -> String
formatSci x =
  let s = if x < 0 then "-" else "" :: String
      a = abs x
      e = floor (logBase 10 a) :: Int
      m = a / (10 ** fromIntegral e)
      (m', e') = if m >= 10 then (m / 10, e + 1) else (m, e)
      sign = if e' >= 0 then "+" else "-" :: String
  in printf "%s%.2fE%s%d" s m' sign (abs e' :: Int)
