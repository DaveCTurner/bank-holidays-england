{-|

Calculation of bank holidays in England and Wales, using the rules that have
been in place since 1978, and including all exceptions to the rules in the
years 1995 to 2014. I do not know of any exceptions from 1978 until 1995, so
the calculations may be correct for those years too. Calculations for future
dates are predictions which may be rendered false if exceptions to the rules
are announced.

There are normally 8 bank holidays in England and Wales:

  * New Year's Day
  * Good Friday
  * Easter Monday
  * May Day
  * Spring Bank Holiday
  * Summer Bank Holiday
  * Christmas Day
  * Boxing Day

The rules for determining the precise date of each of these in any given year
are a little involved, since holidays may be moved to avoid falling on a
weekend:

  * The New Year's Day holiday is the 1st of January, or the following Monday if
    the 1st is a weekend.
  * Good Friday and Easter Monday are the Friday and Monday either side of
    Easter Sunday (as calculated by the Gregorian method).
  * May Day is the first Monday in May.
  * The Spring Bank Holiday is the last Monday in May.
  * The Summer Bank Holiday is the last Monday in August.
  * Christmas Day is the 25th of December unless that's a weekend,
    in which case it's the 27th.
  * Boxing Day is the 26th of December unless that's a weekend,
    in which case it's the 28th.

Exceptions may be made to these rules on a year-by-year basis.

This package is a reasonably efficient (constant-time) implementation of these
rules.

-}

module Data.Time.Calendar.BankHoliday.EnglandAndWales 
  ( bankHolidays
  , isBankHoliday
  , countBankHolidays
  ) where

import Data.Time
import Data.Time.Calendar.Easter
import qualified Data.Set as S

{-| List the bank holidays for the given year, in ascending order. Bank
holidays never fall on a weekend. -}
bankHolidays :: Integer -> [Day]
bankHolidays yy = standardHolidays ++ if yy == 1999 then [dec 31] else []

  where
  [jan, apr, may, jun, sep, dec] = map (fromGregorian yy)
    [1,   4,   5,   6,   9,  12]

  wd mm dd = toModifiedJulianDay (mm dd) `mod` 7

  standardHolidays = newYearsDay ++ easter ++ mayDay ++ spring ++ [weekBefore $ firstMondayIn sep] ++ christmas

  mayDay = case yy of
    2011 -> [apr 29, may 2]
    1995 -> [may 8]
    _    -> [firstMondayIn may]

  spring = case yy of
    2002 -> [jun 3, jun 4]
    2012 -> [jun 4, jun 5]
    _    -> [weekBefore $ firstMondayIn jun]

  newYearsDay = case wd jan 1 of
    3 {- Sat -} -> [jan 3]
    4 {- Sun -} -> [jan 2]
    _           -> [jan 1]

  easter = let easterSunday = gregorianEaster yy in [addDays (-2) easterSunday, addDays 1 easterSunday]

  christmas = case wd dec 25 of
    2 {- Fri -} -> [dec 25, dec 28]
    3 {- Sat -} -> [dec 27, dec 28]
    4 {- Sun -} -> [dec 26, dec 27]
    _           -> [dec 25, dec 26]

  firstMondayIn mm = addDays (negate $ wd mm 02) (mm 07)
  weekBefore = addDays (-7)

{-| Returns whether a day is a bank holiday. -}
isBankHoliday :: Day -> Bool
isBankHoliday d = (not $ S.member d skipped) && (S.member d extras || isStandardHoliday)
  where
  skipped = S.fromList  [ fromGregorian 1995 05 1
                        , fromGregorian 2002 05 27
                        , fromGregorian 2012 05 28
                        ]
  extras  = S.fromList  [ fromGregorian 1995 05 08
                        , fromGregorian 1999 12 31
                        , fromGregorian 2002 06 03
                        , fromGregorian 2002 06 04
                        , fromGregorian 2011 04 29
                        , fromGregorian 2012 06 04
                        , fromGregorian 2012 06 05
                        ]
  (yy,mm,dd)   = toGregorian d
  dayOfWeek    = mod (toModifiedJulianDay d) 7
  isMonday     = dayOfWeek == 5
  isWeekend    = dayOfWeek `elem` [3,4]
  easterSunday = gregorianEaster yy

  isStandardHoliday
    | isWeekend = False
    | isMonday  =  (mm == 1  &&  dd <= 3)
                || (mm == 5  && (dd <= 7 || 31-7 < dd))
                || (mm == 8  &&             31-7 < dd)
                || (mm == 12 && 25 <= dd && dd < 29)
                || d == addDays 1 easterSunday
    | otherwise =  (mm,dd) == (1,1)
                || (mm == 12 && 25 <= dd && (dd < 27 || (dayOfWeek == 6 && dd < 29)))
                || d == addDays (-2) easterSunday

countDaysWrapper :: (Day -> Day -> Integer) -> Day -> Day -> Integer
countDaysWrapper f d0 d1 = case compare d0 d1 of
  LT -> f d0 d1
  EQ -> 0
  GT -> negate $ f d1 d0

{-| Count the number of bank holidays between two 'Day's.

If @d0 <= d1@ then @countBankHolidays d0 d1@ is the number of 'Day's @d@ for
which @isBankHoliday d && d0 <= d && d < d1@. Note the count includes @d0@ but
excludes @d1@.

Additionally, @countBankHolidays d0 d1 == negate (countBankHolidays d1 d0)@ and
@countBankHolidays d0 d2 == countBankHolidays d0 d1 + countBankHolidays d1 d2@.

 -}
countBankHolidays :: Day -> Day -> Integer
countBankHolidays = countDaysWrapper go
  where
  go d0 d1 =
    if y0 == y1
      then fromIntegral $ length $ takeWhile (<d1) $ dropWhile (<d0) $ bankHolidays y0
      else fromIntegral (length (takeWhile (<d1) $ bankHolidays y1)
                       - length (takeWhile (<d0) $ bankHolidays y0)
                       + length (dropWhile (<y0) $ takeWhile (<y1) [1999,2002,2011,2012]))
         + 8 * (y1 - y0)
    where
    (y0,_,_) = toGregorian d0
    (y1,_,_) = toGregorian d1
