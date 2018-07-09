{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Uuid
  ( Uuid(..)
  , uuidToInteger
  , integerToUuid
  , uuidPlus
  , uuidMinus
  , uuidAnd
  , uuidOr
  , uuidXor
  , uuidShift
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Bits
import Data.Hashable (Hashable)
import Data.Semigroup
import Data.Word (Word64)
import GHC.Generics (Generic)
import Web.PathPieces (PathPiece(..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TBI
import qualified Data.Text.Read as TR

data Uuid = Uuid
  { uuidA :: {-# UNPACK #-} !Word64
  , uuidB :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show, Read, Generic)

instance Hashable Uuid
instance NFData Uuid

instance ToJSON Uuid where
  toJSON = Aeson.String . toPathPiece

instance FromJSON Uuid where
  parseJSON = Aeson.withText "Uuid" $ \t -> case fromPathPiece t of
    Nothing -> fail "could not parse uuid"
    Just uuid -> return uuid

instance PathPiece Uuid where
  toPathPiece (Uuid a b) = LT.toStrict (TB.toLazyText (TBI.decimal a <> TB.singleton '-' <> TBI.decimal b))
  fromPathPiece t = case T.splitOn (T.singleton '-') t of
    [t1,t2] -> case TR.decimal t1 of
      Left _ -> Nothing
      Right (w1,t1') -> if T.null t1'
        then case TR.decimal t2 of
          Left _ -> Nothing
          Right (w2,t2') -> if T.null t2'
            then Just (Uuid w1 w2)
            else Nothing
        else Nothing
    _ -> Nothing

uuidToInteger :: Uuid -> Integer
uuidToInteger (Uuid lo hi) =
  toInteger lo + (2^(finiteBitSize lo)) * toInteger hi

integerToUuid :: Integer -> Uuid
integerToUuid x =
  let (h,l) =  x `quotRem` (2^(finiteBitSize lo))
      (lo,hi) = (fromInteger l, fromInteger h)
   in Uuid lo hi

uuidPlus :: Uuid -> Uuid -> Uuid
uuidPlus (Uuid alo ahi) (Uuid blo bhi) = Uuid lo' hi'
  where
  lo' = alo + blo
  hi' = ahi + bhi + (if lo' < alo then 1 else 0)

uuidMinus :: Uuid -> Uuid -> Uuid
uuidMinus (Uuid alo ahi) (Uuid blo bhi) = Uuid lo' hi'
  where
  lo' = alo - blo
  hi' = ahi - bhi - if lo' > alo then 1 else 0

uuidAnd :: Uuid -> Uuid -> Uuid
uuidAnd (Uuid alo ahi) (Uuid blo bhi) = Uuid lo' hi'
  where
  lo' = alo .&. blo
  hi' = ahi .&. bhi

uuidOr :: Uuid -> Uuid -> Uuid
uuidOr (Uuid alo ahi) (Uuid blo bhi) = Uuid lo' hi'
  where
  lo' = alo .|. blo
  hi' = ahi .|. bhi

uuidXor :: Uuid -> Uuid -> Uuid
uuidXor (Uuid alo ahi) (Uuid blo bhi) = Uuid lo' hi'
  where
  lo' = alo `xor` blo
  hi' = ahi `xor` bhi

uuidShift :: Uuid -> Int -> Uuid
uuidShift (Uuid lo hi) x = if x >= 0
  then
    if loSize <= hiSize
      then
        Uuid (shift lo x)
             (shift hi x .|. (shift lo (x - (finiteBitSize lo))))
      else
        Uuid (shift lo x)
             (shift hi x .|. (shift lo (x - (finiteBitSize lo))))
  else
    if loSize <= hiSize
      then
        Uuid (shift lo x .|. (shift hi (x + (finiteBitSize lo))))
             (shift hi x)
      else
        Uuid (shift lo x .|. (shift hi (x + (finiteBitSize lo))))
             (shift hi x)
  where
    loSize = finiteBitSize lo
    hiSize = finiteBitSize hi


instance Bits Uuid where
  (.&.) = uuidAnd
  (.|.) = uuidOr
  xor = uuidXor
  shift = uuidShift
  complement (Uuid a b) = Uuid (complement a) (complement b)
  bit = bitDefault
  testBit = testBitDefault
  popCount = popCountDefault
  isSigned _ = False
  bitSize _ = 128
  bitSizeMaybe _ = Just 128
  x `rotate` i = case compare i 0 of
    LT -> (x `uuidShift` i) .|. (x `uuidShift` (i + 64))
    EQ -> x
    GT -> (x `uuidShift` i) .|. (x `uuidShift` (i - 64))

instance FiniteBits Uuid where
  finiteBitSize _ = 128

instance Num Uuid where
  (+) = uuidPlus
  (-) = uuidMinus
  (*) a b = go 0 0
    where
    go i r
      | i == finiteBitSize r = r
      | testBit b i = go (i+1) (r + (a `shiftL` i))
      | otherwise = go (i+1) r
  negate = id
  abs = id
  signum (Uuid a b) = if a == 0 && b == 0 then 0 else 1
  fromInteger = integerToUuid

instance Enum Uuid where
  toEnum i = Uuid (toEnum i) 0
  fromEnum (Uuid l _) = fromEnum l
  pred (Uuid 0 h) = Uuid maxBound (pred h)
  pred (Uuid l h) = Uuid (pred l) h
  succ (Uuid l h) = if l == maxBound then Uuid 0 (succ h) else Uuid (succ l) h

instance Real Uuid where
  toRational w = toRational (fromIntegral w :: Integer)

instance Integral Uuid where
  toInteger = uuidToInteger
  quotRem a b =
          let r = a - q*b
              q = go 0 (finiteBitSize a) 0
          in (q,r)
    where
    -- Trivial long division
    go t 0 v = if v >= b then t+1 else t
    go t i v
           | v >= b    = go (setBit t i) i' v2
           | otherwise = go t i' v1
      where i' = i - 1
            newBit = if (testBit a i') then 1 else 0
            v1 = (v `shiftL` 1) .|. newBit
            v2 = ((v - b) `shiftL` 1) .|. newBit
  divMod = quotRem
