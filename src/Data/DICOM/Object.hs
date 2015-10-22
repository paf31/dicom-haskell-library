-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.Object
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-- | Types and smart constructors for DICOM objects and elements.
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Data.DICOM.Object
  ( ElementContent(..)
  , Element(..)
  , SequenceItem(..)
  , Sequence(..)
  , Object(..)

  , readObject
  , readObjectFromFile
  , writeObject
  , writeObjectToFile

  , element
  , sq
  , item
  , object

  , ae
  , as
  , cs
  , da
  , ds
  , dt
  , fl
  , fd
  , is
  , lo
  , lt
  , ob
  , ow
  , pn
  , sh
  , sl
  , ss
  , st
  , tm
  , ui
  , ul
  , un
  , us
  , ut
  ) where

import Prelude hiding (LT)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Data.Bool (bool)
import Data.Int (Int64)
import Data.Monoid (Monoid(..), (<>))
import Data.Foldable (traverse_)
import Data.List (sortBy)
import Data.Function (on)

import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime,defaultTimeLocale)

import Data.DICOM.VL
import Data.DICOM.VR
import Data.DICOM.Tag

import Control.Monad (unless)
import Control.Applicative

import Text.Printf (printf)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

-- Magic constants

dicm :: B.ByteString
dicm = BC.pack "DICM"

-- Data types

data ElementContent
  = BytesContent B.ByteString
  | FragmentContent [B.ByteString]
  | SequenceContent Sequence deriving Eq

instance Show ElementContent where
  showsPrec p (BytesContent _)    = showParen (p > 10) $ showString "BytesContent {..}"
  showsPrec p (FragmentContent f) = showParen (p > 10) $ showString "FragmentContent { length = " . shows (length f) . showString " }"
  showsPrec p (SequenceContent s) = showParen (p > 10) $ showString "SequenceContent " . showsPrec 11 s

data Element = Element
  { elementTag     :: Tag
  , elementVL      :: VL
  , elementVR      :: VR
  , elementContent :: ElementContent
  } deriving (Show, Eq)

data SequenceItem = SequenceItem
  { sequenceItemLength :: Word32
  , sequenceItemElements :: [Element]
  } deriving (Show, Eq)

newtype Sequence = Sequence { runSequence :: [SequenceItem] } deriving (Show, Eq)

newtype Object = Object { runObject :: [Element] } deriving (Show, Eq)

-- Serialization

instance Binary Element where
  get = do
    _tag <- get
    _vr <- get
    _vl <- if isVLReserved _vr then
             (do skip 2
                 vl . fromIntegral <$> getWord32le)
            else vl . fromIntegral <$> getWord16le
    content <- case _vr of
      SQ -> SequenceContent <$> readSequence _vl
      _  -> case _vl of
              UndefinedValueLength ->
                case _tag of
                  PixelData -> FragmentContent <$> readFragmentData
                  _ -> failWithOffset "Undefined VL not implemented"
              _ -> do
                bytes <- getByteString $ fromIntegral $ runVL _vl
                return $ BytesContent bytes
    return $ Element _tag _vl _vr content
  put el = do
    put $ elementTag el
    put $ elementVR el
    if isVLReserved (elementVR el) then
        (do putWord16le 0
            putWord32le . fromIntegral . runVL $ elementVL el)
       else putWord16le . fromIntegral . runVL $ elementVL el
    case elementContent el of
      SequenceContent s -> writeSequence (elementVL el) s
      BytesContent bs -> putByteString bs
      FragmentContent _ -> fail "Fragment content is not supported for writing."

readSequence :: VL -> Get Sequence
readSequence UndefinedValueLength = do
  els <- untilG (isSequenceDelimitationItem <$> get) get
  SequenceDelimitationItem <- get
  skip 4
  return $ Sequence els
readSequence _vl = Sequence <$> untilByteCount (fromIntegral $ runVL _vl) get

writeSequence :: VL -> Sequence -> Put
writeSequence _vl s = do
  traverse_ put (runSequence s)
  case _vl of
    UndefinedValueLength -> do
      put SequenceDelimitationItem
      putWord32le 0
    _ -> return ()

readFragmentData :: Get [B.ByteString]
readFragmentData = do
  els <- untilG (isSequenceDelimitationItem <$> get) $ do
    t <- get
    case t of
      Item -> do
        itemLength <- getWord32le
        getByteString $ fromIntegral $ itemLength
      _ -> failWithOffset "Expected Item tag"
  SequenceDelimitationItem <- get
  skip 4
  return els

instance Binary SequenceItem where
  get = do
    t <- get
    case t of
      Item -> do
        itemLength <- getWord32le
        case vl (fromIntegral itemLength) of
          UndefinedValueLength -> do
            els <- untilG (isItemDelimitationItem <$> get) get
            ItemDelimitationItem <- get
            skip 4
            return $ SequenceItem itemLength els
          _ -> do
            els <- untilByteCount (fromIntegral itemLength) get
            return $ SequenceItem itemLength els
      _ -> failWithOffset "Expected Item tag"
  put si = do
    put Item
    putWord32le $ sequenceItemLength si
    traverse_ put $ sequenceItemElements si
    case vl (fromIntegral (sequenceItemLength si)) of
      UndefinedValueLength -> do
        put ItemDelimitationItem
        putWord32le 0
      _ -> return ()

isItemDelimitationItem :: Tag -> Bool
isItemDelimitationItem ItemDelimitationItem = True
isItemDelimitationItem _ = False

isSequenceDelimitationItem :: Tag -> Bool
isSequenceDelimitationItem SequenceDelimitationItem = True
isSequenceDelimitationItem _ = False

untilG :: Get Bool -> Get a -> Get [a]
untilG more a = lookAhead more >>= bool ((:) <$> a <*> untilG more a) (pure [])

untilByteCount :: Int64 -> Get a -> Get [a]
untilByteCount count a = do
  start <- bytesRead
  flip untilG a $ do
    end <- bytesRead
    return (end - start >= count)

isVLReserved :: VR -> Bool
isVLReserved OB = True
isVLReserved OW = True
isVLReserved OF = True
isVLReserved SQ = True
isVLReserved UT = True
isVLReserved UN = True
isVLReserved _  = False

instance Binary Object where
  get = do
    skip 128
    header <- getByteString 4
    unless (header == dicm) $ failWithOffset "Invalid DICOM header"
    Object <$> untilG isEmpty get
  put obj = do
    putByteString $ B.replicate 128 0
    putByteString dicm

    let fileMetaInfo = takeWhile ((== TagGroup 0x0002) . tagGroup . elementTag) $ runObject obj
        groupLength = sum $ map (BL.length . encode) fileMetaInfo
    put $ tag (TagGroup 0x0002) (TagElement 0x0000)
    put UL
    putWord16le 4
    putWord32le $ fromIntegral groupLength

    traverse_ put (runObject obj)

failWithOffset :: String -> Get a
failWithOffset msg = do
  offset <- bytesRead
  fail $ "Error at offset " ++ printf "%08x" offset ++ ": " ++ msg

readObject :: BL.ByteString -> Either String Object
readObject bs = case decodeOrFail bs of
  Left  (_   , _, e)                      -> Left e
  Right (rest, _, _) | not (BL.null rest) -> Left "Unconsumed input"
  Right (_   , _, a)                      -> Right a

writeObject :: Object -> BL.ByteString
writeObject = encode

-- File IO

readObjectFromFile :: FilePath -> IO (Either String Object)
readObjectFromFile path = readObject <$> BL.readFile path

writeObjectToFile :: FilePath -> Object -> IO ()
writeObjectToFile path = BL.writeFile path . writeObject

-- Smart constructors for DICOM objects
-- TODO: some of these constructors could benefit from better types

instance Monoid Object where
  mempty = Object []
  mappend (Object es1) (Object es2) = Object (sortBy (compare `on` elementTag) $ es1 ++ es2)

element :: VR -> Tag -> B.ByteString -> Element
element vr tg content = Element tg (vl $ fromIntegral count) vr (BytesContent padded)
  where
  (count, padded) = case B.length content of
                     len | len `mod` 2 == 0 -> (len, content)
                         | otherwise -> (len + 1, content <> BC.pack [padChar])
  padChar | isStringVR vr = ' '
          | otherwise = '\0'

-- String value representations

ae :: Tag -> String -> Element
ae t = element AE t . BC.pack

as :: Tag -> String -> Element
as t = element AS t . BC.pack

cs :: Tag -> String -> Element
cs t = element CS t . BC.pack

ds :: Tag -> B.ByteString -> Element
ds = element DS

fl :: Tag -> B.ByteString -> Element
fl = element FL

fd :: Tag -> B.ByteString -> Element
fd = element FD

is :: Tag -> Int -> Element
is t = element IS t . BC.pack . show

lo :: Tag -> String -> Element
lo t = element LO t . BC.pack

lt :: Tag -> String -> Element
lt t = element LT t . BC.pack

pn :: Tag -> String -> Element
pn t = element PN t . BC.pack

sh :: Tag -> String -> Element
sh t = element SH t . BC.pack

sl :: Tag -> B.ByteString -> Element
sl = element SL

ss :: Tag -> B.ByteString -> Element
ss = element SS

st :: Tag -> B.ByteString -> Element
st = element ST

ui :: Tag -> String -> Element
ui t = element UI t . BC.pack

ul :: Tag -> B.ByteString -> Element
ul = element UL

un :: Tag -> B.ByteString -> Element
un = element UN

us :: Tag -> B.ByteString -> Element
us = element US

ut :: Tag -> B.ByteString -> Element
ut = element UT

-- Binary value representations

ob :: Tag -> B.ByteString -> Element
ob = element OB

ow :: Tag -> B.ByteString -> Element
ow = element OW

-- Date/time value representations

da :: Tag -> UTCTime -> Element
da t = element DA t . BC.pack . formatTime defaultTimeLocale "%Y%m%d"

dt :: Tag -> UTCTime -> Element
dt t = element DT t . BC.pack . formatTime defaultTimeLocale "%Y%m%d%H%M%S.000000&0000"

tm :: Tag -> UTCTime -> Element
tm t = element TM t . BC.pack . formatTime defaultTimeLocale "%H%M%S.000000"

sq :: Tag -> [SequenceItem] -> Element
sq tg items = Element tg UndefinedValueLength SQ (SequenceContent (Sequence items))

item :: [Element] -> SequenceItem
item = SequenceItem (fromIntegral . runVL $ UndefinedValueLength) . sortBy (compare `on` elementTag)

object :: [Element] -> Object
object = Object . sortBy (compare `on` elementTag)
