-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.Tag
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Data.DICOM.Tag
  ( TagGroup (TagGroup, runTagGroup)
  , TagElement (TagElement, runTagElement)

  , Tag (tagGroup, tagElement)

  , pattern SequenceGroup

  , pattern Item
  , pattern ItemDelimitationItem
  , pattern SequenceDelimitationItem

  , tag
  ) where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import Control.Applicative

newtype TagGroup = TagGroup { runTagGroup :: Word16 } deriving (Show, Eq, Ord)

newtype TagElement = TagElement { runTagElement :: Word16 } deriving (Show, Eq, Ord)

data Tag = Tag
  { tagGroup   :: TagGroup
  , tagElement :: TagElement
  } deriving (Show, Eq, Ord)

-- Serialization

instance Binary TagGroup where
  get = TagGroup <$> getWord16le
  put = putWord16le . runTagGroup

instance Binary TagElement where
  get = TagElement <$> getWord16le
  put = putWord16le . runTagElement

instance Binary Tag where
  get = Tag <$> get <*> get
  put t = do
    put (tagGroup t)
    put (tagElement t)

-- Special tags

pattern SequenceGroup                = TagGroup 0xFFFE

pattern Item                         = Tag SequenceGroup (TagElement 0xE000)
pattern ItemDelimitationItem         = Tag SequenceGroup (TagElement 0xE00D)
pattern SequenceDelimitationItem     = Tag SequenceGroup (TagElement 0xE0DD)

-- Smart constructors

tag :: TagGroup -> TagElement -> Tag
tag = Tag
