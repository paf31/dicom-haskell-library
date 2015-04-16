-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.VL
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

{-# LANGUAGE PatternSynonyms #-}

module Data.DICOM.VL
  ( VL()

  , runVL

  , pattern UndefinedValueLength

  , vl
  ) where

import Data.Int (Int64)

newtype VL = VL { runVL :: Int64 } deriving (Show, Eq, Ord)

-- Pattern synonyms for matching DICOM magic constants

pattern UndefinedValueLength         = VL 0xFFFFFFFF

-- Smart constructors

vl :: Int64 -> VL
vl = VL
