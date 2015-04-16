-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.VR
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

module Data.DICOM.VR
  ( VR(..)

  , isStringVR
  ) where

import Prelude hiding (LT)

import Safe

import Data.Binary

data VR
  = AE
  | AS
  | AT
  | CS
  | DA
  | DS
  | DT
  | FL
  | FD
  | IS
  | LO
  | LT
  | OB
  | OF
  | OW
  | PN
  | SH
  | SL
  | SQ
  | SS
  | ST
  | TM
  | UI
  | UL
  | UN
  | US
  | UT deriving (Show, Read, Eq, Ord)

instance Binary VR where
  get = do
    c1 <- get
    c2 <- get
    let code = [c1, c2]
    case readMay code of
      Just res -> return res
      Nothing -> fail $ "Unknown VR: " ++ code
  put vr = do
    let [c1, c2] = show vr
    put c1
    put c2

isStringVR :: VR -> Bool
isStringVR OB = False
isStringVR OW = False
isStringVR _  = True
