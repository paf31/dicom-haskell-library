-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.FileMetaInformation
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-- | Functions for dealing with the file meta info header.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.DICOM.FileMetaInformation ( 
  FileMetaInformation(..), 
  fileMetaInformation 
  ) where

import Data.DICOM.Object
import Data.DICOM.Dictionary

import Data.String

data FileMetaInformation = FileMetaInformation
  { fmiSOPClassUID :: String
  , fmiSOPInstanceUID :: String
  , fmiTransferSyntaxUID :: String
  , fmiSOPImplementationClassUID :: String
  } deriving (Show, Eq, Ord)

fileMetaInformation :: FileMetaInformation -> Object
fileMetaInformation FileMetaInformation{..} = object
  [ filemetainformationversion  "\0\1"
  , mediastoredsopclassuid      $ fromString fmiSOPClassUID
  , mediastoredsopinstanceuid   $ fromString fmiSOPInstanceUID
  , transfersyntaxuid           $ fromString fmiTransferSyntaxUID
  , implementationclassuid      $ fromString fmiSOPImplementationClassUID
  ]

