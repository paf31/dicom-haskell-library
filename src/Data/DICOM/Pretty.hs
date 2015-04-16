-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.Pretty
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-----------------------------------------------------------------------------

module Data.DICOM.Pretty (
  ppObject
) where

import Text.Printf
import Text.PrettyPrint ((<>), (<+>))
import qualified Text.PrettyPrint as P

import Data.DICOM.Object
import Data.DICOM.Tag
import Data.DICOM.VL
import Data.DICOM.VR

import qualified Data.ByteString.Char8 as BC

ppElementContent :: P.Doc -> ElementContent -> P.Doc
ppElementContent d (BytesContent bs) = P.hsep [d, P.sizedText 40 $ BC.unpack bs]
ppElementContent d (SequenceContent s) = P.vcat [d, P.nest 4 . P.vcat . map ppSequenceItem . runSequence $ s]

ppSequenceItem :: SequenceItem -> P.Doc
ppSequenceItem si = P.vcat
  [ P.text "Item"
    <+> P.parens ((P.text . show . sequenceItemLength $ si) <> P.text "b")
  , P.vcat (map ppElement $ sequenceItemElements si)
  ]

ppTagGroup :: TagGroup -> P.Doc
ppTagGroup tg = P.text (printf "%04x" $ runTagGroup tg)

ppTagElement :: TagElement -> P.Doc
ppTagElement te = P.text (printf "%04x" $ runTagElement te)

ppTag :: Tag -> P.Doc
ppTag _tag = P.parens $
  ppTagGroup (tagGroup _tag)
  <> P.comma
  <> P.space
  <> ppTagElement (tagElement _tag)

ppVL :: VL -> P.Doc
ppVL = P.sizedText 4 . printf "%4d" . runVL

ppVR :: VR -> P.Doc
ppVR = P.sizedText 2 . show

ppElement :: Element -> P.Doc
ppElement e =
  ppTag (elementTag e)
  <+> ppVL (elementVL e)
  <+> ppVR (elementVR e)
  `ppElementContent` elementContent e

ppObject :: Object -> P.Doc
ppObject (Object els) = P.vcat (map ppElement els)
