-----------------------------------------------------------------------------
--
-- Module      :  Data.DICOM.Dictionary
-- Copyright   :  Copyright (c) DICOM Grid 2015
-- License     :  GPL-3
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :  experimental
-- Portability :
--
-- | A dictionary of DICOM tags
--
-----------------------------------------------------------------------------

module Data.DICOM.Dictionary where

import Data.DICOM.Tag
import Data.DICOM.Object

import Data.Time.Clock (UTCTime)

import qualified Data.ByteString as B

group0002length :: B.ByteString -> Element
group0002length = ul $ tag (TagGroup 0x0002) (TagElement 0x0000)

filemetainformationversion :: B.ByteString -> Element
filemetainformationversion = ob $ tag (TagGroup 0x0002) (TagElement 0x0001)

mediastoredsopclassuid :: String -> Element
mediastoredsopclassuid = ui $ tag (TagGroup 0x0002) (TagElement 0x0002)

mediastoredsopinstanceuid :: String -> Element
mediastoredsopinstanceuid = ui $ tag (TagGroup 0x0002) (TagElement 0x0003)

transfersyntaxuid :: String -> Element
transfersyntaxuid = ui $ tag (TagGroup 0x0002) (TagElement 0x0010)

implementationclassuid :: String -> Element
implementationclassuid = ui $ tag (TagGroup 0x0002) (TagElement 0x0012)

implementationversionname :: String -> Element
implementationversionname = sh $ tag (TagGroup 0x0002) (TagElement 0x0013)

sourceapplicationentitytitle :: String -> Element
sourceapplicationentitytitle = ae $ tag (TagGroup 0x0002) (TagElement 0x0016)

privateinformationcreatoruid :: String -> Element
privateinformationcreatoruid = ui $ tag (TagGroup 0x0002) (TagElement 0x0100)

privateinformation :: B.ByteString -> Element
privateinformation = ob $ tag (TagGroup 0x0002) (TagElement 0x0102)

group0004length :: B.ByteString -> Element
group0004length = ul $ tag (TagGroup 0x0004) (TagElement 0x0000)

filesetid :: String -> Element
filesetid = cs $ tag (TagGroup 0x0004) (TagElement 0x1130)

filesetdescriptorfilefileid :: String -> Element
filesetdescriptorfilefileid = cs $ tag (TagGroup 0x0004) (TagElement 0x1141)

filesetdescriptorfileformat :: String -> Element
filesetdescriptorfileformat = cs $ tag (TagGroup 0x0004) (TagElement 0x1142)

rootdirectoryentitysfirstdirectoryrecordoffset :: B.ByteString -> Element
rootdirectoryentitysfirstdirectoryrecordoffset = ul $ tag (TagGroup 0x0004) (TagElement 0x1200)

rootdirectoryentityslastdirectoryrecordoffset :: B.ByteString -> Element
rootdirectoryentityslastdirectoryrecordoffset = ul $ tag (TagGroup 0x0004) (TagElement 0x1202)

filesetconsistenceflag :: B.ByteString -> Element
filesetconsistenceflag = us $ tag (TagGroup 0x0004) (TagElement 0x1212)

directoryrecordsequence :: [SequenceItem] -> Element
directoryrecordsequence = sq $ tag (TagGroup 0x0004) (TagElement 0x1220)

nextdirectoryrecordoffset :: B.ByteString -> Element
nextdirectoryrecordoffset = ul $ tag (TagGroup 0x0004) (TagElement 0x1400)

recordinuseflag :: B.ByteString -> Element
recordinuseflag = us $ tag (TagGroup 0x0004) (TagElement 0x1410)

referencedlowerleveldirectoryentityoffset :: B.ByteString -> Element
referencedlowerleveldirectoryentityoffset = ul $ tag (TagGroup 0x0004) (TagElement 0x1420)

directoryrecordtype :: String -> Element
directoryrecordtype = cs $ tag (TagGroup 0x0004) (TagElement 0x1430)

privaterecorduid :: String -> Element
privaterecorduid = ui $ tag (TagGroup 0x0004) (TagElement 0x1432)

referencedfileid :: String -> Element
referencedfileid = cs $ tag (TagGroup 0x0004) (TagElement 0x1500)

referencedsopclassuidinfile :: String -> Element
referencedsopclassuidinfile = ui $ tag (TagGroup 0x0004) (TagElement 0x1510)

referencedsopinstanceuidinfile :: String -> Element
referencedsopinstanceuidinfile = ui $ tag (TagGroup 0x0004) (TagElement 0x1511)

numberofreferences :: B.ByteString -> Element
numberofreferences = ul $ tag (TagGroup 0x0004) (TagElement 0x1600)

group0008length :: B.ByteString -> Element
group0008length = ul $ tag (TagGroup 0x0008) (TagElement 0x0000)

group0008lengthtoendret :: B.ByteString -> Element
group0008lengthtoendret = ul $ tag (TagGroup 0x0008) (TagElement 0x0001)

specificcharacterset :: String -> Element
specificcharacterset = cs $ tag (TagGroup 0x0008) (TagElement 0x0005)

imagetype :: String -> Element
imagetype = cs $ tag (TagGroup 0x0008) (TagElement 0x0008)

recognitioncoderet :: String -> Element
recognitioncoderet = sh $ tag (TagGroup 0x0008) (TagElement 0x0010)

instancecreationdate :: UTCTime -> Element
instancecreationdate = da $ tag (TagGroup 0x0008) (TagElement 0x0012)

instancecreationtime :: UTCTime -> Element
instancecreationtime = tm $ tag (TagGroup 0x0008) (TagElement 0x0013)

instancecreatoruid :: String -> Element
instancecreatoruid = ui $ tag (TagGroup 0x0008) (TagElement 0x0014)

sopclassuid :: String -> Element
sopclassuid = ui $ tag (TagGroup 0x0008) (TagElement 0x0016)

sopinstanceuid :: String -> Element
sopinstanceuid = ui $ tag (TagGroup 0x0008) (TagElement 0x0018)

studydate :: UTCTime -> Element
studydate = da $ tag (TagGroup 0x0008) (TagElement 0x0020)

seriesdate :: UTCTime -> Element
seriesdate = da $ tag (TagGroup 0x0008) (TagElement 0x0021)

acquisitiondate :: UTCTime -> Element
acquisitiondate = da $ tag (TagGroup 0x0008) (TagElement 0x0022)

imagedate :: UTCTime -> Element
imagedate = da $ tag (TagGroup 0x0008) (TagElement 0x0023)

overlaydate :: UTCTime -> Element
overlaydate = da $ tag (TagGroup 0x0008) (TagElement 0x0024)

curvedate :: UTCTime -> Element
curvedate = da $ tag (TagGroup 0x0008) (TagElement 0x0025)

studytime :: UTCTime -> Element
studytime = tm $ tag (TagGroup 0x0008) (TagElement 0x0030)

seriestime :: UTCTime -> Element
seriestime = tm $ tag (TagGroup 0x0008) (TagElement 0x0031)

acquisitiontime :: UTCTime -> Element
acquisitiontime = tm $ tag (TagGroup 0x0008) (TagElement 0x0032)

imagetime :: UTCTime -> Element
imagetime = tm $ tag (TagGroup 0x0008) (TagElement 0x0033)

overlaytime :: UTCTime -> Element
overlaytime = tm $ tag (TagGroup 0x0008) (TagElement 0x0034)

curvetime :: UTCTime -> Element
curvetime = tm $ tag (TagGroup 0x0008) (TagElement 0x0035)

datasettyperet :: B.ByteString -> Element
datasettyperet = us $ tag (TagGroup 0x0008) (TagElement 0x0040)

datasetsubtyperet :: String -> Element
datasetsubtyperet = sh $ tag (TagGroup 0x0008) (TagElement 0x0041)

nuclearmedicineseriestype :: String -> Element
nuclearmedicineseriestype = cs $ tag (TagGroup 0x0008) (TagElement 0x0042)

accessionnumber :: String -> Element
accessionnumber = sh $ tag (TagGroup 0x0008) (TagElement 0x0050)

queryretrievelevel :: String -> Element
queryretrievelevel = cs $ tag (TagGroup 0x0008) (TagElement 0x0052)

retrieveaetitle :: String -> Element
retrieveaetitle = ae $ tag (TagGroup 0x0008) (TagElement 0x0054)

failedsopinstanceuidlist :: String -> Element
failedsopinstanceuidlist = ae $ tag (TagGroup 0x0008) (TagElement 0x0058)

modality :: String -> Element
modality = cs $ tag (TagGroup 0x0008) (TagElement 0x0060)

conversiontype :: String -> Element
conversiontype = cs $ tag (TagGroup 0x0008) (TagElement 0x0064)

manufacturer :: String -> Element
manufacturer = lo $ tag (TagGroup 0x0008) (TagElement 0x0070)

institutionname :: String -> Element
institutionname = lo $ tag (TagGroup 0x0008) (TagElement 0x0080)

institutionaddress :: B.ByteString -> Element
institutionaddress = st $ tag (TagGroup 0x0008) (TagElement 0x0081)

institutioncodesequence :: [SequenceItem] -> Element
institutioncodesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x0082)

referringphysiciansname :: String -> Element
referringphysiciansname = pn $ tag (TagGroup 0x0008) (TagElement 0x0090)

referringphysiciansaddress :: B.ByteString -> Element
referringphysiciansaddress = st $ tag (TagGroup 0x0008) (TagElement 0x0092)

referringphysicianstelephonenumbers :: String -> Element
referringphysicianstelephonenumbers = sh $ tag (TagGroup 0x0008) (TagElement 0x0094)

codevalue :: String -> Element
codevalue = sh $ tag (TagGroup 0x0008) (TagElement 0x0100)

codingschemedesignator :: String -> Element
codingschemedesignator = sh $ tag (TagGroup 0x0008) (TagElement 0x0102)

codemeaning :: String -> Element
codemeaning = lo $ tag (TagGroup 0x0008) (TagElement 0x0104)

networkidret :: String -> Element
networkidret = sh $ tag (TagGroup 0x0008) (TagElement 0x1000)

stationname :: String -> Element
stationname = sh $ tag (TagGroup 0x0008) (TagElement 0x1010)

studydescription :: String -> Element
studydescription = lo $ tag (TagGroup 0x0008) (TagElement 0x1030)

procedurecodesequence :: [SequenceItem] -> Element
procedurecodesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1032)

seriesdescription :: String -> Element
seriesdescription = lo $ tag (TagGroup 0x0008) (TagElement 0x103E)

institutionaldepartmentname :: String -> Element
institutionaldepartmentname = lo $ tag (TagGroup 0x0008) (TagElement 0x1040)

attendingphysiciansname :: String -> Element
attendingphysiciansname = pn $ tag (TagGroup 0x0008) (TagElement 0x1050)

nameofphysiciansreadingstudy :: String -> Element
nameofphysiciansreadingstudy = pn $ tag (TagGroup 0x0008) (TagElement 0x1060)

operatorsname :: String -> Element
operatorsname = pn $ tag (TagGroup 0x0008) (TagElement 0x1070)

admittingdiagnosesdescription :: String -> Element
admittingdiagnosesdescription = lo $ tag (TagGroup 0x0008) (TagElement 0x1080)

admittingdiagnosiscodesequence :: [SequenceItem] -> Element
admittingdiagnosiscodesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1084)

manufacturersmodelname :: String -> Element
manufacturersmodelname = lo $ tag (TagGroup 0x0008) (TagElement 0x1090)

referencedresultssequence :: [SequenceItem] -> Element
referencedresultssequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1100)

referencedstudysequence :: [SequenceItem] -> Element
referencedstudysequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1110)

referencedstudycomponentsequence :: [SequenceItem] -> Element
referencedstudycomponentsequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1111)

referencedseriessequence :: [SequenceItem] -> Element
referencedseriessequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1115)

referencedpatientsequence :: [SequenceItem] -> Element
referencedpatientsequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1120)

referencedvisitsequence :: [SequenceItem] -> Element
referencedvisitsequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1125)

referencedoverlaysequence :: [SequenceItem] -> Element
referencedoverlaysequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1130)

referencedimagesequence :: [SequenceItem] -> Element
referencedimagesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1140)

referencedcurvesequence :: [SequenceItem] -> Element
referencedcurvesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1145)

referencedsopclassuid :: String -> Element
referencedsopclassuid = ui $ tag (TagGroup 0x0008) (TagElement 0x1150)

referencedsopinstanceuid :: String -> Element
referencedsopinstanceuid = ui $ tag (TagGroup 0x0008) (TagElement 0x1155)

referencedsopsequence :: [SequenceItem] -> Element
referencedsopsequence = sq $ tag (TagGroup 0x0008) (TagElement 0x1199)

derivationdescription :: B.ByteString -> Element
derivationdescription = st $ tag (TagGroup 0x0008) (TagElement 0x2111)

sourceimagesequence :: [SequenceItem] -> Element
sourceimagesequence = sq $ tag (TagGroup 0x0008) (TagElement 0x2112)

stagename :: String -> Element
stagename = sh $ tag (TagGroup 0x0008) (TagElement 0x2120)

stagenumber :: Int -> Element
stagenumber = is $ tag (TagGroup 0x0008) (TagElement 0x2122)

numberofstages :: Int -> Element
numberofstages = is $ tag (TagGroup 0x0008) (TagElement 0x2124)

numberofeventtimers :: Int -> Element
numberofeventtimers = is $ tag (TagGroup 0x0008) (TagElement 0x2129)

viewnumber :: Int -> Element
viewnumber = is $ tag (TagGroup 0x0008) (TagElement 0x2128)

numberofviewsinstage :: Int -> Element
numberofviewsinstage = is $ tag (TagGroup 0x0008) (TagElement 0x212A)

eventelapsedtimes :: B.ByteString -> Element
eventelapsedtimes = ds $ tag (TagGroup 0x0008) (TagElement 0x2130)

eventtimernames :: String -> Element
eventtimernames = lo $ tag (TagGroup 0x0008) (TagElement 0x2132)

starttrim :: Int -> Element
starttrim = is $ tag (TagGroup 0x0008) (TagElement 0x2142)

stoptrim :: Int -> Element
stoptrim = is $ tag (TagGroup 0x0008) (TagElement 0x2143)

recommendeddisplayframerate :: Int -> Element
recommendeddisplayframerate = is $ tag (TagGroup 0x0008) (TagElement 0x2144)

transducerposition :: String -> Element
transducerposition = cs $ tag (TagGroup 0x0008) (TagElement 0x2200)

transducerorientation :: String -> Element
transducerorientation = cs $ tag (TagGroup 0x0008) (TagElement 0x2204)

anatomicstructure :: String -> Element
anatomicstructure = cs $ tag (TagGroup 0x0008) (TagElement 0x2208)

group0008commentsret :: String -> Element
group0008commentsret = sh $ tag (TagGroup 0x0008) (TagElement 0x4000)

group0010length :: B.ByteString -> Element
group0010length = ul $ tag (TagGroup 0x0010) (TagElement 0x0000)

patientsname :: String -> Element
patientsname = pn $ tag (TagGroup 0x0010) (TagElement 0x0010)

patientid :: String -> Element
patientid = lo $ tag (TagGroup 0x0010) (TagElement 0x0020)

issuerofpatientid :: String -> Element
issuerofpatientid = lo $ tag (TagGroup 0x0010) (TagElement 0x0021)

patientsbirthdate :: UTCTime -> Element
patientsbirthdate = da $ tag (TagGroup 0x0010) (TagElement 0x0030)

patientsbirthtime :: UTCTime -> Element
patientsbirthtime = tm $ tag (TagGroup 0x0010) (TagElement 0x0032)

patientssex :: String -> Element
patientssex = cs $ tag (TagGroup 0x0010) (TagElement 0x0040)

patientssocialsecuritynumber :: String -> Element
patientssocialsecuritynumber = sh $ tag (TagGroup 0x0010) (TagElement 0x0042)

patientsinsuranceplancodesequence :: [SequenceItem] -> Element
patientsinsuranceplancodesequence = sq $ tag (TagGroup 0x0010) (TagElement 0x0050)

otherpatientids :: String -> Element
otherpatientids = lo $ tag (TagGroup 0x0010) (TagElement 0x1000)

otherpatientnames :: String -> Element
otherpatientnames = pn $ tag (TagGroup 0x0010) (TagElement 0x1001)

patientsmaidenname :: String -> Element
patientsmaidenname = pn $ tag (TagGroup 0x0010) (TagElement 0x1005)

patientsage :: String -> Element
patientsage = as $ tag (TagGroup 0x0010) (TagElement 0x1010)

patientssize :: B.ByteString -> Element
patientssize = ds $ tag (TagGroup 0x0010) (TagElement 0x1020)

patientsweight :: B.ByteString -> Element
patientsweight = ds $ tag (TagGroup 0x0010) (TagElement 0x1030)

patientsaddress :: String -> Element
patientsaddress = lo $ tag (TagGroup 0x0010) (TagElement 0x1040)

insuranceplanidentificationret :: String -> Element
insuranceplanidentificationret = sh $ tag (TagGroup 0x0010) (TagElement 0x1050)

patientsmothersmaidenname :: String -> Element
patientsmothersmaidenname = pn $ tag (TagGroup 0x0010) (TagElement 0x1060)

militaryrank :: String -> Element
militaryrank = lo $ tag (TagGroup 0x0010) (TagElement 0x1080)

branchofservice :: String -> Element
branchofservice = lo $ tag (TagGroup 0x0010) (TagElement 0x1081)

medicalrecordlocator :: String -> Element
medicalrecordlocator = lo $ tag (TagGroup 0x0010) (TagElement 0x1090)

medicalalerts :: String -> Element
medicalalerts = lo $ tag (TagGroup 0x0010) (TagElement 0x2000)

contrastallergies :: String -> Element
contrastallergies = lo $ tag (TagGroup 0x0010) (TagElement 0x2110)

countryofresidence :: String -> Element
countryofresidence = lo $ tag (TagGroup 0x0010) (TagElement 0x2150)

regionofresidence :: String -> Element
regionofresidence = lo $ tag (TagGroup 0x0010) (TagElement 0x2152)

patientstelephonenumbers :: String -> Element
patientstelephonenumbers = sh $ tag (TagGroup 0x0010) (TagElement 0x2154)

ethnicgroup :: String -> Element
ethnicgroup = sh $ tag (TagGroup 0x0010) (TagElement 0x2160)

occupation :: String -> Element
occupation = sh $ tag (TagGroup 0x0010) (TagElement 0x2180)

smokingstatus :: String -> Element
smokingstatus = cs $ tag (TagGroup 0x0010) (TagElement 0x21A0)

additionalpatienthistory :: String -> Element
additionalpatienthistory = lt $ tag (TagGroup 0x0010) (TagElement 0x21B0)

pregnancystatus :: B.ByteString -> Element
pregnancystatus = us $ tag (TagGroup 0x0010) (TagElement 0x21C0)

lastmenstrualdate :: UTCTime -> Element
lastmenstrualdate = da $ tag (TagGroup 0x0010) (TagElement 0x21D0)

patientsreligiouspreference :: String -> Element
patientsreligiouspreference = lo $ tag (TagGroup 0x0010) (TagElement 0x21F0)

patientcomments :: String -> Element
patientcomments = lt $ tag (TagGroup 0x0010) (TagElement 0x4000)

group0018length :: B.ByteString -> Element
group0018length = ul $ tag (TagGroup 0x0018) (TagElement 0x0000)

contrastbolusagent :: String -> Element
contrastbolusagent = lo $ tag (TagGroup 0x0018) (TagElement 0x0010)

bodypartexamined :: String -> Element
bodypartexamined = cs $ tag (TagGroup 0x0018) (TagElement 0x0015)

scanningsequence :: String -> Element
scanningsequence = cs $ tag (TagGroup 0x0018) (TagElement 0x0020)

sequencevariant :: String -> Element
sequencevariant = cs $ tag (TagGroup 0x0018) (TagElement 0x0021)

scanoptions :: String -> Element
scanoptions = cs $ tag (TagGroup 0x0018) (TagElement 0x0022)

mracquisitiontype :: String -> Element
mracquisitiontype = cs $ tag (TagGroup 0x0018) (TagElement 0x0023)

sequencename :: String -> Element
sequencename = sh $ tag (TagGroup 0x0018) (TagElement 0x0024)

angioflag :: String -> Element
angioflag = cs $ tag (TagGroup 0x0018) (TagElement 0x0025)

radionuclide :: String -> Element
radionuclide = lo $ tag (TagGroup 0x0018) (TagElement 0x0030)

radiopharmaceutical :: String -> Element
radiopharmaceutical = lo $ tag (TagGroup 0x0018) (TagElement 0x0031)

energywindowcenterline :: B.ByteString -> Element
energywindowcenterline = ds $ tag (TagGroup 0x0018) (TagElement 0x0032)

energywindowtotalwidth :: B.ByteString -> Element
energywindowtotalwidth = ds $ tag (TagGroup 0x0018) (TagElement 0x0033)

interventiondrugname :: String -> Element
interventiondrugname = lo $ tag (TagGroup 0x0018) (TagElement 0x0034)

interventiondrugstarttime :: UTCTime -> Element
interventiondrugstarttime = tm $ tag (TagGroup 0x0018) (TagElement 0x0035)

cinerate :: Int -> Element
cinerate = is $ tag (TagGroup 0x0018) (TagElement 0x0040)

slicethickness :: B.ByteString -> Element
slicethickness = ds $ tag (TagGroup 0x0018) (TagElement 0x0050)

kvp :: B.ByteString -> Element
kvp = ds $ tag (TagGroup 0x0018) (TagElement 0x0060)

countsaccumulated :: Int -> Element
countsaccumulated = is $ tag (TagGroup 0x0018) (TagElement 0x0070)

acquisitionterminationcondition :: String -> Element
acquisitionterminationcondition = cs $ tag (TagGroup 0x0018) (TagElement 0x0071)

effectiveseriesduration :: B.ByteString -> Element
effectiveseriesduration = ds $ tag (TagGroup 0x0018) (TagElement 0x0072)

repetitiontime :: B.ByteString -> Element
repetitiontime = ds $ tag (TagGroup 0x0018) (TagElement 0x0080)

echotime :: B.ByteString -> Element
echotime = ds $ tag (TagGroup 0x0018) (TagElement 0x0081)

inversiontime :: B.ByteString -> Element
inversiontime = ds $ tag (TagGroup 0x0018) (TagElement 0x0082)

numberofaverages :: B.ByteString -> Element
numberofaverages = ds $ tag (TagGroup 0x0018) (TagElement 0x0083)

imagingfrequency :: B.ByteString -> Element
imagingfrequency = ds $ tag (TagGroup 0x0018) (TagElement 0x0084)

imagednucleus :: String -> Element
imagednucleus = sh $ tag (TagGroup 0x0018) (TagElement 0x0085)

echonumberss :: Int -> Element
echonumberss = is $ tag (TagGroup 0x0018) (TagElement 0x0086)

magneticfieldstrength :: B.ByteString -> Element
magneticfieldstrength = ds $ tag (TagGroup 0x0018) (TagElement 0x0087)

spacingbetweenslices :: B.ByteString -> Element
spacingbetweenslices = ds $ tag (TagGroup 0x0018) (TagElement 0x0088)

numberofphaseencodingsteps :: Int -> Element
numberofphaseencodingsteps = is $ tag (TagGroup 0x0018) (TagElement 0x0089)

datacollectiondiameter :: B.ByteString -> Element
datacollectiondiameter = ds $ tag (TagGroup 0x0018) (TagElement 0x0090)

echotrainlength :: Int -> Element
echotrainlength = is $ tag (TagGroup 0x0018) (TagElement 0x0091)

percentsampling :: B.ByteString -> Element
percentsampling = ds $ tag (TagGroup 0x0018) (TagElement 0x0093)

percentphasefieldofview :: B.ByteString -> Element
percentphasefieldofview = ds $ tag (TagGroup 0x0018) (TagElement 0x0094)

pixelbandwidth :: B.ByteString -> Element
pixelbandwidth = ds $ tag (TagGroup 0x0018) (TagElement 0x0095)

deviceserialnumber :: String -> Element
deviceserialnumber = lo $ tag (TagGroup 0x0018) (TagElement 0x1000)

plateid :: String -> Element
plateid = lo $ tag (TagGroup 0x0018) (TagElement 0x1004)

secondarycapturedeviceid :: String -> Element
secondarycapturedeviceid = lo $ tag (TagGroup 0x0018) (TagElement 0x1010)

dateofsecondarycapture :: UTCTime -> Element
dateofsecondarycapture = da $ tag (TagGroup 0x0018) (TagElement 0x1012)

timeofsecondarycapture :: UTCTime -> Element
timeofsecondarycapture = tm $ tag (TagGroup 0x0018) (TagElement 0x1014)

secondarycapturedevicemanufacturer :: String -> Element
secondarycapturedevicemanufacturer = lo $ tag (TagGroup 0x0018) (TagElement 0x1016)

secondarycapturedevicemanufacturersmodelname :: String -> Element
secondarycapturedevicemanufacturersmodelname = lo $ tag (TagGroup 0x0018) (TagElement 0x1018)

secondarycapturedevicesoftwareversions :: String -> Element
secondarycapturedevicesoftwareversions = lo $ tag (TagGroup 0x0018) (TagElement 0x1019)

softwareversionss :: String -> Element
softwareversionss = lo $ tag (TagGroup 0x0018) (TagElement 0x1020)

videoimageformatacquired :: String -> Element
videoimageformatacquired = sh $ tag (TagGroup 0x0018) (TagElement 0x1022)

digitalimageformatacquired :: String -> Element
digitalimageformatacquired = lo $ tag (TagGroup 0x0018) (TagElement 0x1023)

protocolname :: String -> Element
protocolname = lo $ tag (TagGroup 0x0018) (TagElement 0x1030)

contrastbolusroute :: String -> Element
contrastbolusroute = lo $ tag (TagGroup 0x0018) (TagElement 0x1040)

contrastbolusvolume :: B.ByteString -> Element
contrastbolusvolume = ds $ tag (TagGroup 0x0018) (TagElement 0x1041)

contrastbolusstarttime :: UTCTime -> Element
contrastbolusstarttime = tm $ tag (TagGroup 0x0018) (TagElement 0x1042)

contrastbolusstoptime :: UTCTime -> Element
contrastbolusstoptime = tm $ tag (TagGroup 0x0018) (TagElement 0x1043)

contrastbolustotaldose :: B.ByteString -> Element
contrastbolustotaldose = ds $ tag (TagGroup 0x0018) (TagElement 0x1044)

syringecounts :: Int -> Element
syringecounts = is $ tag (TagGroup 0x0018) (TagElement 0x1045)

spatialresolution :: B.ByteString -> Element
spatialresolution = ds $ tag (TagGroup 0x0018) (TagElement 0x1050)

triggertime :: B.ByteString -> Element
triggertime = ds $ tag (TagGroup 0x0018) (TagElement 0x1060)

triggersourceortype :: String -> Element
triggersourceortype = lo $ tag (TagGroup 0x0018) (TagElement 0x1061)

nominalinterval :: Int -> Element
nominalinterval = is $ tag (TagGroup 0x0018) (TagElement 0x1062)

frametime :: B.ByteString -> Element
frametime = ds $ tag (TagGroup 0x0018) (TagElement 0x1063)

framingtype :: String -> Element
framingtype = lo $ tag (TagGroup 0x0018) (TagElement 0x1064)

frametimevector :: B.ByteString -> Element
frametimevector = ds $ tag (TagGroup 0x0018) (TagElement 0x1065)

framedelay :: B.ByteString -> Element
framedelay = ds $ tag (TagGroup 0x0018) (TagElement 0x1066)

radionuclideroute :: String -> Element
radionuclideroute = lo $ tag (TagGroup 0x0018) (TagElement 0x1070)

radionuclidevolume :: B.ByteString -> Element
radionuclidevolume = ds $ tag (TagGroup 0x0018) (TagElement 0x1071)

radionuclidestarttime :: UTCTime -> Element
radionuclidestarttime = tm $ tag (TagGroup 0x0018) (TagElement 0x1072)

radionuclidestoptime :: UTCTime -> Element
radionuclidestoptime = tm $ tag (TagGroup 0x0018) (TagElement 0x1073)

radionuclidetotaldose :: B.ByteString -> Element
radionuclidetotaldose = ds $ tag (TagGroup 0x0018) (TagElement 0x1074)

beatrejectionflag :: String -> Element
beatrejectionflag = cs $ tag (TagGroup 0x0018) (TagElement 0x1080)

lowrrvalue :: Int -> Element
lowrrvalue = is $ tag (TagGroup 0x0018) (TagElement 0x1081)

highrrvalue :: Int -> Element
highrrvalue = is $ tag (TagGroup 0x0018) (TagElement 0x1082)

intervalsacquired :: Int -> Element
intervalsacquired = is $ tag (TagGroup 0x0018) (TagElement 0x1083)

intervalsrejected :: Int -> Element
intervalsrejected = is $ tag (TagGroup 0x0018) (TagElement 0x1084)

pvcrejection :: String -> Element
pvcrejection = lo $ tag (TagGroup 0x0018) (TagElement 0x1085)

skipbeats :: Int -> Element
skipbeats = is $ tag (TagGroup 0x0018) (TagElement 0x1086)

heartrate :: Int -> Element
heartrate = is $ tag (TagGroup 0x0018) (TagElement 0x1088)

cardiacnumberofimages :: Int -> Element
cardiacnumberofimages = is $ tag (TagGroup 0x0018) (TagElement 0x1090)

triggerwindow :: Int -> Element
triggerwindow = is $ tag (TagGroup 0x0018) (TagElement 0x1094)

reconstructiondiameter :: B.ByteString -> Element
reconstructiondiameter = ds $ tag (TagGroup 0x0018) (TagElement 0x1100)

distancesourcetodetector :: B.ByteString -> Element
distancesourcetodetector = ds $ tag (TagGroup 0x0018) (TagElement 0x1110)

distancesourcetopatient :: B.ByteString -> Element
distancesourcetopatient = ds $ tag (TagGroup 0x0018) (TagElement 0x1111)

gantrydetectortilt :: B.ByteString -> Element
gantrydetectortilt = ds $ tag (TagGroup 0x0018) (TagElement 0x1120)

tableheight :: B.ByteString -> Element
tableheight = ds $ tag (TagGroup 0x0018) (TagElement 0x1030)

tabletraverse :: B.ByteString -> Element
tabletraverse = ds $ tag (TagGroup 0x0018) (TagElement 0x1131)

rotationdirection :: String -> Element
rotationdirection = cs $ tag (TagGroup 0x0018) (TagElement 0x1140)

angularposition :: B.ByteString -> Element
angularposition = ds $ tag (TagGroup 0x0018) (TagElement 0x1141)

radialposition :: B.ByteString -> Element
radialposition = ds $ tag (TagGroup 0x0018) (TagElement 0x1142)

scanarc :: B.ByteString -> Element
scanarc = ds $ tag (TagGroup 0x0018) (TagElement 0x1143)

angularstep :: B.ByteString -> Element
angularstep = ds $ tag (TagGroup 0x0018) (TagElement 0x1144)

centerofrotationoffset :: B.ByteString -> Element
centerofrotationoffset = ds $ tag (TagGroup 0x0018) (TagElement 0x1145)

rotationoffset :: B.ByteString -> Element
rotationoffset = ds $ tag (TagGroup 0x0018) (TagElement 0x1146)

fieldofviewshape :: String -> Element
fieldofviewshape = cs $ tag (TagGroup 0x0018) (TagElement 0x1147)

fieldofviewdimensionss :: Int -> Element
fieldofviewdimensionss = is $ tag (TagGroup 0x0018) (TagElement 0x1149)

exposuretime :: Int -> Element
exposuretime = is $ tag (TagGroup 0x0018) (TagElement 0x1150)

xraytubecurrent :: Int -> Element
xraytubecurrent = is $ tag (TagGroup 0x0018) (TagElement 0x1151)

exposure :: Int -> Element
exposure = is $ tag (TagGroup 0x0018) (TagElement 0x1152)

filtertype :: String -> Element
filtertype = sh $ tag (TagGroup 0x0018) (TagElement 0x1160)

generatorpower :: Int -> Element
generatorpower = is $ tag (TagGroup 0x0018) (TagElement 0x1170)

collimatorgridname :: String -> Element
collimatorgridname = sh $ tag (TagGroup 0x0018) (TagElement 0x1180)

collimatortype :: String -> Element
collimatortype = cs $ tag (TagGroup 0x0018) (TagElement 0x1181)

focaldistance :: Int -> Element
focaldistance = is $ tag (TagGroup 0x0018) (TagElement 0x1182)

xfocuscenter :: B.ByteString -> Element
xfocuscenter = ds $ tag (TagGroup 0x0018) (TagElement 0x1183)

yfocuscenter :: B.ByteString -> Element
yfocuscenter = ds $ tag (TagGroup 0x0018) (TagElement 0x1184)

focalspots :: B.ByteString -> Element
focalspots = ds $ tag (TagGroup 0x0018) (TagElement 0x1190)

dateoflastcalibration :: UTCTime -> Element
dateoflastcalibration = da $ tag (TagGroup 0x0018) (TagElement 0x1200)

timeoflastcalibration :: UTCTime -> Element
timeoflastcalibration = tm $ tag (TagGroup 0x0018) (TagElement 0x1201)

convolutionkernel :: String -> Element
convolutionkernel = sh $ tag (TagGroup 0x0018) (TagElement 0x1210)

upperlowerpixelvaluesret :: B.ByteString -> Element
upperlowerpixelvaluesret = ds $ tag (TagGroup 0x0018) (TagElement 0x1240)

actualframeduration :: Int -> Element
actualframeduration = is $ tag (TagGroup 0x0018) (TagElement 0x1242)

countrate :: Int -> Element
countrate = is $ tag (TagGroup 0x0018) (TagElement 0x1243)

receivingcoil :: String -> Element
receivingcoil = sh $ tag (TagGroup 0x0018) (TagElement 0x1250)

transmittingcoil :: String -> Element
transmittingcoil = sh $ tag (TagGroup 0x0018) (TagElement 0x1151)

screentype :: String -> Element
screentype = sh $ tag (TagGroup 0x0018) (TagElement 0x1160)

phosphortype :: String -> Element
phosphortype = lo $ tag (TagGroup 0x0018) (TagElement 0x1261)

scanvelocity :: Int -> Element
scanvelocity = is $ tag (TagGroup 0x0018) (TagElement 0x1300)

wholebodytechnique :: String -> Element
wholebodytechnique = cs $ tag (TagGroup 0x0018) (TagElement 0x1301)

scanlength :: Int -> Element
scanlength = is $ tag (TagGroup 0x0018) (TagElement 0x1302)

acquisitionmatrix :: B.ByteString -> Element
acquisitionmatrix = us $ tag (TagGroup 0x0018) (TagElement 0x1310)

phaseencodingdirection :: String -> Element
phaseencodingdirection = cs $ tag (TagGroup 0x0018) (TagElement 0x1312)

flipangle :: B.ByteString -> Element
flipangle = ds $ tag (TagGroup 0x0018) (TagElement 0x1314)

variableflipangleflag :: String -> Element
variableflipangleflag = cs $ tag (TagGroup 0x0018) (TagElement 0x1315)

sar :: B.ByteString -> Element
sar = ds $ tag (TagGroup 0x0018) (TagElement 0x1316)

dbdt :: B.ByteString -> Element
dbdt = ds $ tag (TagGroup 0x0018) (TagElement 0x1318)

acquisitiondeviceprocessingdescription :: String -> Element
acquisitiondeviceprocessingdescription = lo $ tag (TagGroup 0x0018) (TagElement 0x1400)

acquisitiondeviceprocessingcode :: String -> Element
acquisitiondeviceprocessingcode = lo $ tag (TagGroup 0x0018) (TagElement 0x1401)

cassetteorientation :: String -> Element
cassetteorientation = cs $ tag (TagGroup 0x0018) (TagElement 0x1402)

cassettesize :: String -> Element
cassettesize = cs $ tag (TagGroup 0x0018) (TagElement 0x1403)

exposuresonplate :: B.ByteString -> Element
exposuresonplate = us $ tag (TagGroup 0x0018) (TagElement 0x1404)

relativexrayexposure :: Int -> Element
relativexrayexposure = is $ tag (TagGroup 0x0018) (TagElement 0x1405)

group0018commentsret :: String -> Element
group0018commentsret = sh $ tag (TagGroup 0x0018) (TagElement 0x4000)

outputpower :: String -> Element
outputpower = sh $ tag (TagGroup 0x0018) (TagElement 0x5000)

transducerdata :: String -> Element
transducerdata = lo $ tag (TagGroup 0x0018) (TagElement 0x5010)

focusdepth :: B.ByteString -> Element
focusdepth = ds $ tag (TagGroup 0x0018) (TagElement 0x5012)

preprocessingfunction :: String -> Element
preprocessingfunction = lo $ tag (TagGroup 0x0018) (TagElement 0x5020)

postprocessingfunction :: String -> Element
postprocessingfunction = lo $ tag (TagGroup 0x0018) (TagElement 0x5021)

mechanicalindex :: B.ByteString -> Element
mechanicalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5022)

thermalindex :: B.ByteString -> Element
thermalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5024)

cranialthermalindex :: B.ByteString -> Element
cranialthermalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5026)

softtissuethermalindex :: B.ByteString -> Element
softtissuethermalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5027)

softtissuefocusthermalindex :: B.ByteString -> Element
softtissuefocusthermalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5028)

softtissuesurfacethermalindex :: B.ByteString -> Element
softtissuesurfacethermalindex = ds $ tag (TagGroup 0x0018) (TagElement 0x5029)

dynamicrangeret :: Int -> Element
dynamicrangeret = is $ tag (TagGroup 0x0018) (TagElement 0x5030)

totalgainret :: Int -> Element
totalgainret = is $ tag (TagGroup 0x0018) (TagElement 0x5040)

depthofscanfield :: Int -> Element
depthofscanfield = is $ tag (TagGroup 0x0018) (TagElement 0x5050)

patientposition :: String -> Element
patientposition = cs $ tag (TagGroup 0x0018) (TagElement 0x5100)

viewposition :: String -> Element
viewposition = cs $ tag (TagGroup 0x0018) (TagElement 0x5101)

imagetransformationmatrix :: B.ByteString -> Element
imagetransformationmatrix = ds $ tag (TagGroup 0x0018) (TagElement 0x5210)

imagetranslationvector :: B.ByteString -> Element
imagetranslationvector = ds $ tag (TagGroup 0x0018) (TagElement 0x5212)

sensitivity :: B.ByteString -> Element
sensitivity = ds $ tag (TagGroup 0x0018) (TagElement 0x6000)

sequenceofultrasoundregions :: [SequenceItem] -> Element
sequenceofultrasoundregions = sq $ tag (TagGroup 0x0018) (TagElement 0x6011)

regionspatialformat :: B.ByteString -> Element
regionspatialformat = us $ tag (TagGroup 0x0018) (TagElement 0x6012)

regiondatatype :: B.ByteString -> Element
regiondatatype = us $ tag (TagGroup 0x0018) (TagElement 0x6014)

regionflags :: B.ByteString -> Element
regionflags = ul $ tag (TagGroup 0x0018) (TagElement 0x6016)

regionlocationminx0 :: B.ByteString -> Element
regionlocationminx0 = ul $ tag (TagGroup 0x0018) (TagElement 0x6018)

regionlocationminy0 :: B.ByteString -> Element
regionlocationminy0 = ul $ tag (TagGroup 0x0018) (TagElement 0x601A)

regionlocationmaxx1 :: B.ByteString -> Element
regionlocationmaxx1 = ul $ tag (TagGroup 0x0018) (TagElement 0x601C)

regionlocationmaxy1 :: B.ByteString -> Element
regionlocationmaxy1 = ul $ tag (TagGroup 0x0018) (TagElement 0x601E)

referencepixelx0 :: B.ByteString -> Element
referencepixelx0 = sl $ tag (TagGroup 0x0018) (TagElement 0x6020)

referencepixely0 :: B.ByteString -> Element
referencepixely0 = sl $ tag (TagGroup 0x0018) (TagElement 0x6022)

physicalunitsxdirection :: B.ByteString -> Element
physicalunitsxdirection = us $ tag (TagGroup 0x0018) (TagElement 0x6024)

physicalunitsydirection :: B.ByteString -> Element
physicalunitsydirection = us $ tag (TagGroup 0x0018) (TagElement 0x6026)

referencepixelphysicalvaluex :: B.ByteString -> Element
referencepixelphysicalvaluex = fd $ tag (TagGroup 0x0018) (TagElement 0x1628)

referencepixelphysicalvaluey :: B.ByteString -> Element
referencepixelphysicalvaluey = fd $ tag (TagGroup 0x0018) (TagElement 0x602A)

physicaldeltax :: B.ByteString -> Element
physicaldeltax = fd $ tag (TagGroup 0x0018) (TagElement 0x602C)

physicaldeltay :: B.ByteString -> Element
physicaldeltay = fd $ tag (TagGroup 0x0018) (TagElement 0x602E)

transducerfrequency :: B.ByteString -> Element
transducerfrequency = ul $ tag (TagGroup 0x0018) (TagElement 0x6030)

transducertype :: String -> Element
transducertype = cs $ tag (TagGroup 0x0018) (TagElement 0x6031)

pulserepetitionfrequency :: B.ByteString -> Element
pulserepetitionfrequency = ul $ tag (TagGroup 0x0018) (TagElement 0x6032)

dopplercorrectionangle :: B.ByteString -> Element
dopplercorrectionangle = fd $ tag (TagGroup 0x0018) (TagElement 0x6034)

sterringangle :: B.ByteString -> Element
sterringangle = fd $ tag (TagGroup 0x0018) (TagElement 0x6036)

dopplersamplevolumexposition :: B.ByteString -> Element
dopplersamplevolumexposition = ul $ tag (TagGroup 0x0018) (TagElement 0x6038)

dopplersamplevolumeyposition :: B.ByteString -> Element
dopplersamplevolumeyposition = ul $ tag (TagGroup 0x0018) (TagElement 0x603A)

tmlinepositionx0 :: B.ByteString -> Element
tmlinepositionx0 = ul $ tag (TagGroup 0x0018) (TagElement 0x603C)

tmlinepositiony0 :: B.ByteString -> Element
tmlinepositiony0 = ul $ tag (TagGroup 0x0018) (TagElement 0x603E)

tmlinepositionx1 :: B.ByteString -> Element
tmlinepositionx1 = ul $ tag (TagGroup 0x0018) (TagElement 0x6040)

tmlinepositiony1 :: B.ByteString -> Element
tmlinepositiony1 = ul $ tag (TagGroup 0x0018) (TagElement 0x6042)

pixelcomponentorganization :: B.ByteString -> Element
pixelcomponentorganization = us $ tag (TagGroup 0x0018) (TagElement 0x6044)

pixelcomponentrangestart :: B.ByteString -> Element
pixelcomponentrangestart = ul $ tag (TagGroup 0x0018) (TagElement 0x6048)

pixelcomponentrangestop :: B.ByteString -> Element
pixelcomponentrangestop = ul $ tag (TagGroup 0x0018) (TagElement 0x604A)

pixelcomponentphysicalunits :: B.ByteString -> Element
pixelcomponentphysicalunits = us $ tag (TagGroup 0x0018) (TagElement 0x604C)

pixelcomponentdatatype :: B.ByteString -> Element
pixelcomponentdatatype = us $ tag (TagGroup 0x0018) (TagElement 0x604E)

numberoftablebreakpoints :: B.ByteString -> Element
numberoftablebreakpoints = ul $ tag (TagGroup 0x0018) (TagElement 0x6050)

tableofxbreakpoints :: B.ByteString -> Element
tableofxbreakpoints = ul $ tag (TagGroup 0x0018) (TagElement 0x6052)

tableofybreakpoints :: B.ByteString -> Element
tableofybreakpoints = fd $ tag (TagGroup 0x0018) (TagElement 0x6054)

group0020length :: B.ByteString -> Element
group0020length = ul $ tag (TagGroup 0x0020) (TagElement 0x0000)

studyinstanceuid :: String -> Element
studyinstanceuid = ui $ tag (TagGroup 0x0020) (TagElement 0x000D)

seriesinstanceuid :: String -> Element
seriesinstanceuid = ui $ tag (TagGroup 0x0020) (TagElement 0x000E)

studyid :: String -> Element
studyid = sh $ tag (TagGroup 0x0020) (TagElement 0x0010)

seriesnumber :: Int -> Element
seriesnumber = is $ tag (TagGroup 0x0020) (TagElement 0x0011)

acquisitionnumber :: Int -> Element
acquisitionnumber = is $ tag (TagGroup 0x0020) (TagElement 0x0012)

imagenumber :: Int -> Element
imagenumber = is $ tag (TagGroup 0x0020) (TagElement 0x0013)

isotopenumber :: Int -> Element
isotopenumber = is $ tag (TagGroup 0x0020) (TagElement 0x0014)

phasenumber :: Int -> Element
phasenumber = is $ tag (TagGroup 0x0020) (TagElement 0x0015)

intervalnumber :: Int -> Element
intervalnumber = is $ tag (TagGroup 0x0020) (TagElement 0x0016)

timeslotnumber :: Int -> Element
timeslotnumber = is $ tag (TagGroup 0x0020) (TagElement 0x0017)

anglenumber :: Int -> Element
anglenumber = is $ tag (TagGroup 0x0020) (TagElement 0x0018)

patientorientation :: String -> Element
patientorientation = cs $ tag (TagGroup 0x0020) (TagElement 0x0020)

overlaynumber :: B.ByteString -> Element
overlaynumber = us $ tag (TagGroup 0x0020) (TagElement 0x0022)

curvenumber :: B.ByteString -> Element
curvenumber = us $ tag (TagGroup 0x0020) (TagElement 0x0024)

imagepositionret :: B.ByteString -> Element
imagepositionret = ds $ tag (TagGroup 0x0020) (TagElement 0x0030)

imagepositionpatient :: B.ByteString -> Element
imagepositionpatient = ds $ tag (TagGroup 0x0020) (TagElement 0x0032)

imageorientationret :: B.ByteString -> Element
imageorientationret = ds $ tag (TagGroup 0x0020) (TagElement 0x0035)

imageorientationpatient :: B.ByteString -> Element
imageorientationpatient = ds $ tag (TagGroup 0x0020) (TagElement 0x0037)

locationret :: B.ByteString -> Element
locationret = ds $ tag (TagGroup 0x0020) (TagElement 0x0050)

frameofreferenceuid :: String -> Element
frameofreferenceuid = ui $ tag (TagGroup 0x0020) (TagElement 0x0052)

laterality :: String -> Element
laterality = cs $ tag (TagGroup 0x0020) (TagElement 0x0060)

imagegeometrytyperet :: String -> Element
imagegeometrytyperet = sh $ tag (TagGroup 0x0020) (TagElement 0x0070)

maskingimageuid :: String -> Element
maskingimageuid = ui $ tag (TagGroup 0x0020) (TagElement 0x0080)

temporalpositionidentifier :: Int -> Element
temporalpositionidentifier = is $ tag (TagGroup 0x0020) (TagElement 0x0100)

numberoftemporalpositions :: Int -> Element
numberoftemporalpositions = is $ tag (TagGroup 0x0020) (TagElement 0x0105)

temporalresolution :: B.ByteString -> Element
temporalresolution = ds $ tag (TagGroup 0x0020) (TagElement 0x0110)

seriesinstudy :: Int -> Element
seriesinstudy = is $ tag (TagGroup 0x0020) (TagElement 0x1000)

acquisitionsinseriesret :: Int -> Element
acquisitionsinseriesret = is $ tag (TagGroup 0x0020) (TagElement 0x1001)

imagesinacquisition :: Int -> Element
imagesinacquisition = is $ tag (TagGroup 0x0020) (TagElement 0x1002)

acquisitioninstudy :: Int -> Element
acquisitioninstudy = is $ tag (TagGroup 0x0020) (TagElement 0x1004)

referenceret :: String -> Element
referenceret = sh $ tag (TagGroup 0x0020) (TagElement 0x1020)

positionreferenceindicator :: String -> Element
positionreferenceindicator = lo $ tag (TagGroup 0x0020) (TagElement 0x1040)

slicelocation :: B.ByteString -> Element
slicelocation = ds $ tag (TagGroup 0x0020) (TagElement 0x1041)

otherstudynumbers :: Int -> Element
otherstudynumbers = is $ tag (TagGroup 0x0020) (TagElement 0x1070)

numberofpatientrelatedstudies :: Int -> Element
numberofpatientrelatedstudies = is $ tag (TagGroup 0x0020) (TagElement 0x1200)

numberofpatientrelatedseries :: Int -> Element
numberofpatientrelatedseries = is $ tag (TagGroup 0x0020) (TagElement 0x1202)

numberofpatientrelatedimages :: Int -> Element
numberofpatientrelatedimages = is $ tag (TagGroup 0x0020) (TagElement 0x1204)

numberofstudyrelatedseries :: Int -> Element
numberofstudyrelatedseries = is $ tag (TagGroup 0x0020) (TagElement 0x1206)

numberofstudyrelatedimages :: Int -> Element
numberofstudyrelatedimages = is $ tag (TagGroup 0x0020) (TagElement 0x1208)

sourceimageidrets :: String -> Element
sourceimageidrets = sh $ tag (TagGroup 0x0020) (TagElement 0x3100)

modifyingdeviceidret :: String -> Element
modifyingdeviceidret = sh $ tag (TagGroup 0x0020) (TagElement 0x3401)

modifiedimageidret :: String -> Element
modifiedimageidret = sh $ tag (TagGroup 0x0020) (TagElement 0x3402)

modifiedimagedateret :: String -> Element
modifiedimagedateret = sh $ tag (TagGroup 0x0020) (TagElement 0x3403)

modifyingdevicemanufacturerret :: String -> Element
modifyingdevicemanufacturerret = sh $ tag (TagGroup 0x0020) (TagElement 0x3404)

modifiedimagetimeret :: String -> Element
modifiedimagetimeret = sh $ tag (TagGroup 0x0020) (TagElement 0x3405)

modifiedimagedescriptionret :: String -> Element
modifiedimagedescriptionret = sh $ tag (TagGroup 0x0020) (TagElement 0x3406)

imagecomments :: String -> Element
imagecomments = lt $ tag (TagGroup 0x0020) (TagElement 0x4000)

originalimageidentificationret :: B.ByteString -> Element
originalimageidentificationret = us $ tag (TagGroup 0x0020) (TagElement 0x5000)

originalimageidentificationnomenclatureret :: String -> Element
originalimageidentificationnomenclatureret = sh $ tag (TagGroup 0x0020) (TagElement 0x5002)

group0028length :: B.ByteString -> Element
group0028length = ul $ tag (TagGroup 0x0028) (TagElement 0x0000)

samplesperpixel :: B.ByteString -> Element
samplesperpixel = us $ tag (TagGroup 0x0028) (TagElement 0x0002)

photometricinterpretation :: String -> Element
photometricinterpretation = cs $ tag (TagGroup 0x0028) (TagElement 0x0004)

imagedimensionsret :: B.ByteString -> Element
imagedimensionsret = us $ tag (TagGroup 0x0028) (TagElement 0x0005)

planarconfiguration :: B.ByteString -> Element
planarconfiguration = us $ tag (TagGroup 0x0028) (TagElement 0x0006)

numberofframes :: Int -> Element
numberofframes = is $ tag (TagGroup 0x0028) (TagElement 0x0008)

rows :: B.ByteString -> Element
rows = us $ tag (TagGroup 0x0028) (TagElement 0x0010)

columns :: B.ByteString -> Element
columns = us $ tag (TagGroup 0x0028) (TagElement 0x0011)

pixelspacing :: B.ByteString -> Element
pixelspacing = ds $ tag (TagGroup 0x0028) (TagElement 0x0030)

zoomfactor :: B.ByteString -> Element
zoomfactor = ds $ tag (TagGroup 0x0028) (TagElement 0x0031)

zoomcenter :: B.ByteString -> Element
zoomcenter = ds $ tag (TagGroup 0x0028) (TagElement 0x0032)

pixelaspectratio :: Int -> Element
pixelaspectratio = is $ tag (TagGroup 0x0028) (TagElement 0x0034)

imageformatret :: String -> Element
imageformatret = sh $ tag (TagGroup 0x0028) (TagElement 0x0040)

manipulatedimageret :: String -> Element
manipulatedimageret = sh $ tag (TagGroup 0x0028) (TagElement 0x0050)

correctedimage :: String -> Element
correctedimage = cs $ tag (TagGroup 0x0028) (TagElement 0x0051)

compressioncoderet :: String -> Element
compressioncoderet = sh $ tag (TagGroup 0x0028) (TagElement 0x0060)

bitsallocated :: B.ByteString -> Element
bitsallocated = us $ tag (TagGroup 0x0028) (TagElement 0x0100)

bitsstored :: B.ByteString -> Element
bitsstored = us $ tag (TagGroup 0x0028) (TagElement 0x0101)

highbit :: B.ByteString -> Element
highbit = us $ tag (TagGroup 0x0028) (TagElement 0x0102)

pixelrepresentation :: B.ByteString -> Element
pixelrepresentation = us $ tag (TagGroup 0x0028) (TagElement 0x0103)

smallestvalidpixelvalueret :: B.ByteString -> Element
smallestvalidpixelvalueret = us $ tag (TagGroup 0x0028) (TagElement 0x0104)

largestvalidpixelvalueret :: B.ByteString -> Element
largestvalidpixelvalueret = us $ tag (TagGroup 0x0028) (TagElement 0x0105)

smallestimagepixelvalue :: B.ByteString -> Element
smallestimagepixelvalue = us $ tag (TagGroup 0x0028) (TagElement 0x0106)

largestimagepixelvalue :: B.ByteString -> Element
largestimagepixelvalue = us $ tag (TagGroup 0x0028) (TagElement 0x0107)

smallestpixelvalueinseries :: B.ByteString -> Element
smallestpixelvalueinseries = us $ tag (TagGroup 0x0028) (TagElement 0x0108)

largestpixelvalueinseries :: B.ByteString -> Element
largestpixelvalueinseries = us $ tag (TagGroup 0x0028) (TagElement 0x0109)

pixelpaddingvalue :: B.ByteString -> Element
pixelpaddingvalue = us $ tag (TagGroup 0x0028) (TagElement 0x0120)

imagelocationret :: B.ByteString -> Element
imagelocationret = us $ tag (TagGroup 0x0028) (TagElement 0x0200)

windowcenter :: B.ByteString -> Element
windowcenter = ds $ tag (TagGroup 0x0028) (TagElement 0x1050)

windowwidth :: B.ByteString -> Element
windowwidth = ds $ tag (TagGroup 0x0028) (TagElement 0x1051)

rescaleintercept :: B.ByteString -> Element
rescaleintercept = ds $ tag (TagGroup 0x0028) (TagElement 0x1052)

rescaleslope :: B.ByteString -> Element
rescaleslope = ds $ tag (TagGroup 0x0028) (TagElement 0x1053)

rescaletype :: String -> Element
rescaletype = lo $ tag (TagGroup 0x0028) (TagElement 0x1054)

windowcenterwidthexplanation :: String -> Element
windowcenterwidthexplanation = lo $ tag (TagGroup 0x0028) (TagElement 0x1055)

grayscaleret :: String -> Element
grayscaleret = sh $ tag (TagGroup 0x0028) (TagElement 0x1080)

graylookuptabledescriptorret :: B.ByteString -> Element
graylookuptabledescriptorret = us $ tag (TagGroup 0x0028) (TagElement 0x1100)

redpalettecolorlookuptabledescriptor :: B.ByteString -> Element
redpalettecolorlookuptabledescriptor = us $ tag (TagGroup 0x0028) (TagElement 0x1101)

greenpalettecolorlookuptabledescriptor :: B.ByteString -> Element
greenpalettecolorlookuptabledescriptor = us $ tag (TagGroup 0x0028) (TagElement 0x1102)

bluepalettecolorlookuptabledescriptor :: B.ByteString -> Element
bluepalettecolorlookuptabledescriptor = us $ tag (TagGroup 0x0028) (TagElement 0x1103)

graylookuptabledataret :: B.ByteString -> Element
graylookuptabledataret = us $ tag (TagGroup 0x0028) (TagElement 0x1200)

redpalettecolorlookuptabledata :: B.ByteString -> Element
redpalettecolorlookuptabledata = us $ tag (TagGroup 0x0028) (TagElement 0x1201)

greenpalettecolorlookuptabledata :: B.ByteString -> Element
greenpalettecolorlookuptabledata = us $ tag (TagGroup 0x0028) (TagElement 0x1202)

bluepalettecolorlookuptabledata :: B.ByteString -> Element
bluepalettecolorlookuptabledata = us $ tag (TagGroup 0x0028) (TagElement 0x1203)

modalitylutsequence :: [SequenceItem] -> Element
modalitylutsequence = sq $ tag (TagGroup 0x0028) (TagElement 0x3000)

lutdescriptor :: B.ByteString -> Element
lutdescriptor = us $ tag (TagGroup 0x0028) (TagElement 0x3002)

lutexplanation :: String -> Element
lutexplanation = lo $ tag (TagGroup 0x0028) (TagElement 0x3003)

madalityluttype :: String -> Element
madalityluttype = lo $ tag (TagGroup 0x0028) (TagElement 0x3004)

lutdata :: B.ByteString -> Element
lutdata = us $ tag (TagGroup 0x0028) (TagElement 0x3006)

voilutsequence :: [SequenceItem] -> Element
voilutsequence = sq $ tag (TagGroup 0x0028) (TagElement 0x3010)

group0028commentsret :: String -> Element
group0028commentsret = sh $ tag (TagGroup 0x0028) (TagElement 0x4000)

group0032length :: B.ByteString -> Element
group0032length = ul $ tag (TagGroup 0x0032) (TagElement 0x0000)

studystatusid :: String -> Element
studystatusid = cs $ tag (TagGroup 0x0032) (TagElement 0x000A)

studypriorityid :: String -> Element
studypriorityid = cs $ tag (TagGroup 0x0032) (TagElement 0x000C)

studyidissuer :: String -> Element
studyidissuer = lo $ tag (TagGroup 0x0032) (TagElement 0x0012)

studyverifieddate :: UTCTime -> Element
studyverifieddate = da $ tag (TagGroup 0x0032) (TagElement 0x0032)

studyverifiedtime :: UTCTime -> Element
studyverifiedtime = tm $ tag (TagGroup 0x0032) (TagElement 0x0033)

studyreaddate :: UTCTime -> Element
studyreaddate = da $ tag (TagGroup 0x0032) (TagElement 0x0034)

studyreadtime :: UTCTime -> Element
studyreadtime = tm $ tag (TagGroup 0x0032) (TagElement 0x0035)

scheduledstudystartdate :: UTCTime -> Element
scheduledstudystartdate = da $ tag (TagGroup 0x0032) (TagElement 0x1000)

scheduledstudystarttime :: UTCTime -> Element
scheduledstudystarttime = tm $ tag (TagGroup 0x0032) (TagElement 0x1001)

scheduledstudystopdate :: UTCTime -> Element
scheduledstudystopdate = da $ tag (TagGroup 0x0032) (TagElement 0x1010)

scheduledstudystoptime :: UTCTime -> Element
scheduledstudystoptime = tm $ tag (TagGroup 0x0032) (TagElement 0x1011)

scheduledstudylocation :: String -> Element
scheduledstudylocation = lo $ tag (TagGroup 0x0032) (TagElement 0x1020)

scheduledstudylocationaetitles :: String -> Element
scheduledstudylocationaetitles = ae $ tag (TagGroup 0x0032) (TagElement 0x1021)

reasonforstudy :: String -> Element
reasonforstudy = lo $ tag (TagGroup 0x0032) (TagElement 0x1030)

requestingphysician :: String -> Element
requestingphysician = pn $ tag (TagGroup 0x0032) (TagElement 0x1032)

requestingservice :: String -> Element
requestingservice = lo $ tag (TagGroup 0x0032) (TagElement 0x1033)

studyarrivaldate :: UTCTime -> Element
studyarrivaldate = da $ tag (TagGroup 0x0032) (TagElement 0x1040)

studyarrivaltime :: UTCTime -> Element
studyarrivaltime = tm $ tag (TagGroup 0x0032) (TagElement 0x1041)

studycompletiondate :: UTCTime -> Element
studycompletiondate = da $ tag (TagGroup 0x0032) (TagElement 0x1050)

studycompletiontime :: UTCTime -> Element
studycompletiontime = tm $ tag (TagGroup 0x0032) (TagElement 0x1051)

studycomponentstatusid :: String -> Element
studycomponentstatusid = cs $ tag (TagGroup 0x0032) (TagElement 0x1055)

requestedproceduredescription :: String -> Element
requestedproceduredescription = lo $ tag (TagGroup 0x0032) (TagElement 0x1060)

requestedprocedurecodesequence :: [SequenceItem] -> Element
requestedprocedurecodesequence = sq $ tag (TagGroup 0x0032) (TagElement 0x1064)

requestedcontrastagent :: String -> Element
requestedcontrastagent = lo $ tag (TagGroup 0x0032) (TagElement 0x1070)

studycomments :: String -> Element
studycomments = lt $ tag (TagGroup 0x0032) (TagElement 0x4000)

group0038length :: B.ByteString -> Element
group0038length = ul $ tag (TagGroup 0x0038) (TagElement 0x0000)

referencedpatientaliassequence :: [SequenceItem] -> Element
referencedpatientaliassequence = sq $ tag (TagGroup 0x0038) (TagElement 0x0004)

visitstatusid :: String -> Element
visitstatusid = cs $ tag (TagGroup 0x0038) (TagElement 0x0008)

admissinid :: String -> Element
admissinid = lo $ tag (TagGroup 0x0038) (TagElement 0x0010)

issuerofadmissionid :: String -> Element
issuerofadmissionid = lo $ tag (TagGroup 0x0038) (TagElement 0x0011)

routeofadmissions :: String -> Element
routeofadmissions = lo $ tag (TagGroup 0x0038) (TagElement 0x0016)

scheduledadmissindate :: UTCTime -> Element
scheduledadmissindate = da $ tag (TagGroup 0x0038) (TagElement 0x001A)

scheduledadissiontime :: UTCTime -> Element
scheduledadissiontime = tm $ tag (TagGroup 0x0038) (TagElement 0x001B)

scheduleddischargedate :: UTCTime -> Element
scheduleddischargedate = da $ tag (TagGroup 0x0038) (TagElement 0x001C)

scheduleddischargetime :: UTCTime -> Element
scheduleddischargetime = tm $ tag (TagGroup 0x0038) (TagElement 0x001D)

scheduledpatientinstitutionresidence :: String -> Element
scheduledpatientinstitutionresidence = lo $ tag (TagGroup 0x0038) (TagElement 0x001E)

admittingdate :: UTCTime -> Element
admittingdate = da $ tag (TagGroup 0x0038) (TagElement 0x0020)

admittingtime :: UTCTime -> Element
admittingtime = tm $ tag (TagGroup 0x0038) (TagElement 0x0021)

dischargedate :: UTCTime -> Element
dischargedate = da $ tag (TagGroup 0x0038) (TagElement 0x0030)

dischargetime :: UTCTime -> Element
dischargetime = tm $ tag (TagGroup 0x0038) (TagElement 0x0032)

dischargediagnosisdescription :: String -> Element
dischargediagnosisdescription = lo $ tag (TagGroup 0x0038) (TagElement 0x0040)

dischargediagnosiscodesequence :: [SequenceItem] -> Element
dischargediagnosiscodesequence = sq $ tag (TagGroup 0x0038) (TagElement 0x0044)

specialneeds :: String -> Element
specialneeds = lo $ tag (TagGroup 0x0038) (TagElement 0x0050)

currentpatientlocation :: String -> Element
currentpatientlocation = lo $ tag (TagGroup 0x0038) (TagElement 0x0300)

patientsinstitutionresidence :: String -> Element
patientsinstitutionresidence = lo $ tag (TagGroup 0x0038) (TagElement 0x0400)

patientstate :: String -> Element
patientstate = lo $ tag (TagGroup 0x0038) (TagElement 0x0500)

visitcomments :: String -> Element
visitcomments = lt $ tag (TagGroup 0x0038) (TagElement 0x4000)

relationshiptype :: String -> Element
relationshiptype = cs $ tag (TagGroup 0x0040) (TagElement 0xA010)

conceptcodesequence :: [SequenceItem] -> Element
conceptcodesequence = sq $ tag (TagGroup 0x0040) (TagElement 0xA168)

currentrequestedprocedureevidencesequence :: [SequenceItem] -> Element
currentrequestedprocedureevidencesequence = sq $ tag (TagGroup 0x0040) (TagElement 0xA375)

valuetype :: String -> Element
valuetype = cs $ tag (TagGroup 0x0040) (TagElement 0xA040)

conceptnamecodesequence :: [SequenceItem] -> Element
conceptnamecodesequence = sq $ tag (TagGroup 0x0040) (TagElement 0xA043)

continuityofcontent :: String -> Element
continuityofcontent = cs $ tag (TagGroup 0x0040) (TagElement 0xA050)

contentsequence :: [SequenceItem] -> Element
contentsequence = sq $ tag (TagGroup 0x0040) (TagElement 0xA730)

group0088length :: B.ByteString -> Element
group0088length = ul $ tag (TagGroup 0x0088) (TagElement 0x0000)

storagemediafilesetid :: String -> Element
storagemediafilesetid = sh $ tag (TagGroup 0x0088) (TagElement 0x0130)

storagemediafilesetuid :: String -> Element
storagemediafilesetuid = ui $ tag (TagGroup 0x0088) (TagElement 0x0140)

group2000length :: B.ByteString -> Element
group2000length = ul $ tag (TagGroup 0x2000) (TagElement 0x0000)

numberofcopies :: Int -> Element
numberofcopies = is $ tag (TagGroup 0x2000) (TagElement 0x0010)

printpriority :: String -> Element
printpriority = cs $ tag (TagGroup 0x2000) (TagElement 0x0020)

mediumtype :: String -> Element
mediumtype = cs $ tag (TagGroup 0x2000) (TagElement 0x0030)

filmdestination :: String -> Element
filmdestination = cs $ tag (TagGroup 0x2000) (TagElement 0x0040)

filmsessionlabel :: String -> Element
filmsessionlabel = lo $ tag (TagGroup 0x2000) (TagElement 0x0050)

memoryallocation :: Int -> Element
memoryallocation = is $ tag (TagGroup 0x2000) (TagElement 0x0060)

referencedfilmboxsequence :: [SequenceItem] -> Element
referencedfilmboxsequence = sq $ tag (TagGroup 0x2000) (TagElement 0x0500)

group2010length :: B.ByteString -> Element
group2010length = ul $ tag (TagGroup 0x2010) (TagElement 0x0000)

imagedisplayformat :: B.ByteString -> Element
imagedisplayformat = st $ tag (TagGroup 0x2010) (TagElement 0x0010)

annotationdisplayformatid :: String -> Element
annotationdisplayformatid = cs $ tag (TagGroup 0x2010) (TagElement 0x0030)

filmorientation :: String -> Element
filmorientation = cs $ tag (TagGroup 0x2010) (TagElement 0x0040)

filmsizeid :: String -> Element
filmsizeid = cs $ tag (TagGroup 0x2010) (TagElement 0x0050)

magnificationtype :: String -> Element
magnificationtype = cs $ tag (TagGroup 0x2010) (TagElement 0x0060)

smoothingtype :: String -> Element
smoothingtype = cs $ tag (TagGroup 0x2010) (TagElement 0x0080)

borderdensity :: String -> Element
borderdensity = cs $ tag (TagGroup 0x2010) (TagElement 0x0100)

emptyimagedensity :: String -> Element
emptyimagedensity = cs $ tag (TagGroup 0x2010) (TagElement 0x0110)

mindensity :: B.ByteString -> Element
mindensity = us $ tag (TagGroup 0x2010) (TagElement 0x0120)

maxdensity :: B.ByteString -> Element
maxdensity = us $ tag (TagGroup 0x2010) (TagElement 0x0130)

trim :: String -> Element
trim = cs $ tag (TagGroup 0x2010) (TagElement 0x0140)

configurationinformation :: B.ByteString -> Element
configurationinformation = st $ tag (TagGroup 0x2010) (TagElement 0x0150)

referencedfilmsessionsequence :: [SequenceItem] -> Element
referencedfilmsessionsequence = sq $ tag (TagGroup 0x2010) (TagElement 0x0500)

referencedbasicimageboxsequence :: [SequenceItem] -> Element
referencedbasicimageboxsequence = sq $ tag (TagGroup 0x2010) (TagElement 0x0510)

referencedbasicannotationboxsequence :: [SequenceItem] -> Element
referencedbasicannotationboxsequence = sq $ tag (TagGroup 0x2010) (TagElement 0x0520)

group2020length :: B.ByteString -> Element
group2020length = ul $ tag (TagGroup 0x2020) (TagElement 0x0000)

imageposition :: B.ByteString -> Element
imageposition = us $ tag (TagGroup 0x2020) (TagElement 0x0010)

polarity :: String -> Element
polarity = cs $ tag (TagGroup 0x2020) (TagElement 0x0020)

requestedimagesize :: B.ByteString -> Element
requestedimagesize = ds $ tag (TagGroup 0x2020) (TagElement 0x0030)

preformattedgreyscaleimagesequence :: [SequenceItem] -> Element
preformattedgreyscaleimagesequence = sq $ tag (TagGroup 0x2020) (TagElement 0x0110)

preformattedcolorimagesequence :: [SequenceItem] -> Element
preformattedcolorimagesequence = sq $ tag (TagGroup 0x2020) (TagElement 0x0111)

referencedimageoverlayboxsequence :: [SequenceItem] -> Element
referencedimageoverlayboxsequence = sq $ tag (TagGroup 0x2020) (TagElement 0x0130)

referencedvoilutsequence :: [SequenceItem] -> Element
referencedvoilutsequence = sq $ tag (TagGroup 0x2020) (TagElement 0x0140)

group2030length :: B.ByteString -> Element
group2030length = ul $ tag (TagGroup 0x2030) (TagElement 0x0000)

annotationposition :: B.ByteString -> Element
annotationposition = us $ tag (TagGroup 0x2030) (TagElement 0x0010)

textstring :: String -> Element
textstring = lo $ tag (TagGroup 0x2030) (TagElement 0x0020)

group2040length :: B.ByteString -> Element
group2040length = ul $ tag (TagGroup 0x2040) (TagElement 0x0000)

referencedoverlayplanesequence :: [SequenceItem] -> Element
referencedoverlayplanesequence = sq $ tag (TagGroup 0x2040) (TagElement 0x0010)

refencedoverlayplanegroups :: B.ByteString -> Element
refencedoverlayplanegroups = us $ tag (TagGroup 0x2040) (TagElement 0x0011)

overlaymagnificationtype :: String -> Element
overlaymagnificationtype = cs $ tag (TagGroup 0x2040) (TagElement 0x0060)

overlaysmoothingtype :: String -> Element
overlaysmoothingtype = cs $ tag (TagGroup 0x2040) (TagElement 0x0070)

overlayforegrounddensity :: String -> Element
overlayforegrounddensity = cs $ tag (TagGroup 0x2040) (TagElement 0x0080)

overlaymode :: String -> Element
overlaymode = cs $ tag (TagGroup 0x2040) (TagElement 0x0090)

thresholddensity :: String -> Element
thresholddensity = cs $ tag (TagGroup 0x2040) (TagElement 0x0100)

referencedimageboxsequence :: [SequenceItem] -> Element
referencedimageboxsequence = sq $ tag (TagGroup 0x2040) (TagElement 0x0500)

group2100length :: B.ByteString -> Element
group2100length = ul $ tag (TagGroup 0x2100) (TagElement 0x0000)

executionstatus :: String -> Element
executionstatus = cs $ tag (TagGroup 0x2100) (TagElement 0x0020)

executionstatusinfo :: String -> Element
executionstatusinfo = cs $ tag (TagGroup 0x2100) (TagElement 0x0030)

creationdate :: UTCTime -> Element
creationdate = da $ tag (TagGroup 0x2100) (TagElement 0x0040)

creationtime :: UTCTime -> Element
creationtime = tm $ tag (TagGroup 0x2100) (TagElement 0x0050)

originator :: String -> Element
originator = ae $ tag (TagGroup 0x2100) (TagElement 0x0070)

referencedprintjobsequence :: [SequenceItem] -> Element
referencedprintjobsequence = sq $ tag (TagGroup 0x2100) (TagElement 0x0500)

group2110length :: B.ByteString -> Element
group2110length = ul $ tag (TagGroup 0x2110) (TagElement 0x0000)

printerstatus :: String -> Element
printerstatus = cs $ tag (TagGroup 0x2110) (TagElement 0x0010)

printerstatusinfo :: String -> Element
printerstatusinfo = cs $ tag (TagGroup 0x2110) (TagElement 0x0020)

printername :: B.ByteString -> Element
printername = st $ tag (TagGroup 0x2110) (TagElement 0x0030)

group4000lengthret :: B.ByteString -> Element
group4000lengthret = ul $ tag (TagGroup 0x4000) (TagElement 0x0000)

arbitrayret :: String -> Element
arbitrayret = sh $ tag (TagGroup 0x4000) (TagElement 0x0010)

group4000commentsret :: String -> Element
group4000commentsret = lt $ tag (TagGroup 0x4000) (TagElement 0x4000)

group4008length :: B.ByteString -> Element
group4008length = ul $ tag (TagGroup 0x4008) (TagElement 0x0000)

resultsid :: String -> Element
resultsid = sh $ tag (TagGroup 0x4008) (TagElement 0x0040)

resultsidissuer :: String -> Element
resultsidissuer = lo $ tag (TagGroup 0x4008) (TagElement 0x0042)

referencedinterpretationsequence :: [SequenceItem] -> Element
referencedinterpretationsequence = sq $ tag (TagGroup 0x4008) (TagElement 0x0050)

interpretationrecordeddate :: UTCTime -> Element
interpretationrecordeddate = da $ tag (TagGroup 0x4008) (TagElement 0x0100)

interpretationrecordedtime :: UTCTime -> Element
interpretationrecordedtime = tm $ tag (TagGroup 0x4008) (TagElement 0x0101)

interpretationrecorder :: String -> Element
interpretationrecorder = pn $ tag (TagGroup 0x4008) (TagElement 0x0102)

referencetorecordedsound :: String -> Element
referencetorecordedsound = lo $ tag (TagGroup 0x4008) (TagElement 0x0103)

interpretationtranscriptiontime :: UTCTime -> Element
interpretationtranscriptiontime = da $ tag (TagGroup 0x4008) (TagElement 0x0108)

interpretationtranscriber :: String -> Element
interpretationtranscriber = pn $ tag (TagGroup 0x4008) (TagElement 0x010A)

interpretationtext :: B.ByteString -> Element
interpretationtext = st $ tag (TagGroup 0x4008) (TagElement 0x010B)

interpretationauthor :: String -> Element
interpretationauthor = pn $ tag (TagGroup 0x4008) (TagElement 0x010C)

interpretationapproversequence :: [SequenceItem] -> Element
interpretationapproversequence = sq $ tag (TagGroup 0x4008) (TagElement 0x0111)

interpretationapprovaldate :: UTCTime -> Element
interpretationapprovaldate = da $ tag (TagGroup 0x4008) (TagElement 0x0112)

interpretationapprovaltime :: UTCTime -> Element
interpretationapprovaltime = tm $ tag (TagGroup 0x4008) (TagElement 0x0113)

physicianapprovinginterpretation :: String -> Element
physicianapprovinginterpretation = pn $ tag (TagGroup 0x4008) (TagElement 0x0114)

interpretationdiagnosisdescription :: String -> Element
interpretationdiagnosisdescription = lt $ tag (TagGroup 0x4008) (TagElement 0x0115)

diagnosiscodesequence :: [SequenceItem] -> Element
diagnosiscodesequence = sq $ tag (TagGroup 0x4008) (TagElement 0x0117)

resultsdistributionlistsequence :: [SequenceItem] -> Element
resultsdistributionlistsequence = sq $ tag (TagGroup 0x4008) (TagElement 0x0118)

distributionname :: String -> Element
distributionname = pn $ tag (TagGroup 0x4008) (TagElement 0x0119)

distributionaddress :: String -> Element
distributionaddress = lo $ tag (TagGroup 0x4008) (TagElement 0x011A)

interpretationid :: String -> Element
interpretationid = sh $ tag (TagGroup 0x4008) (TagElement 0x0200)

interpretationidissuer :: String -> Element
interpretationidissuer = lo $ tag (TagGroup 0x4008) (TagElement 0x0202)

interpretationtypeid :: String -> Element
interpretationtypeid = cs $ tag (TagGroup 0x4008) (TagElement 0x0210)

interpretationstatusid :: String -> Element
interpretationstatusid = cs $ tag (TagGroup 0x4008) (TagElement 0x0212)

impression :: B.ByteString -> Element
impression = st $ tag (TagGroup 0x4008) (TagElement 0x0300)

group4008comments :: String -> Element
group4008comments = sh $ tag (TagGroup 0x4008) (TagElement 0x4000)

group5000length :: B.ByteString -> Element
group5000length = ul $ tag (TagGroup 0x5000) (TagElement 0x0000)

curvedimensions :: B.ByteString -> Element
curvedimensions = us $ tag (TagGroup 0x5000) (TagElement 0x0005)

numberofpoints :: B.ByteString -> Element
numberofpoints = us $ tag (TagGroup 0x5000) (TagElement 0x0010)

typeofdata :: String -> Element
typeofdata = cs $ tag (TagGroup 0x5000) (TagElement 0x0020)

curvedescription :: String -> Element
curvedescription = lo $ tag (TagGroup 0x5000) (TagElement 0x0022)

axisunits :: String -> Element
axisunits = sh $ tag (TagGroup 0x5000) (TagElement 0x0030)

axislabels :: String -> Element
axislabels = sh $ tag (TagGroup 0x5000) (TagElement 0x0040)

datavaluerepresentation :: B.ByteString -> Element
datavaluerepresentation = us $ tag (TagGroup 0x5000) (TagElement 0x0103)

minimumcoordinatevalue :: B.ByteString -> Element
minimumcoordinatevalue = us $ tag (TagGroup 0x5000) (TagElement 0x0104)

maximumcoordinatevalue :: B.ByteString -> Element
maximumcoordinatevalue = us $ tag (TagGroup 0x5000) (TagElement 0x0105)

curverange :: String -> Element
curverange = sh $ tag (TagGroup 0x5000) (TagElement 0x0106)

curvedatadescriptor :: B.ByteString -> Element
curvedatadescriptor = us $ tag (TagGroup 0x5000) (TagElement 0x0110)

coordinatestartvalue :: B.ByteString -> Element
coordinatestartvalue = us $ tag (TagGroup 0x5000) (TagElement 0x0112)

coordinatestepvalue :: B.ByteString -> Element
coordinatestepvalue = us $ tag (TagGroup 0x5000) (TagElement 0x0114)

audiotype :: B.ByteString -> Element
audiotype = us $ tag (TagGroup 0x5000) (TagElement 0x2000)

audiosampleformat :: B.ByteString -> Element
audiosampleformat = us $ tag (TagGroup 0x5000) (TagElement 0x2002)

numberofchannels :: B.ByteString -> Element
numberofchannels = us $ tag (TagGroup 0x5000) (TagElement 0x2004)

numberofsamples :: B.ByteString -> Element
numberofsamples = ul $ tag (TagGroup 0x5000) (TagElement 0x2006)

samplerate :: B.ByteString -> Element
samplerate = ul $ tag (TagGroup 0x5000) (TagElement 0x2008)

totaltime :: B.ByteString -> Element
totaltime = ul $ tag (TagGroup 0x5000) (TagElement 0x200A)

audiosampledata :: B.ByteString -> Element
audiosampledata = ob $ tag (TagGroup 0x5000) (TagElement 0x200C)

audiocomments :: String -> Element
audiocomments = lt $ tag (TagGroup 0x5000) (TagElement 0x200E)

curvedata :: B.ByteString -> Element
curvedata = ob $ tag (TagGroup 0x5000) (TagElement 0x3000)

group6000length :: B.ByteString -> Element
group6000length = ul $ tag (TagGroup 0x6000) (TagElement 0x0000)

numberofframesinoverlay :: Int -> Element
numberofframesinoverlay = is $ tag (TagGroup 0x6000) (TagElement 0x0015)

overlaytype :: String -> Element
overlaytype = cs $ tag (TagGroup 0x6000) (TagElement 0x0040)

origin :: B.ByteString -> Element
origin = ss $ tag (TagGroup 0x6000) (TagElement 0x0050)

bitposition :: B.ByteString -> Element
bitposition = us $ tag (TagGroup 0x6000) (TagElement 0x0102)

overlayformatret :: String -> Element
overlayformatret = sh $ tag (TagGroup 0x6000) (TagElement 0x0110)

overlaylocationret :: B.ByteString -> Element
overlaylocationret = us $ tag (TagGroup 0x6000) (TagElement 0x0200)

overlaydescriptorgray :: B.ByteString -> Element
overlaydescriptorgray = us $ tag (TagGroup 0x6000) (TagElement 0x1100)

overlaydescriptorred :: B.ByteString -> Element
overlaydescriptorred = us $ tag (TagGroup 0x6000) (TagElement 0x1101)

overlaydescriptorgreen :: B.ByteString -> Element
overlaydescriptorgreen = us $ tag (TagGroup 0x6000) (TagElement 0x1102)

overlaydescriptorblue :: B.ByteString -> Element
overlaydescriptorblue = us $ tag (TagGroup 0x6000) (TagElement 0x1103)

overlaysgray :: B.ByteString -> Element
overlaysgray = us $ tag (TagGroup 0x6000) (TagElement 0x1200)

overlaysred :: B.ByteString -> Element
overlaysred = us $ tag (TagGroup 0x6000) (TagElement 0x1201)

overlaysgreen :: B.ByteString -> Element
overlaysgreen = us $ tag (TagGroup 0x6000) (TagElement 0x1202)

overlaysblue :: B.ByteString -> Element
overlaysblue = us $ tag (TagGroup 0x6000) (TagElement 0x1203)

roiarea :: Int -> Element
roiarea = is $ tag (TagGroup 0x6000) (TagElement 0x1301)

roimean :: B.ByteString -> Element
roimean = ds $ tag (TagGroup 0x6000) (TagElement 0x1302)

roistandarddeviation :: B.ByteString -> Element
roistandarddeviation = ds $ tag (TagGroup 0x6000) (TagElement 0x1303)

overlaydata :: B.ByteString -> Element
overlaydata = ow $ tag (TagGroup 0x6000) (TagElement 0x3000)

group6000commentsret :: String -> Element
group6000commentsret = sh $ tag (TagGroup 0x6000) (TagElement 0x4000)

group7fe0length :: B.ByteString -> Element
group7fe0length = ul $ tag (TagGroup 0x7FE0) (TagElement 0x0000)

pixeldata :: B.ByteString -> Element
pixeldata = ob $ tag (TagGroup 0x7FE0) (TagElement 0x0010)

