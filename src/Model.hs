{-# LANGUAGE TemplateHaskell #-}
module Model where

import Data.Label


data Person = Person {
    _personXref :: String,
    _resn :: Resn,
    _names :: [Name],
    _gender :: Gender,
    --     +1 <<INDIVIDUAL_EVENT_STRUCTURE>>  {0:M}
    --     +1 <<INDIVIDUAL_ATTRIBUTE_STRUCTURE>>  {0:M}
    --     +1 <<LDS_INDIVIDUAL_ORDINANCE>>  {0:M}
    --     +1 <<CHILD_TO_FAMILY_LINK>>  {0:M}
    --     +1 <<SPOUSE_TO_FAMILY_LINK>>  {0:M}
    _submitters :: [String],
    --     +1 <<ASSOCIATION_STRUCTURE>>  {0:M}
    _aliases :: [String],
    _ancestorsInterests :: [String],
    _descendantsInterests :: [String],
    _personSourceCitations :: [SourceCitation],
    _personMultimediaLinks :: [MultimediaLink],
    _personNotes :: [Note]
    --     +1 RFN <PERMANENT_RECORD_FILE_NUMBER>  {0:1}
    --     +1 AFN <ANCESTRAL_FILE_NUMBER>  {0:1}
    --     +1 REFN <USER_REFERENCE_NUMBER>  {0:M}
    --       +2 TYPE <USER_REFERENCE_TYPE>  {0:1}
    --     +1 RIN <AUTOMATED_RECORD_ID>  {0:1}
    --     +1 <<CHANGE_DATE>>  {0:1}
} deriving (Show, Eq)

newPerson xref = Person xref Free [] UnknownGender [] [] [] [] [] [] []


data Resn = Free | Locked | Privacy deriving (Show, Eq)
data Gender = UnknownGender | Male | Female deriving (Show, Eq)


data Name = Name {
    _namePersonal :: String,
    _npfx :: Maybe String,
    _givn :: Maybe String,
    _nick :: Maybe String,
    _spfx :: Maybe String,
    _surn :: Maybe String,
    _nsfx :: Maybe String,
    _nameSourceCitations :: [SourceCitation],
    _nameNotes :: [Note]
} deriving (Show, Eq)

newName namePersonal = Name namePersonal Nothing Nothing Nothing Nothing Nothing Nothing [] []


data SourceCitation = SourceCitation {
    _srcXref :: Maybe String,
    _description :: String,
    _page :: Maybe Int,
    _event :: Maybe Event,
    _srcNotes :: [Note],
    _srcTexts :: [String],
    _dataQuality :: Maybe CertaintyAssessment,
    _srcMultimediaLinks :: [MultimediaLink],
    _dat :: Maybe Data
} deriving (Show, Eq)

newSourceCitation1 xref = SourceCitation (Just xref) "" Nothing Nothing [] [] Nothing [] Nothing
newSourceCitation2 description = SourceCitation Nothing description Nothing Nothing [] [] Nothing [] Nothing


data Data = Data {
    _dataDate :: Maybe Date,
    _dataTexts :: [String]
} deriving (Show, Eq)

newData = Data Nothing []


data MultimediaLink = MultimediaLink {
    _mltXref :: Maybe String,
    _format :: Maybe MultimediaFormat,
    _customFormat :: Maybe String,
    _descriptiveTitle :: Maybe String,
    _multimediaFileReference :: Maybe String,
    _notes :: [Note]
} deriving (Show, Eq)

newMultimediaLink1 xref = MultimediaLink (Just xref) Nothing Nothing Nothing Nothing []
newMultimediaLink2 = MultimediaLink Nothing Nothing Nothing Nothing Nothing []


data Event = Event {
    _typ :: EventType,
    _customType :: Maybe String,
    _role :: Maybe RelationshipRole,
    _customRole :: Maybe String
} deriving (Show, Eq)

newEvent typ customType = Event typ (Just customType) Nothing Nothing


data Note = Note {
    _noteXref :: Maybe String,
    _submitterText :: String,
    _noteSourceCitations :: [SourceCitation]
} deriving (Show, Eq)

newNote1 xref = Note (Just xref) "" []
newNote2 submitterText = Note Nothing submitterText []


data Date = Date {
    _firstDate :: Maybe String,
    _secondDate :: Maybe String,
    _datePhrase :: Maybe String,
    _approx :: Maybe DateApproximated,
    _range :: Maybe DateRange,
    _period :: Maybe DatePeriod,
    _isInterpreted :: Maybe Bool
} deriving (Show, Eq)

newDate date = Date (Just date) Nothing Nothing Nothing Nothing Nothing Nothing
newRangeDate firstDate range = Date (Just firstDate) Nothing Nothing Nothing (Just range) Nothing Nothing
newBetweenDate firstDate secondDate = Date (Just firstDate) (Just secondDate) Nothing Nothing (Just Between) Nothing Nothing
newApproxDate firstDate approx = Date (Just firstDate) Nothing Nothing (Just approx) Nothing Nothing Nothing
newPeriodDate firstDate period = Date (Just firstDate) Nothing Nothing Nothing Nothing (Just period) Nothing
newFromToDate firstDate secondDate = Date (Just firstDate) (Just secondDate) Nothing Nothing Nothing (Just FromTo) Nothing
newDatePhrase phrase = Date Nothing Nothing (Just phrase) Nothing Nothing Nothing Nothing
newInterpretedDatePhrase firstDate phrase = Date (Just firstDate) Nothing (Just phrase) Nothing Nothing Nothing (Just True)


data Family = Family {
    _familyXref :: String
} deriving (Show)

newFamily xref = Family xref


data RelationshipRole = Child | Husband | Wife | Mother | Father | Spouse | CustomRelationshipRole deriving (Show, Eq)
data EventType = Anul | Cens | Div | Divf | Enga | Marr | Marb | Marc | Marl | Mars | Adop | Birt | Bapm | Barm | Basm | Bles | Buri | Chr | Chra | Conf | Crem | Deat | Emig | Fcom | Grad | Immi | Natu | Ordn | Reti | Prob | Will | Even | CustomEventType deriving (Show, Eq)
data CertaintyAssessment = Unreliable | Questionable | Secondary | Direct deriving (Show, Eq)
data MultimediaFormat = Bmp | Gif | Jpeg | Ole | Pcx | Tiff | Wav | Custom deriving (Show, Eq)
data DateApproximated = About | Calculated | Estimated deriving (Show, Eq)
data DateRange = Before | After | Between deriving (Show, Eq)
data DatePeriod = From | To | FromTo deriving (Show, Eq)


mkLabels [ ''Person, ''Name, ''SourceCitation, ''Event, ''Note, ''Family, ''MultimediaLink, ''Data, ''Date ]