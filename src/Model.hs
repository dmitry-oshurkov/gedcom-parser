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
    --     +1 SUBM @<XREF:SUBM>@  {0:M}
    --     +1 <<ASSOCIATION_STRUCTURE>>  {0:M}
    --     +1 ALIA @<XREF:INDI>@  {0:M}
    --     +1 ANCI @<XREF:SUBM>@  {0:M}
    --     +1 DESI @<XREF:SUBM>@  {0:M}
    _personSourceCitations :: [SourceCitation],
    --     +1 <<MULTIMEDIA_LINK>>  {0:M}
    _personNotes :: [Note]
    --     +1 RFN <PERMANENT_RECORD_FILE_NUMBER>  {0:1}
    --     +1 AFN <ANCESTRAL_FILE_NUMBER>  {0:1}
    --     +1 REFN <USER_REFERENCE_NUMBER>  {0:M}
    --       +2 TYPE <USER_REFERENCE_TYPE>  {0:1}
    --     +1 RIN <AUTOMATED_RECORD_ID>  {0:1}
    --     +1 <<CHANGE_DATE>>  {0:1}
} deriving (Show)

newPerson xref = Person xref Free [] UnknownGender [] []


data Resn = Free | Locked | Privacy deriving (Show, Eq)
data Gender = UnknownGender | Male | Female deriving (Show, Eq)


data Name = Name {
    _namePersonal :: String,
    _npfx :: String,
    _givn :: String,
    _nick :: String,
    _spfx :: String,
    _surn :: String,
    _nsfx :: String,
    _nameSourceCitations :: [SourceCitation],
    _nameNotes :: [Note]
} deriving (Show, Eq)

newName namePersonal = Name namePersonal "" "" "" "" "" "" [] []


data SourceCitation = SourceCitation {
    _srcXref :: Maybe String,
    _description :: String,
    _page :: Int,
    _event :: Maybe Event,
    _srcNotes :: [Note]
} deriving (Show, Eq)

newSourceCitation1 xref = SourceCitation (Just xref) "" 0 Nothing []
newSourceCitation2 description = SourceCitation Nothing description 0 Nothing []


data Event = Event {
    _typ :: EventType,
    _customType :: Maybe String,
    _role :: Maybe RelationshipRole,
    _customRole :: Maybe String
} deriving (Show, Eq)

newEvent typ customType = Event typ customType Nothing Nothing
justNewEvent typ customType = Just (newEvent typ (Just customType))


data RelationshipRole = Child | Husband | Wife | Mother | Father | Spouse | CustomRelationshipRole deriving (Show, Eq)
data EventType = Anul | Cens | Div | Divf | Enga | Marr | Marb | Marc | Marl | Mars | Adop | Birt | Bapm | Barm | Basm | Bles | Buri | Chr | Chra | Conf | Crem | Deat | Emig | Fcom | Grad | Immi | Natu | Ordn | Reti | Prob | Will | Even | CustomEventType deriving (Show, Eq)


data Note = Note {
    _noteXref :: Maybe String,
    _submitterText :: String,
    _noteSourceCitations :: [SourceCitation]
} deriving (Show, Eq)

newNote1 xref = Note (Just xref) "" []
newNote2 submitterText = Note Nothing submitterText []


data Family = Family {
    _familyXref :: String
} deriving (Show)

newFamily xref = Family xref


mkLabels [ ''Person, ''Name, ''SourceCitation, ''Event, ''Note, ''Family ]