{-# LANGUAGE TemplateHaskell #-}
module Model where

import Data.Label


data Person = Person {
    _xref :: String,
    _resn :: Resn,
    _names :: [Name],
    _gender :: Gender
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
    --     +1 <<SOURCE_CITATION>>  {0:M}
    --     +1 <<MULTIMEDIA_LINK>>  {0:M}
    --     +1 <<NOTE_STRUCTURE>>  {0:M}
    --     +1 RFN <PERMANENT_RECORD_FILE_NUMBER>  {0:1}
    --     +1 AFN <ANCESTRAL_FILE_NUMBER>  {0:1}
    --     +1 REFN <USER_REFERENCE_NUMBER>  {0:M}
    --       +2 TYPE <USER_REFERENCE_TYPE>  {0:1}
    --     +1 RIN <AUTOMATED_RECORD_ID>  {0:1}
    --     +1 <<CHANGE_DATE>>  {0:1}
} deriving (Show)

newPerson xref = Person xref Free [] UnknownGender


data Resn = Free | Locked | Privacy deriving (Show, Eq)
data Gender = UnknownGender | Male | Female deriving (Show, Eq)


data Name = Name {
    _value :: String,
    _npfx :: String,
    _givn :: String,
    _nick :: String,
    _spfx :: String,
    _surn :: String,
    _nsfx :: String,
    _sourceCitations :: [SourceCitation]
--       +2 <<NOTE_STRUCTURE>>  {0:M}
--       +2 <<MULTIMEDIA_LINK>>  {0:M}
--     +1 <<NOTE_STRUCTURE>>  {0:M}
} deriving (Show, Eq)

newName value = Name value "" "" "" "" "" "" []


data SourceCitation = SourceCitation {
    srccitXref :: String,
    srccitPage :: Int,
    srccitEvent :: Maybe Event
} deriving (Show, Eq)

newSourceCitation = SourceCitation {
    srccitXref = "",
    srccitPage = 0,
    srccitEvent = Nothing
}


data Event = Event {
    eventType :: EventType,
    customEventType :: Maybe String,
    eventRole :: Maybe RelationshipRole,
    customEventRole :: Maybe String
} deriving (Show, Eq)

newEvent = Event {
    eventType = CustomEventType,
    customEventType = Nothing,
    eventRole = Nothing,
    customEventRole = Nothing
}


data RelationshipRole = Child | Husband | Wife | Mother | Father | Spouse | CustomRelationshipRole deriving (Show, Eq)
data EventType = Anul | Cens | Div | Divf | Enga | Marr | Marb | Marc | Marl | Mars | Adop | Birt | Bapm | Barm | Basm | Bles | Buri | Chr | Chra | Conf | Crem | Deat | Emig | Fcom | Grad | Immi | Natu | Ordn | Reti | Prob | Will | Even | CustomEventType deriving (Show, Eq)


data Family = Family {
    fval :: String
} deriving (Show)


mkLabels [ ''Person, ''Name, ''SourceCitation, ''Event, ''Family ]