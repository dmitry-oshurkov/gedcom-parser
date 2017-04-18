module Lib where

import Text.Regex.PCRE
import Data.Label
import Model


parseTag src =
    (level, tag, value)
    where
    rq = (src :: String) =~ "(?<LEVEL>[0-9]{1}){1}\\s+(?<TAG>[A-Z0-9@_]{3,})?\\s*(?<VALUE>.+)?" :: [[String]]
    match = head rq
    level = read (match!!1) :: Int
    tag = match!!2
    value = match!!3


bodyOf obj startLevel nextTags (people, families) continue parseBody
    | null nextTags = continue obj
    | level == startLevel = continue obj
    | otherwise = parseBody obj (level, tag, value) nextTags (people, families) self
    where
    self o = bodyOf o startLevel (tail nextTags) (people, families) continue parseBody
    (level, tag, value) = head nextTags


parseLine (level, xref, tag) nextTags (people, families)
    | null nextTags = (people, families)
    | level == 0 && head xref == '@' = parseTopLevel (level, xref, tag) nextTags (people, families) self
    | otherwise = self (people, families)
    where
    self = parseLine (head nextTags) (tail nextTags)


parseTopLevel (level, xref, tag) nextTags (people, families) continue
    | tag == "INDI" = bodyOf (newPerson xref) level nextTags (people, families) continue' parsePerson
    | tag == "FAM" = bodyOf (newFamily tag) level nextTags (people, families) continue'' parseFamily
    | otherwise = continue (people, families)
    where
    continue' o = continue (people ++ [o], families)
    continue'' o = continue (people, families ++ [o])


parsePerson obj (level, tag, value) nextTags (people, families) continue
    | tag == "RESN" = continue $ set resn (parseResn value) obj
    | tag == "NAME" = bodyOf (newName value) level (tail nextTags) (people, families) continue' parseName
    | tag == "SEX" = continue $ set gender (parseGender value) obj
    | otherwise = continue obj
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
    where
    continue' o = continue $ modify names (++ [o]) obj


parseResn val
    | val == "locked" = Locked
    | val == "privacy" = Privacy
    | otherwise = error "Unexpected RESN"


parseGender val
    | val == "M" = Male
    | val == "F" = Female
    | otherwise = error "Unexpected SEX"


parseName obj (level, tag, value) nextTags (people, families) continue
    | tag == "NPFX" = continue $ set npfx value obj
    | tag == "GIVN" = continue $ set givn value obj
    | tag == "NICK" = continue $ set nick value obj
    | tag == "SPFX" = continue $ set spfx value obj
    | tag == "SURN" = continue $ set surn value obj
    | tag == "NSFX" = continue $ set nsfx value obj
    | tag == "SOUR" = bodyOf (newSourceCitation value) level (tail nextTags) (people, families) continue' parseSourceCitation
    | tag == "NOTE" && head value == '@' = bodyOf (newNote1 value) level (tail nextTags) (people, families) continue'' parseNote1 -- todo: combine parseNote1, parseNote2?
    | tag == "NOTE" && head value /= '@' = bodyOf (newNote2 value) level (tail nextTags) (people, families) continue'' parseNote2
    | otherwise = continue obj
    where
    continue' o = continue $ modify sourceCitations (++ [o]) obj
    continue'' o = continue $ modify notes (++ [o]) obj


parseSourceCitation obj (level, tag, value) nextTags (people, families) continue
    | tag == "PAGE" = continue $ set page (read value :: Int) obj
    | tag == "EVEN" = bodyOf (justNewEvent (parseEventType value) value) level (tail nextTags) (people, families) continue' parseEvent -- EVEN [  <EVENT_TYPE_INDIVIDUAL> | <EVENT_TYPE_FAMILY> | <ATTRIBUTE_TYPE> ]        -- ATTRIBUTE_TYPE: = {Size=1:4}               [ CAST | EDUC | NATI | OCCU | PROP | RELI | RESI | TITL ]
    | otherwise = continue obj
-- n SOUR @<XREF:SOUR>@    /* pointer to source record */  {1:1}
--       +2 ROLE <ROLE_IN_EVENT>  {0:1}
--     +1 DATA        {0:1}
--       +2 DATE <ENTRY_RECORDING_DATE>  {0:1}
--       +2 TEXT <TEXT_FROM_SOURCE>  {0:M}
--         +3 [ CONC | CONT ] <TEXT_FROM_SOURCE>  {0:M}
--     +1 QUAY <CERTAINTY_ASSESSMENT>  {0:1}
--     +1 <<MULTIMEDIA_LINK>>  {0:M}

--   |              /* Systems not using source records */
--   n SOUR <SOURCE_DESCRIPTION>  {1:1}
--     +1 [ CONC | CONT ] <SOURCE_DESCRIPTION>  {0:M}
--     +1 TEXT <TEXT_FROM_SOURCE>  {0:M}
--        +2 [CONC | CONT ] <TEXT_FROM_SOURCE>  {0:M}
--     +1 <<NOTE_STRUCTURE>>  {0:M}
    where
    continue' o = continue $ set event o obj


parseNote1 obj (level, tag, value) nextTags (people, families) continue
    | tag == "SOUR" = bodyOf (newSourceCitation value) level (tail nextTags) (people, families) continue' parseSourceCitation
    | otherwise = continue obj
    where
    continue' o = continue $ modify sourceCitations2 (++ [o]) obj

parseNote2 obj (level, tag, value) nextTags (people, families) continue
    | tag == "CONC" = continue $ modify text (++ value) obj
    | tag == "CONT" = continue $ modify text (++ "\n" ++ value) obj
    | tag == "SOUR" = bodyOf (newSourceCitation value) level (tail nextTags) (people, families) continue' parseSourceCitation
    | otherwise = continue obj
    where
    continue' o = continue $ modify sourceCitations2 (++ [o]) obj


parseEvent obj (level, tag, value) nextTags (people, families) continue
    | tag == "ROLE" = continue obj
    | otherwise = continue obj


parseRelationshipRole val
    | val == "CHIL" = Child
    | val == "HUSB" = Husband
    | val == "WIFE" = Wife
    | val == "MOTH" = Mother
    | val == "FATH" = Father
    | val == "SPOU" = Spouse
    | otherwise = CustomRelationshipRole


parseEventType val
    | val == "ANUL" = Anul
    | val == "CENS" = Cens
    | val == "DIV" = Div
    | val == "DIVF" = Divf
    | val == "ENGA" = Enga
    | val == "MARR" = Marr
    | val == "MARB" = Marb
    | val == "MARC" = Marc
    | val == "MARL" = Marl
    | val == "MARS" = Mars
    | val == "ADOP" = Adop
    | val == "BIRT" = Birt
    | val == "BAPM" = Bapm
    | val == "BARM" = Barm
    | val == "BASM" = Basm
    | val == "BLES" = Bles
    | val == "BURI" = Buri
    | val == "CHR" = Chr
    | val == "CHRA" = Chra
    | val == "CONF" = Conf
    | val == "CREM" = Crem
    | val == "DEAT" = Deat
    | val == "EMIG" = Emig
    | val == "FCOM" = Fcom
    | val == "GRAD" = Grad
    | val == "IMMI" = Immi
    | val == "NATU" = Natu
    | val == "ORDN" = Ordn
    | val == "RETI" = Reti
    | val == "PROB" = Prob
    | val == "WILL" = Will
    | val == "EVEN" = Even
    | otherwise = CustomEventType


parseFamily obj (level, tag, value) nextTags (people, families) continue =
    continue obj