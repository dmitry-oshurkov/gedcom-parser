{-# LANGUAGE TypeOperators #-}
module Lib where

import Text.Regex.PCRE
import Data.Label
import Data.List.Split
import Data.Time
import Model


splitContent contents = map parseTag $ splitOn "\n" contents

parseTag src =
    (level, tag, value)
    where
    rq = (src :: String) =~ "(?<LEVEL>[0-9]{1}){1}\\s+(?<TAG>[A-Z0-9@_]{3,})?\\s*(?<VALUE>.+)?" :: [[String]]
    match = head rq
    level = read (match!!1) :: Int
    tag = match!!2
    value = match!!3


parseBody obj startLevel nextTags result continue parse
    | null nextTags = continue obj
    | level == startLevel = continue obj
    | level == (startLevel + 1) = parse obj (level, tag, value) nextTags result self
    | otherwise = self obj
    where
    self o = parseBody o startLevel (tail nextTags) result continue parse
    (level, tag, value) = head nextTags


modifyList continue obj field val = continue $ modify field (++ [val]) obj
setField continue obj field val = continue $ set field val obj
setField' continue obj field val = setField continue obj field (Just val)


parseGEDCOM (level, xref, tag) nextTags result
    | null nextTags = result
    | level == 0 && head xref == '@' = parseTopLevel (level, xref, tag) nextTags result self
    | otherwise = self result
    where
    self = parseGEDCOM (head nextTags) (tail nextTags)


parseTopLevel (level, xref, tag) nextTags result continue
    | tag == "INDI" = bodyOf'' (newPerson xref) continue' parsePerson
    | tag == "FAM" = bodyOf'' (newFamily tag) continue'' parseFamily
    | otherwise = continue result
    where
    (people, families) = result
    continue' o = continue (people ++ [o], families)
    continue'' o = continue (people, families ++ [o])
    bodyOf'' newObj = parseBody newObj level nextTags result


parsePerson obj (level, tag, value) nextTags result continue
    | tag == "RESN" = setField'' resn (parseResn value)
    | tag == "NAME" = modifyList'' names newName parseName
    | tag == "SEX" = setField'' gender (parseGender value)
--     +1 <<INDIVIDUAL_EVENT_STRUCTURE>>  {0:M}
--     +1 <<INDIVIDUAL_ATTRIBUTE_STRUCTURE>>  {0:M}
--     +1 <<LDS_INDIVIDUAL_ORDINANCE>>  {0:M}
    | tag == "FAMC" = modifyList'' childToFamilyLinks newChildToFamilyLink parseChildToFamilyLink
    | tag == "FAMS" = modifyList'' spouseToFamilyLinks newSpouseToFamilyLink parseSpouseToFamilyLink
    | tag == "SUBM" = modifyList' submitters value
    | tag == "ASSO" = modifyList'' associations newAssociation parseAssociation
    | tag == "ALIA" = modifyList' aliases value
    | tag == "ANCI" = modifyList' ancestorsInterests value
    | tag == "DESI" = modifyList' descendantsInterests value
    | tag == "OBJE" = modifyList'' personMultimediaLinks newMultimediaLink parseMultimediaLink
    | tag `elem` ["SOUR", "NOTE"] = parseCommon2 obj (level, tag, value) nextTags result continue personSourceCitations personNotes
    | tag == "RFN" = setField''' recordFileNumber value
    | tag == "AFN" = setField''' ancestralFileNumber value
    | tag == "REFN" = modifyList'' personUserReferenceNumbers newUserReferenceNumber parseUserReferenceNumber
    | tag == "RIN" = setField''' recIdNumber (read value :: Int)
    | tag == "CHAN" = parseBody' personChangeDate setField''' newChangeDate parseChangeDate
    | otherwise = continue obj
    where
    parseBody' field setFieldFun newObj = parseBody newObj level (tail nextTags) result (setFieldFun field)
    setField'' = setField continue obj
    setField''' = setField' continue obj
    modifyList' = modifyList continue obj
    modifyList'' field newObjFun = parseBody' field modifyList' (newObjFun value)


parseAssociation obj (level, tag, value) nextTags result continue
    | tag == "RELA" = continue $ set relationIsDescriptor (Just value) obj
    | tag `elem` ["SOUR", "NOTE"] = parseCommon2 obj (level, tag, value) nextTags result continue assocSourceCitations assocNotes
    | otherwise = continue obj


parseChildToFamilyLink obj (level, tag, value) nextTags result continue
    | tag == "PEDI" = continue $ set ctflPedigreeLinkageType (Just $ parsePedigreeLinkageType value) obj
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue ctflNotes
    | otherwise = continue obj


parseSpouseToFamilyLink obj (level, tag, value) nextTags result continue
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue stflNotes
    | otherwise = continue obj


parseUserReferenceNumber obj (level, tag, value) nextTags result continue
    | tag == "TYPE" = continue $ set refnType (Just value) obj
    | otherwise = continue obj


parseChangeDate obj (level, tag, value) nextTags result continue
    | tag == "DATE" = parseBody' ("%e %b %Y", value) continue' parseExactDateTime
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue changeNotes
    | otherwise = continue obj
    where
    continue' (fmt, val) = continue $ set changeDate (parseTimeM True defaultTimeLocale fmt val) obj
    parseBody' newObj = parseBody newObj level (tail nextTags) result


parseExactDateTime (fmt, val) (level, tag, value) nextTags result continue
    | tag == "TIME" = continue (fmt ++ " %k:%M:%S", val ++ " " ++ value)
    | otherwise = continue (fmt, val)


parseName obj (level, tag, value) nextTags result continue
    | tag == "NPFX" = set' npfx
    | tag == "GIVN" = set' givn
    | tag == "NICK" = set' nick
    | tag == "SPFX" = set' spfx
    | tag == "SURN" = set' surn
    | tag == "NSFX" = set' nsfx
    | tag `elem` ["SOUR", "NOTE"] = parseCommon2 obj (level, tag, value) nextTags result continue nameSourceCitations nameNotes
    | otherwise = continue obj
    where
    set' fld = continue $ set fld (Just value) obj


parseSourceCitation obj (level, tag, value) nextTags result continue
    | tag == "PAGE" = setField''' page (read value :: Int)
    | tag == "EVEN" = parseBody' event setField''' (newEvent (parseEventType value) value) parseEvent -- EVEN [  <EVENT_TYPE_INDIVIDUAL> | <EVENT_TYPE_FAMILY> | <ATTRIBUTE_TYPE> ]        -- ATTRIBUTE_TYPE: = {Size=1:4}               [ CAST | EDUC | NATI | OCCU | PROP | RELI | RESI | TITL ]
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue srcNotes
    | tag `elem` ["CONC", "CONT"] = parseCommon obj tag value continue description
    | tag == "TEXT" = parseBody' srcTexts modifyList' value parseText
    | tag == "QUAY" = setField''' dataQuality (parseCertaintyAssessment value)
    | tag == "OBJE" = modifyList'' srcMultimediaLinks newMultimediaLink parseMultimediaLink
    | tag == "DATA" = parseBody' dat setField''' newData parseData
    | otherwise = continue obj
    where
    parseBody' field setFieldFun newObj = parseBody newObj level (tail nextTags) result (setFieldFun field)
    setField''' = setField' continue obj
    modifyList' = modifyList continue obj
    modifyList'' field newObjFun = parseBody' field modifyList' (newObjFun value)


parseData obj (level, tag, value) nextTags result continue
    | tag == "DATE" = continue $ set dataDate (Just $ parseDate value) obj
    | tag == "TEXT" = parseBody' value continue' parseText
    | otherwise = continue obj
    where
    continue' o = continue $ modify dataTexts (++ [o]) obj
    parseBody' newObj = parseBody newObj level (tail nextTags) result


parseDate val
    | tag `elem` ["BEF", "AFT"] = newRangeDate (parseDateValue value) (parseDateRange tag)
    | tag == "BET" = newBetweenDate (parseDateValue match'!!1) (parseDateValue match'!!2)
    | tag `elem` ["ABT", "CAL", "EST"] = newApproxDate (parseDateValue value) (parseDateApproximated tag)
    | tag `elem` ["FROM", "TO"] = periodDate
    | tag == "INT" = newInterpretedDatePhrase (parseDateValue match''''!!1) (match''''!!2)
    | not (null rq''') = newDatePhrase $ match'''!!1
    | otherwise = newDate $ parseDateValue val
    where
    rq = (val :: String) =~ "(?<TAG>[A-Z_]{2,})?\\s+(?<VALUE>.+)?" :: [[String]]
    match = head rq
    tag = match!!1
    value = match!!2

    rq' = (value :: String) =~ "(?<DATE1>.+)?\\s+AND\\s+(?<DATE2>.+)?" :: [[String]]
    match' = head rq'

    rq'' = (value :: String) =~ "(?<DATE1>.+)?\\s+TO\\s+(?<DATE2>.+)?" :: [[String]]
    match'' = head rq''
    isNotMatch = null rq''
    periodDate
        | tag == "FROM" && not isNotMatch = newFromToDate (parseDateValue match''!!1) (parseDateValue match''!!2)
        | tag == "FROM" && isNotMatch = newPeriodDate value From
        | tag == "TO" && isNotMatch = newPeriodDate value To
        | otherwise = error $ "Unexpected DATE_PERIOD {" ++ val ++ "}"

    rq''' = (val :: String) =~ "\\((?<DATE_PHRASE>.+)?\\)" :: [[String]]
    match''' = head rq'''

    rq'''' = (value :: String) =~ "(?<DATE1>.+)?\\s+\\((?<DATE_PHRASE>.+)?\\)" :: [[String]]
    match'''' = head rq''''


parseDateValue val = val


parseMultimediaLink obj (level, tag, value) nextTags result continue
    | tag == "FORM" = continue $ case parseMultimediaFormat value of { Custom -> set customFormat (Just value) (set format (Just Custom) obj); _ -> set format (Just $ parseMultimediaFormat value) obj }
    | tag == "TITL" = continue $ set descriptiveTitle (Just value) obj
    | tag == "FILE" = continue $ set multimediaFileReference (Just value) obj
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue notes
    | otherwise = continue obj


parseText obj (level, tag, value) nextTags result continue
    | tag == "CONC" = continue $ obj ++ value
    | tag == "CONT" = continue $ obj ++ "\n" ++ value
    | otherwise = continue obj


parseNote obj (level, tag, value) nextTags result continue
    | tag `elem` ["CONC", "CONT"] = parseCommon obj tag value continue submitterText
    | tag == "SOUR" = parseSOUR obj level value nextTags result continue noteSourceCitations
    | otherwise = continue obj


parseCommon obj tag value continue text
    | tag == "CONC" = modify' (++ value)
    | tag == "CONT" = modify' (++ "\n" ++ value)
    | otherwise = continue obj
    where
    modify' val = continue $ modify text val obj


parseCommon2 :: (Eq t3, Num t3) => t4 -> (t3, String, String) -> [(t3, String, String)] -> (t1, t2) -> (t4 -> t) -> t4 :-> [SourceCitation] -> t4 :-> [Note] -> t
parseCommon2 obj (level, tag, value) nextTags result continue sourceCitations notes
    | tag == "SOUR" = parseSOUR obj level value nextTags result continue sourceCitations
    | tag == "NOTE" = parseNOTE obj level value nextTags result continue notes
    | otherwise = continue obj


parseSOUR obj level value nextTags result continue sourceCitations7 =
    parseBody' (newSourceCitation value) continue' parseSourceCitation
    where
    continue' o = continue $ modify sourceCitations7 (++ [o]) obj
    parseBody' newObj = parseBody newObj level (tail nextTags) result


parseNOTE :: (Eq t4, Num t4) => t5 -> t4 -> String -> [(t4, String, String)] -> (t1, t2) -> (t5 -> t) -> t5 :-> [Note] -> t
parseNOTE obj level value nextTags result continue notes =
    parseBody' (newNote value) continue' parseNote
    where
    continue' o = continue $ modify notes (++ [o]) obj
    parseBody' newObj = parseBody newObj level (tail nextTags) result


parseEvent obj (level, tag, value) nextTags result continue
    | tag == "ROLE" = continue obj
    | otherwise = continue obj


parseResn val
    | val == "locked" = Locked
    | val == "privacy" = Privacy
    | otherwise = error $ "Unexpected RESN {" ++ val ++ "}"


parseGender val
    | val == "M" = Male
    | val == "F" = Female
    | otherwise = error $ "Unexpected SEX {" ++ val ++ "}"


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


parseCertaintyAssessment val
    | val == "1" = Questionable
    | val == "2" = Secondary
    | val == "3" = Direct
    | otherwise = Unreliable


parseMultimediaFormat val
    | val == "bmp" = Bmp
    | val == "gif" = Gif
    | val == "jpeg" = Jpeg
    | val == "ole" = Ole
    | val == "pcx" = Pcx
    | val == "tiff" = Tiff
    | val == "wav" = Wav
    | otherwise = Custom


parseDateApproximated val
    | val == "ABT" = About
    | val == "CAL" = Calculated
    | val == "EST" = Estimated
    | otherwise = error $ "Unexpected DATE_APPROXIMATED {" ++ val ++ "}"


parseDateRange val
    | val == "BEF" = Before
    | val == "AFT" = After
    | val == "BET" = Between
    | otherwise = error $ "Unexpected DATE_RANGE {" ++ val ++ "}"


parsePedigreeLinkageType val
    | val == "adopted" = Adopted
    | val == "birth" = Birth
    | val == "foster" = Foster
    | val == "sealing" = Sealing
    | otherwise = error $ "Unexpected PEDIGREE_LINKAGE_TYPE {" ++ val ++ "}"


parseFamily obj (level, tag, value) nextTags result continue =
    continue obj