{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Crypto.Hash
import Data.Time
import Lib
import Model
import JSON


sha1Hex s = digestToHexByteString (hash (B.toStrict s) :: Digest SHA1)
getNextTags = tail . splitContent


main :: IO ()
main = hspec $ do

    describe "parseGEDCOM" $
        it "returns same digest" $ do
            contents <- readFile "test/TGC55CLF-utf8.ged"
            let tags = splitContent contents
            let (people, families) = parseGEDCOM (head tags) (tail tags) ([], [])
            sha1Hex (encode people) `shouldBe` "e46ede53dd4ceab770c7fa89db78727e9d340fe2"
            sha1Hex (encode families) `shouldBe` "1446e611d189d06fce528d57abe8d8f385aa977f"


    describe "parseTag" $ do
        it "returns level, tag, value of a NAME tag" $
            parseTag "1 NAME Lucy Special /ANSEL/" `shouldBe` (1, "NAME", "Lucy Special /ANSEL/")

        it "returns level, tag and empty value of a GEDC tag" $
            parseTag "1 GEDC" `shouldBe` (1, "GEDC", "")

        it "returns level, xref, tag of a INDI tag" $
            parseTag "0 @I14@ INDI" `shouldBe` (0, "@I14@", "INDI")


    describe "parsePerson" $
        it "builds Person record" $ do

            let nextTags = getNextTags   "0 @PERSON2@ INDI\n\
                                            \1 NAME Mary First /Jones/\n\
                                            \1 SEX F\n\
                                            \1 RESN privacy\n\
                                            \1 BIRT\n\
                                                \2 DATE BEF 1970\n\
                                            \1 DEAT\n\
                                                \2 DATE AFT 2000\n\
                                            \1 FAMC @ADOPTIVE_PARENTS@\n\
                                                \2 PEDI adopted\n\
                                                \2 NOTE Note about the link to his adoptive parents family record.\n\
                                            \1 FAMS @FAMILY1@\n\
                                                \2 NOTE Note about the link to the family record with his first spouse.\n\
                                                \2 NOTE Another note about the link to the family record with his first spouse.\n\
                                            \1 SUBM @SUBMITTER@\n\
                                            \1 ALIA @I9@\n\
                                            \1 ANCI @SUBMITTER@\n\
                                            \1 DESI @SUBMITTER@\n\
                                            \1 NOTE @N31@\n\
                                            \1 RFN Record File Number\n\
                                            \1 AFN Ancestral File Number\n\
                                            \1 CHAN\n\
                                                \2 DATE 11 Jan 2001\n\
                                                    \3 TIME 5:08:06\n\
                                                \2 NOTE This date is the last time this record was changed\n\
                                            \1 RIN 8\n\
                                            \1 REFN User Reference Number\n\
                                                \2 TYPE User Reference Type\n\
                                       \0 @I15@ INDI"

            parseBody (newPerson "@PERSON2@") 0 nextTags ([], []) id parsePerson
                `shouldBe` (newPerson "@PERSON2@") {
                                _personXref = "@PERSON2@",
                                _resn = Privacy,
                                _names = [ newName "Mary First /Jones/" ],
                                _gender = Female,
                                _childToFamilyLinks = [ (newChildToFamilyLink "@ADOPTIVE_PARENTS@") {
                                                                _ctflPedigreeLinkageType = Just Adopted,
                                                                _ctflNotes = [ newNote2 "Note about the link to his adoptive parents family record." ]
                                                        }
                                ],
                                _spouseToFamilyLinks = [ (newSpouseToFamilyLink "@FAMILY1@") { _stflNotes = [ newNote2 "Note about the link to the family record with his first spouse.", newNote2 "Another note about the link to the family record with his first spouse." ] } ],
                                _submitters = [ "@SUBMITTER@" ],
                                _aliases = [ "@I9@" ],
                                _ancestorsInterests = [ "@SUBMITTER@" ],
                                _descendantsInterests = [ "@SUBMITTER@" ],
                                _personNotes = [ newNote1 "@N31@" ],
                                _recordFileNumber = Just "Record File Number",
                                _ancestralFileNumber = Just "Ancestral File Number",
                                _personChangeDate = Just newChangeDate {
                                                                _changeDate = Just (UTCTime (fromGregorian 2001 01 11) (secondsToDiffTime 18486)),
                                                                _changeNotes = [ newNote2 "This date is the last time this record was changed" ]
                                                            },
                                _personUserReferenceNumbers = [ (newUserReferenceNumber "User Reference Number") { _refnType = Just "User Reference Type" } ],
                                _recIdNumber = Just 8
                           }


    describe "parseName" $
        it "builds Name record" $ do
            {-
                1 NAME Joseph Tag /Torture/
                    2 NPFX Prof.
                    2 GIVN Joseph
                    2 NICK Joe
                    2 SPFX Le
                    2 SURN Torture
                    2 NSFX Jr.
                1 SEX M
            -}
            let nextTags = [
                            (2, "NPFX", "Prof."),
                            (2, "GIVN", "Joseph"),
                            (2, "NICK", "Joe"),
                            (2, "SPFX", "Le"),
                            (2, "SURN", "Torture"),
                            (2, "NSFX", "Jr."),
                            (1, "SEX", "M")
                        ]
            parseBody (newName "Joseph Tag /Torture/") 1 nextTags ([], []) id parseName
                `shouldBe` Name "Joseph Tag /Torture/" (Just "Prof.") (Just "Joseph") (Just "Joe") (Just "Le") (Just "Torture") (Just "Jr.") [] []

    describe "parseSourceCitation" $ do
        context "on first case" $
            it "builds SourceCitation record" $ do
                {-
                    2 SOUR @SOURCE1@
                        3 PAGE 42
                        3 EVEN Event type cited in source
                    2 NOTE T
                -}
                let nextTags = [
                                (3, "PAGE", "42"),
                                (3, "EVEN", "Event type cited in source"),
                                (2, "NOTE", "T")
                            ]
                parseBody (newSourceCitation1 "@SOURCE1@") 2 nextTags ([], []) id parseSourceCitation
                    `shouldBe` SourceCitation (Just "@SOURCE1@") "" (Just 42) (Just (Event CustomEventType (Just "Event type cited in source") Nothing Nothing)) [] [] Nothing [] Nothing

        context "on second case" $
            it "builds SourceCitation record" $ do

                let nextTags = getNextTags "1 SOUR @SOURCE1@\n\
                                                \2 CONC s embedded\n\
                                                \2 CONT in the record\n\
                                                \2 TEXT Text from a source. The preferred approach is to cite sources by\n\
                                                    \3 CONC links to SOURCE records.\n\
                                                    \3 CONT Here is a new line of text from the source.\n\
                                                \2 NOTE @N17@\n\
                                                \2 DATA\n\
                                                    \3 DATE 1 JAN 1900\n\
                                                    \3 TEXT Here is some text from the source specific to this source\n\
                                                        \4 CONC citation.\n\
                                                        \4 CONT Here is more text but on a new line.\n\
                                                \2 QUAY 0\n\
                                         \1 OBJE"

                parseBody (newSourceCitation2 "This source i") 1 nextTags ([], []) id parseSourceCitation
                    `shouldBe` (newSourceCitation2 "This source is embedded\nin the record") {

                                   _srcNotes = [ newNote1 "@N17@" ],
                                   _srcTexts = [ "Text from a source. The preferred approach is to cite sources bylinks to SOURCE records.\nHere is a new line of text from the source." ],
                                   _dataQuality = Just Unreliable,
                                   _dat = Just newData {
                                        _dataDate = Just $ newDate "1 JAN 1900",
                                        _dataTexts = [ "Here is some text from the source specific to this sourcecitation.\nHere is more text but on a new line." ]
                                   }
                               }


    describe "parseNote" $ do
        context "on first case" $
            it "builds Note record" $ do
                {-
                    4 NOTE @N26@
                    4 FILE ImgFile.JPG
                -}
                let nextTags = [
                                (4, "FILE", "ImgFile.JPG")
                            ]
                parseBody (newNote1 "@N26@") 4 nextTags ([], []) id parseNote
                    `shouldBe` Note (Just "@N26@") "" []

        context "on second case" $
            it "builds Note record" $ do
                {-
                    2 NOTE These are notes about the first NAME structure in this record. These notes are
                        3 CONC embedded in the INDIVIDUAL record itself.
                        3 CONT
                        3 CONT The second name structure in this record uses all possible tags for a personal name
                        3 CONC structure.
                        3 CONT
                        3 CONT NOTE: many applications are confused by two NAME structures.
                    1 SEX M
                -}
                let nextTags = [
                                (3, "CONC", "embedded in the INDIVIDUAL record itself."),
                                (3, "CONT", ""),
                                (3, "CONT", "The second name structure in this record uses all possible tags for a personal name "),
                                (3, "CONC", "structure."),
                                (3, "CONT", ""),
                                (3, "CONT", "NOTE: many applications are confused by two NAME structures."),
                                (1, "SEX", "M")
                            ]
                parseBody (newNote2 "These are notes about the first NAME structure in this record. These notes are ") 2 nextTags ([], []) id parseNote
                    `shouldBe` Note Nothing "These are notes about the first NAME structure in this record. These notes are embedded in the INDIVIDUAL record itself.\n\nThe second name structure in this record uses all possible tags for a personal name structure.\n\nNOTE: many applications are confused by two NAME structures." []


    describe "bodyOf" $
        it "parses next level tags only" $ do

            let nextTags = splitContent  "2 SOUR @SOURCE1@\n\
                                            \3 PAGE 55\n\
                                            \3 OBJE\n\
                                                \4 NOTE @N26@\n\
                                            \3 NOTE @N7@\n\
                                        \2 NOTE This\n\
                                     \1 NAME Barry"

            parseBody (newName "Villy") 1 nextTags ([], []) id parseName
                `shouldBe` (newName "Villy") {

                                _nameSourceCitations = [

                                    (newSourceCitation1 "@SOURCE1@") {

                                        _page = Just 55,
                                        _srcNotes = [ newNote1 "@N7@" ],
                                        _srcMultimediaLinks = [ newMultimediaLink2 {
                                                                    _notes = [ newNote1 "@N26@" ]
                                                                }
                                        ]
                                    }
                                ],
                                _nameNotes = [ newNote2 "This" ]
                            }


    describe "parseRelationshipRole" $
        it "returns valid value" $ do
            parseRelationshipRole "CHIL" `shouldBe` Child
            parseRelationshipRole "HUSB" `shouldBe` Husband
            parseRelationshipRole "WIFE" `shouldBe` Wife
            parseRelationshipRole "MOTH" `shouldBe` Mother
            parseRelationshipRole "FATH" `shouldBe` Father
            parseRelationshipRole "SPOU" `shouldBe` Spouse
            parseRelationshipRole "Role in cited event" `shouldBe` CustomRelationshipRole


    describe "parseEventType" $
        it "returns valid value" $ do
            parseEventType "ANUL" `shouldBe` Anul
            parseEventType "CENS" `shouldBe` Cens
            parseEventType "DIV" `shouldBe` Div
            parseEventType "DIVF" `shouldBe` Divf
            parseEventType "ENGA" `shouldBe` Enga
            parseEventType "MARR" `shouldBe` Marr
            parseEventType "MARB" `shouldBe` Marb
            parseEventType "MARC" `shouldBe` Marc
            parseEventType "MARL" `shouldBe` Marl
            parseEventType "MARS" `shouldBe` Mars
            parseEventType "ADOP" `shouldBe` Adop
            parseEventType "BIRT" `shouldBe` Birt
            parseEventType "BAPM" `shouldBe` Bapm
            parseEventType "BARM" `shouldBe` Barm
            parseEventType "BASM" `shouldBe` Basm
            parseEventType "BLES" `shouldBe` Bles
            parseEventType "BURI" `shouldBe` Buri
            parseEventType "CHR" `shouldBe` Chr
            parseEventType "CHRA" `shouldBe` Chra
            parseEventType "CONF" `shouldBe` Conf
            parseEventType "CREM" `shouldBe` Crem
            parseEventType "DEAT" `shouldBe` Deat
            parseEventType "EMIG" `shouldBe` Emig
            parseEventType "FCOM" `shouldBe` Fcom
            parseEventType "GRAD" `shouldBe` Grad
            parseEventType "IMMI" `shouldBe` Immi
            parseEventType "NATU" `shouldBe` Natu
            parseEventType "ORDN" `shouldBe` Ordn
            parseEventType "RETI" `shouldBe` Reti
            parseEventType "PROB" `shouldBe` Prob
            parseEventType "WILL" `shouldBe` Will
            parseEventType "EVEN" `shouldBe` Even
            parseEventType "Event type cited in source" `shouldBe` CustomEventType


    describe "parseResn" $ do
        context "when value is known" $
            it "returns valid value" $ do
                parseResn "locked" `shouldBe` Locked
                parseResn "privacy" `shouldBe` Privacy

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseResn "abracadabra") `shouldThrow` errorCall "Unexpected RESN {abracadabra}"


    describe "parseGender" $ do
        context "when value is known" $
            it "returns valid value" $ do
                parseGender "M" `shouldBe` Male
                parseGender "F" `shouldBe` Female

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseGender "abracadabra") `shouldThrow` errorCall "Unexpected SEX {abracadabra}"


    describe "parseCertaintyAssessment" $ do
        context "when valid input" $
            it "returns valid value" $ do
                parseCertaintyAssessment "0" `shouldBe` Unreliable
                parseCertaintyAssessment "1" `shouldBe` Questionable
                parseCertaintyAssessment "2" `shouldBe` Secondary
                parseCertaintyAssessment "3" `shouldBe` Direct

        context "when unknown input" $
            it "returns Unreliable value" $
                parseCertaintyAssessment "ertewrt" `shouldBe` Unreliable


    describe "parseMultimediaFormat" $
        it "returns valid value" $ do
            parseMultimediaFormat "bmp" `shouldBe` Bmp
            parseMultimediaFormat "gif" `shouldBe` Gif
            parseMultimediaFormat "jpeg" `shouldBe` Jpeg
            parseMultimediaFormat "ole" `shouldBe` Ole
            parseMultimediaFormat "pcx" `shouldBe` Pcx
            parseMultimediaFormat "tiff" `shouldBe` Tiff
            parseMultimediaFormat "wav" `shouldBe` Wav
            parseMultimediaFormat "abracadabra" `shouldBe` Custom


    describe "parseDateApproximated" $ do
        context "when valid input" $
            it "returns valid value" $ do
                parseDateApproximated "ABT" `shouldBe` About
                parseDateApproximated "CAL" `shouldBe` Calculated
                parseDateApproximated "EST" `shouldBe` Estimated

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseDateApproximated "abracadabra") `shouldThrow` errorCall "Unexpected DATE_APPROXIMATED {abracadabra}"


    describe "parseDateRange" $ do
        context "when valid input" $
            it "returns valid value" $ do
                parseDateRange "BEF" `shouldBe` Before
                parseDateRange "AFT" `shouldBe` After
                parseDateRange "BET" `shouldBe` Between

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseDateRange "abracadabra") `shouldThrow` errorCall "Unexpected DATE_RANGE {abracadabra}"


    describe "parsePedigreeLinkageType" $ do
        context "when valid input" $
            it "returns valid value" $ do
                parsePedigreeLinkageType "adopted" `shouldBe` Adopted
                parsePedigreeLinkageType "birth" `shouldBe` Birth
                parsePedigreeLinkageType "foster" `shouldBe` Foster
                parsePedigreeLinkageType "sealing" `shouldBe` Sealing

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parsePedigreeLinkageType "abracadabra") `shouldThrow` errorCall "Unexpected PEDIGREE_LINKAGE_TYPE {abracadabra}"


    describe "parseMultimediaLink" $
        it "builds MultimediaLink record" $ do

            let nextTags = getNextTags "3 OBJE\n\
                                         \4 TITL Multimedia link about this source\n\
                                         \4 FORM jpeg\n\
                                         \4 NOTE @N26@\n\
                                         \4 FILE ImgFile.JPG\n\
                                     \3 NOTE @N7@"

            parseBody newMultimediaLink2 3 nextTags ([], []) id parseMultimediaLink
                `shouldBe` newMultimediaLink2 {
                                _format = Just Jpeg,
                                _descriptiveTitle = Just "Multimedia link about this source",
                                _multimediaFileReference = Just "ImgFile.JPG",
                                _notes = [ newNote1 "@N26@" ]
                           }


    describe "parseDate" $ do
        it "builds Before Date record" $
            parseDate "BEF 31 DEC 1997"
                `shouldBe` newRangeDate "31 DEC 1997" Before

        it "builds Between Date record" $
            parseDate "BET 31 DEC 1997 AND 1 FEB 1998"
                `shouldBe` newBetweenDate "31 DEC 1997" "1 FEB 1998"

        it "builds Calculated Date record" $
            parseDate "CAL 31 DEC 1990"
                `shouldBe` newApproxDate "31 DEC 1990" Calculated

        it "builds FromTo Date record" $
            parseDate "FROM 23 DEC 1980 TO 26 DEC 1980"
                `shouldBe` newFromToDate "23 DEC 1980" "26 DEC 1980"

        it "builds From Date record" $
            parseDate "FROM 24 MAY 1981"
                `shouldBe` newPeriodDate "24 MAY 1981" From

        it "builds To Date record" $
            parseDate "TO 24 JUN 1982"
                `shouldBe` newPeriodDate "24 JUN 1982" To

        it "builds DatePhrase Date record" $
            parseDate "(near of 1972 and 1988 years)"
                `shouldBe` newDatePhrase "near of 1972 and 1988 years"

        it "builds InterpretedDatePhrase Date record" $
            parseDate "INT 1995 (from estimated age)"
                `shouldBe` newInterpretedDatePhrase "1995" "from estimated age"


    describe "parseChangeDate" $
        it "parses date without time" $ do

            let nextTags = getNextTags "1 CHAN\n\
                                        \2 DATE 18 Jul 2005\n\
                                      \1 RIN 12"

            parseBody newChangeDate 1 nextTags ([], []) id parseChangeDate
                `shouldBe` newChangeDate {
                                _changeDate = Just (UTCTime (fromGregorian 2005 07 18) (secondsToDiffTime 0))
                           }


    describe "parseAssociation" $
        it "builds Association record" $ do

            let nextTags = getNextTags    "1 ASSO @I9@\n\
                                            \2 RELA Has multimedia links\n\
                                        \1 ALIA @I9@"

            parseBody (newAssociation "@I9@") 1 nextTags ([], []) id parseAssociation
                `shouldBe` (newAssociation "@I9@") {
                                _relationIsDescriptor = Just "Has multimedia links"
                            }