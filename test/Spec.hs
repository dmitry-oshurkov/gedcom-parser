{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Crypto.Hash
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
            sha1Hex (encode people) `shouldBe` "0cd681d50951ab6a52f33c42b347c4935e1248ed"
            sha1Hex (encode families) `shouldBe` "1446e611d189d06fce528d57abe8d8f385aa977f"


    describe "parseTag" $ do
        it "returns level, tag, value of a NAME tag" $
            parseTag "1 NAME Lucy Special /ANSEL/" `shouldBe` (1, "NAME", "Lucy Special /ANSEL/")

        it "returns level, tag and empty value of a GEDC tag" $
            parseTag "1 GEDC" `shouldBe` (1, "GEDC", "")

        it "returns level, xref, tag of a INDI tag" $
            parseTag "0 @I14@ INDI" `shouldBe` (0, "@I14@", "INDI")


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
            bodyOf (newName "Joseph Tag /Torture/") 1 nextTags ([], []) id parseName
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
                bodyOf (newSourceCitation1 "@SOURCE1@") 2 nextTags ([], []) id parseSourceCitation
                    `shouldBe` SourceCitation (Just "@SOURCE1@") "" (Just 42) (Just (Event CustomEventType (Just "Event type cited in source") Nothing Nothing)) [] Nothing Nothing Nothing Nothing

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

                bodyOf (newSourceCitation2 "This source i") 1 nextTags ([], []) id parseSourceCitation
                    `shouldBe` (newSourceCitation2 "This source is embedded\nin the record") {

                                   _srcNotes = [ newNote1 "@N17@" ],
                                   _text = Just "Text from a source. The preferred approach is to cite sources bylinks to SOURCE records.\nHere is a new line of text from the source.",
                                   _dataQuality = Just Unreliable,
                                   _dat = Just newData {
                                        _dataDate = Just "1 JAN 1900",
                                        _dataText = Just "Here is some text from the source specific to this sourcecitation.\nHere is more text but on a new line."
                                   }
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
                bodyOf (newNote1 "@N26@") 4 nextTags ([], []) id parseNote
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
                bodyOf (newNote2 "These are notes about the first NAME structure in this record. These notes are ") 2 nextTags ([], []) id parseNote
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

            bodyOf (newName "Villy") 1 nextTags ([], []) id parseName
                `shouldBe` (newName "Villy") {

                                _nameSourceCitations = [

                                    (newSourceCitation1 "@SOURCE1@") {

                                        _page = Just 55,
                                        _srcNotes = [ newNote1 "@N7@" ],
                                        _multimedia = Just newMultimediaLink2 {
                                            _note = Just $ newNote1 "@N26@"
                                        }
                                    }
                                ],
                                _nameNotes = [ newNote2 "This" ]
                            }


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


    describe "parseMultimediaLink" $
        it "builds MultimediaLink record" $ do

            let nextTags = getNextTags "3 OBJE\n\
                                         \4 TITL Multimedia link about this source\n\
                                         \4 FORM jpeg\n\
                                         \4 NOTE @N26@\n\
                                         \4 FILE ImgFile.JPG\n\
                                     \3 NOTE @N7@"

            bodyOf newMultimediaLink2 3 nextTags ([], []) id parseMultimediaLink
                `shouldBe` newMultimediaLink2 {
                                _format = Just Jpeg,
                                _descriptiveTitle = Just "Multimedia link about this source",
                                _multimediaFileReference = Just "ImgFile.JPG",
                                _note = Just $ newNote1 "@N26@"
                           }