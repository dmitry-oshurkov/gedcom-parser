{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Aeson
import Lib
import Model
import JSON


main :: IO ()
main = hspec $ do

    describe "parseGEDCOM" $
        it "returns valid JSON" $ do
            contents <- readFile "test/TGC55CLF-utf8.ged"
            let tags = splitContent contents
            let (people, families) = parseGEDCOM (head tags) (tail tags) ([], [])
            encode people `shouldBe` "[{\"xref\":\"@I14@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Charlie Accented /ANSEL/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Male\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N24@\",\"sourceCitations\":[]}]},{\"xref\":\"@I13@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Lucy Special /ANSEL/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N25@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON6@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Teresa Mary /Caregiver/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N27@\",\"sourceCitations\":[]}]},{\"xref\":\"@I12@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Extra URL /Filelinks/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N23@\",\"sourceCitations\":[]}]},{\"xref\":\"@I11@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"General Custom /Filelinks/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Male\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N22@\",\"sourceCitations\":[]}]},{\"xref\":\"@I10@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Nonstandard Multimedia /Filelinks/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N21@\",\"sourceCitations\":[]}]},{\"xref\":\"@I9@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Standard GEDCOM /Filelinks/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Male\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N18@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON2@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Mary First /Jones/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N31@\",\"sourceCitations\":[]}]},{\"xref\":\"@I15@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Torture GEDCOM /Matriarch/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[]},{\"xref\":\"@PERSON8@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Elizabeth Second /Smith/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Female\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N32@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON3@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Chris Locked /Torture/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"UnknownGender\",\"resn\":\"Locked\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N20@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON1@\",\"names\":[{\"nick\":\"Joe\",\"npfx\":\"Prof.\",\"value\":\"Joseph Tag /Torture/\",\"givn\":\"Joseph\",\"surn\":\"Torture\",\"spfx\":\"Le\",\"sourceCitations\":[{\"event\":null,\"text\":\"\",\"page\":42,\"xref\":\"@SOURCE1@\",\"notes\":[]}],\"notes\":[{\"text\":\"These are notes about the first NAME structure in this record. These notes areembedded in the INDIVIDUAL record itself.\\n\\nThe second name structure in this record uses all possible tags for a personal namestructure.\\n\\nNOTE: many applications are confused by two NAME structures.\",\"xref\":null,\"sourceCitations\":[]}],\"nsfx\":\"Jr.\"},{\"nick\":\"\",\"npfx\":\"\",\"value\":\"William John /Smith/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[{\"event\":{\"role\":null,\"customRole\":null,\"type\":\"CustomEventType\",\"customType\":\"Event type cited in source\"},\"text\":\"\",\"page\":55,\"xref\":\"@SOURCE1@\",\"notes\":[{\"text\":\"\",\"xref\":\"@N7@\",\"sourceCitations\":[]}]}],\"notes\":[{\"text\":\"This is a second personal NAME structure in a single INDIVIDUAL recordwhich is allowed in GEDCOM. This second NAME structure has all possiblefields for a NAME structure.\\n\\nThese notes are embedded in the INDIVIDUAL record.\",\"xref\":null,\"sourceCitations\":[]}],\"nsfx\":\"\"}],\"gender\":\"Male\",\"resn\":\"Free\",\"sourceCitations\":[{\"event\":null,\"text\":\"\",\"page\":42,\"xref\":\"@SOURCE1@\",\"notes\":[{\"text\":\"A source note.\",\"xref\":null,\"sourceCitations\":[]}]},{\"event\":null,\"text\":\"\",\"page\":0,\"xref\":\"@SR2@\",\"notes\":[{\"text\":\"\",\"xref\":\"@N12@\",\"sourceCitations\":[]}]},{\"event\":null,\"text\":\"This source is embedded in the record instead of being a link to aseparate SOURCE record.\\nThe source description can use any number of lines\",\"page\":0,\"xref\":null,\"notes\":[{\"text\":\"\",\"xref\":\"@N17@\",\"sourceCitations\":[]}]}],\"notes\":[{\"text\":\"\",\"xref\":\"@N4@\",\"sourceCitations\":[{\"event\":null,\"text\":\"\",\"page\":0,\"xref\":\"@SOURCE1@\",\"notes\":[]}]},{\"text\":\"This is a second set of notes for this single individual record. It is embedded in theINDIVIDUAL record instead of being in a separate NOTE record.\\n\\nThese notes also have a source citation to a SOURCE record. In GEDCOMthis source can only be a single line and links to a SOURCE record.\",\"xref\":null,\"sourceCitations\":[{\"event\":null,\"text\":\"\",\"page\":0,\"xref\":\"@SOURCE1@\",\"notes\":[]}]}]},{\"xref\":\"@PERSON7@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Pat Smith /Torture/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"UnknownGender\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N30@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON4@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"Sandy Privacy /Torture/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"UnknownGender\",\"resn\":\"Privacy\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N29@\",\"sourceCitations\":[]}]},{\"xref\":\"@PERSON5@\",\"names\":[{\"nick\":\"\",\"npfx\":\"\",\"value\":\"William Joseph /Torture/\",\"givn\":\"\",\"surn\":\"\",\"spfx\":\"\",\"sourceCitations\":[],\"notes\":[],\"nsfx\":\"\"}],\"gender\":\"Male\",\"resn\":\"Free\",\"sourceCitations\":[],\"notes\":[{\"text\":\"\",\"xref\":\"@N28@\",\"sourceCitations\":[]}]}]"


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
                `shouldBe` Name "Joseph Tag /Torture/" "Prof." "Joseph" "Joe" "Le" "Torture" "Jr." [] []

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
                    `shouldBe` SourceCitation (Just "@SOURCE1@") "" 42 (Just (Event CustomEventType (Just "Event type cited in source") Nothing Nothing)) []

        context "on second case" $
            it "builds SourceCitation record" $ do
                let nextTags = splitContent   "2 CONC s embedded\n\
                                            \2 CONT in the record\n\
                                            \2 TEXT Text from a source. The preferred approach is to cite sources by\n\
                                                \3 CONC links to SOURCE records.\n\
                                                \3 CONT Here is a new line of text from the source.\n\
                                            \2 NOTE @N17@\n\
                                        \1 OBJE"

                bodyOf (newSourceCitation2 "This source i") 1 nextTags ([], []) id parseSourceCitation
                    `shouldBe` SourceCitation Nothing "This source is embedded\nin the record" 0 Nothing [ Note (Just "@N17@") "" [] ]


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
                evaluate (parseResn "abracadabra") `shouldThrow` errorCall "Unexpected RESN"


    describe "parseGender" $ do
        context "when value is known" $
            it "returns valid value" $ do
                parseGender "M" `shouldBe` Male
                parseGender "F" `shouldBe` Female

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseGender "abracadabra") `shouldThrow` errorCall "Unexpected SEX"


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
                `shouldBe` Name "Villy" "" "" "" "" "" "" [ SourceCitation (Just "@SOURCE1@") "" 55 Nothing [ Note (Just "@N7@") "" [] ] ] [ Note Nothing "This" [] ]