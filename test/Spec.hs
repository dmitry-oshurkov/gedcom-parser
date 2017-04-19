{-# OPTIONS_GHC -fwarn-missing-signatures #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List.Split
import Lib
import Model


main :: IO ()
main = hspec $ do

    describe "parseTag" $ do
        it "returns the level, tag, value of a NAME tag" $
            parseTag "1 NAME Lucy Special /ANSEL/" `shouldBe` (1, "NAME", "Lucy Special /ANSEL/")

        it "returns the level, tag and empty value of a GEDC tag" $
            parseTag "1 GEDC" `shouldBe` (1, "GEDC", "")

        it "returns the level, xref, tag of a INDI tag" $
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

    describe "parseSourceCitation" $
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
            bodyOf (newSourceCitation "@SOURCE1@") 2 nextTags ([], []) id parseSourceCitation
                `shouldBe` SourceCitation "@SOURCE1@" 42 (Just (Event CustomEventType (Just "Event type cited in source") Nothing Nothing)) []


    describe "parseRelationshipRole" $
        it "returns the valid value" $ do
            parseRelationshipRole "CHIL" `shouldBe` Child
            parseRelationshipRole "HUSB" `shouldBe` Husband
            parseRelationshipRole "WIFE" `shouldBe` Wife
            parseRelationshipRole "MOTH" `shouldBe` Mother
            parseRelationshipRole "FATH" `shouldBe` Father
            parseRelationshipRole "SPOU" `shouldBe` Spouse
            parseRelationshipRole "Role in cited event" `shouldBe` CustomRelationshipRole


    describe "parseEventType" $
        it "returns the valid value" $ do
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
            it "returns the valid value" $ do
                parseResn "locked" `shouldBe` Locked
                parseResn "privacy" `shouldBe` Privacy

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseResn "abracadabra") `shouldThrow` errorCall "Unexpected RESN"


    describe "parseGender" $ do
        context "when value is known" $
            it "returns the valid value" $ do
                parseGender "M" `shouldBe` Male
                parseGender "F" `shouldBe` Female

        context "when value is unknown" $
            it "throws an exception" $
                evaluate (parseGender "abracadabra") `shouldThrow` errorCall "Unexpected SEX"


    describe "parseNote1" $
        it "builds first case Note record" $ do
            {-
                4 NOTE @N26@
                4 FILE ImgFile.JPG
            -}
            let nextTags = [
                            (4, "FILE", "ImgFile.JPG")
                        ]
            bodyOf (newNote1 "@N26@") 4 nextTags ([], []) id parseNote1
                `shouldBe` Note (Just "@N26@") "" []


    describe "parseNote2" $
        it "builds second case Note record" $ do
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
            bodyOf (newNote2 "These are notes about the first NAME structure in this record. These notes are ") 2 nextTags ([], []) id parseNote2
                `shouldBe` Note Nothing "These are notes about the first NAME structure in this record. These notes are embedded in the INDIVIDUAL record itself.\n\nThe second name structure in this record uses all possible tags for a personal name structure.\n\nNOTE: many applications are confused by two NAME structures." []


    describe "bodyOf" $
        it "parse next level tags only" $ do

            let nextTags = map parseTag (splitOn "\n" "2 SOUR @SOURCE1@\n3 PAGE 55\n3 OBJE\n4 NOTE @N26@\n3 NOTE @N7@\n2 NOTE This\n1 NAME Barry")

            bodyOf (newName "Villy") 1 nextTags ([], []) id parseName
                `shouldBe` Name "Villy" "" "" "" "" "" "" [ SourceCitation "@SOURCE1@" 55 Nothing [ Note (Just "@N7@") "" [] ] ] [ Note Nothing "This" [] ]