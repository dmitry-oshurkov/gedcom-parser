{-# OPTIONS_GHC -fwarn-missing-signatures #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
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
            bodyOf newName { nameValue = "Joseph Tag /Torture/" } 1 nextTags ([], []) id parseName
                `shouldBe` newName {
                                        nameValue = "Joseph Tag /Torture/",
                                        nameNPFX = "Prof.",
                                        nameGIVN = "Joseph",
                                        nameNICK = "Joe",
                                        nameSPFX = "Le",
                                        nameSURN = "Torture",
                                        nameNSFX = "Jr."
                                    }


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
            bodyOf newSourceCitation { srccitXref = "@SOURCE1@" } 2 nextTags ([], []) id parseSourceCitation
                `shouldBe` newSourceCitation {
                                                srccitXref = "@SOURCE1@",
                                                srccitPage = 42,
                                                srccitEvent = Just newEvent {
                                                    eventType = CustomEventType,
                                                    customEventType = Just "Event type cited in source",
                                                    eventRole = Nothing,
                                                    customEventRole = Nothing
                                                }
                                             }


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