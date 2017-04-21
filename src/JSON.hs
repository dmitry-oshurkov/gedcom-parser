{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Data.Text
import Data.Aeson
import Model


instance ToJSON Person where
    toJSON (Person xref resn names gender sourceCitations notes) =
        object [
            "xref" .= xref,
            "resn" .= resn,
            "names" .= names,
            "gender" .= gender,
            "sourceCitations" .= sourceCitations,
            "notes" .= notes
        ]

instance ToJSON Resn where
    toJSON = String . pack . show

instance ToJSON Gender where
    toJSON = String . pack . show

instance ToJSON Name where
    toJSON (Name namePersonal npfx givn nick spfx surn nsfx sourceCitations notes) =
        object [
            "namePersonal" .= namePersonal,
            "npfx" .= npfx,
            "givn" .= givn,
            "nick" .= nick,
            "spfx" .= spfx,
            "surn" .= surn,
            "nsfx" .= nsfx,
            "sourceCitations" .= sourceCitations,
            "notes" .= notes
        ]

instance ToJSON SourceCitation where
    toJSON (SourceCitation xref description page event notes text dataQuality multimedia dat) =
        object [
            "xref" .= xref,
            "description" .= description,
            "page" .= page,
            "event" .= event,
            "notes" .= notes,
            "text" .= text,
            "dataQuality" .= dataQuality,
            "multimedia" .= multimedia,
            "data" .= dat
        ]

instance ToJSON MultimediaLink where
    toJSON (MultimediaLink xref format customFormat descriptiveTitle multimediaFileReference note) =
        object [
            "xref" .= xref,
            "format" .= format,
            "customFormat" .= customFormat,
            "descriptiveTitle" .= descriptiveTitle,
            "multimediaFileReference" .= multimediaFileReference,
            "note" .= note
        ]

instance ToJSON Event where
    toJSON (Event typ customType role customRole) =
        object [
            "type" .= typ,
            "customType" .= customType,
            "role" .= role,
            "customRole" .= customRole
        ]

instance ToJSON Data where
    toJSON (Data date text) =
        object [
            "date" .= date,
            "text" .= text
        ]

instance ToJSON RelationshipRole where
    toJSON = String . pack . show

instance ToJSON EventType where
    toJSON = String . pack . show

instance ToJSON CertaintyAssessment where
    toJSON = String . pack . show

instance ToJSON MultimediaFormat where
    toJSON = String . pack . show

instance ToJSON Note where
    toJSON (Note xref submitterText sourceCitations) =
        object [
            "xref" .= xref,
            "submitterText" .= submitterText,
            "sourceCitations" .= sourceCitations
        ]

instance ToJSON Family where
    toJSON (Family xref) =
        object [
            "xref" .= xref
        ]