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
    toJSON (SourceCitation xref description page event notes) =
        object [
            "xref" .= xref,
            "description" .= description,
            "page" .= page,
            "event" .= event,
            "notes" .= notes
        ]

instance ToJSON Event where
    toJSON (Event typ customType role customRole) =
        object [
            "type" .= typ,
            "customType" .= customType,
            "role" .= role,
            "customRole" .= customRole
        ]

instance ToJSON RelationshipRole where
    toJSON = String . pack . show

instance ToJSON EventType where
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