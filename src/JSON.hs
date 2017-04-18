{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Data.Text
import Data.Aeson
import Model


instance ToJSON Person where
    toJSON (Person xref resn names gender) =
        object [
            "xref" .= xref,
            "resn" .= resn,
            "names" .= names,
            "gender" .= gender
        ]

instance ToJSON Resn where
    toJSON = String . pack . show

instance ToJSON Gender where
    toJSON = String . pack . show

instance ToJSON Name where
    toJSON (Name value npfx givn nick spfx surn nsfx sourceCitations notes) =
        object [
            "value" .= value,
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
    toJSON (SourceCitation xref page event) =
        object [
            "xref" .= xref,
            "page" .= page,
            "event" .= event
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
    toJSON (Note xref text sourceCitations) =
        object [
            "xref" .= xref,
            "text" .= text,
            "sourceCitations" .= sourceCitations
        ]

instance ToJSON Family where
    toJSON (Family xref) =
        object [
            "xref" .= xref
        ]