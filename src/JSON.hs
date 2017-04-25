{-# LANGUAGE OverloadedStrings #-}
module JSON where

import Data.Text
import Data.Aeson
import Model


instance ToJSON Person where
    toJSON (Person xref resn names gender sourceCitations multimediaLinks notes) =
        object [
            "xref" .= xref,
            "resn" .= resn,
            "names" .= names,
            "gender" .= gender,
            "sourceCitations" .= sourceCitations,
            "multimediaLinks" .= multimediaLinks,
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
    toJSON (SourceCitation xref description page event notes texts dataQuality multimediaLinks dat) =
        object [
            "xref" .= xref,
            "description" .= description,
            "page" .= page,
            "event" .= event,
            "notes" .= notes,
            "texts" .= texts,
            "dataQuality" .= dataQuality,
            "multimediaLinks" .= multimediaLinks,
            "data" .= dat
        ]

instance ToJSON MultimediaLink where
    toJSON (MultimediaLink xref format customFormat descriptiveTitle fileReference notes) =
        object [
            "xref" .= xref,
            "format" .= format,
            "customFormat" .= customFormat,
            "descriptiveTitle" .= descriptiveTitle,
            "fileReference" .= fileReference,
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

instance ToJSON Data where
    toJSON (Data date texts) =
        object [
            "date" .= date,
            "texts" .= texts
        ]

instance ToJSON Date where
    toJSON (Date firstDate secondDate phrase approx range period isInterpreted) =
        object [
            "firstDate" .= firstDate,
            "secondDate" .= secondDate,
            "phrase" .= phrase,
            "approx" .= approx,
            "range" .= range,
            "period" .= period,
            "isInterpreted" .= isInterpreted
        ]

instance ToJSON RelationshipRole where
    toJSON = String . pack . show

instance ToJSON EventType where
    toJSON = String . pack . show

instance ToJSON CertaintyAssessment where
    toJSON = String . pack . show

instance ToJSON MultimediaFormat where
    toJSON = String . pack . show

instance ToJSON DateApproximated where
    toJSON = String . pack . show

instance ToJSON DateRange where
    toJSON = String . pack . show

instance ToJSON DatePeriod where
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