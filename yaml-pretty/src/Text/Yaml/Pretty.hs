{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Yaml.Pretty where

import           Data.Bool                      (bool)
import           Data.Char
import qualified Data.HashMap.Lazy              as Map
import           Data.List                      (intersperse)
import           Data.Scientific                (Scientific)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Prettyprint.Doc      hiding (encloseSep)
import           Data.Text.Prettyprint.Doc.Util (reflow)
import qualified Data.Vector                    as Vector
import           Data.Yaml

prettyYaml :: ToJSON a => a -> Doc ()
prettyYaml = prettyValue . toJSON

prettyValue :: Value -> Doc a
prettyValue = \case
  Object o -> prettyObject o
  Array a -> prettyArray a
  String s -> prettyString s
  Number n -> prettyNumber n
  Bool b -> bool "false" "true" b
  Null -> "null"

prettyObject :: Object -> Doc a
prettyObject = render . Map.toList
  where
    render kvps
        | null kvps = "{}"
        | otherwise = encloseSep
                          (flatAlt "" "{ ")
                          (flatAlt "" " }")
                          (flatAlt "" ", ")
                          (fmap renderKeyValuePair kvps)
    renderKeyValuePair (k, v) = pretty k <> ":" <+> group (nest 2 (flatAlt line "" <> prettyValue v))

prettyArray :: Array -> Doc a
prettyArray = render . Vector.toList
  where
    render vs
        | null vs   = "[]"
        | otherwise = encloseSep
                          (flatAlt "- " "[ ")
                          (flatAlt ""   " ]")
                          (flatAlt "- " ", ")
                          (fmap (group . nest 2 . prettyValue) vs)


prettyString :: Text -> Doc a
prettyString s
    | Text.null s           = "\"\""
    | mustBeTagged s        = "! '" <> pretty (Text.replace "'" "''" s) <> "'"
    | mustBeQuoted s        = "'" <> pretty (Text.replace "'" "''" s) <> "'"
    | otherwise             = softQuote <> reflow s <> softQuote
  where
    softQuote = flatAlt "" "\""
    mustBeTagged s = Text.head s `elem` ("" :: [Char])
    mustBeQuoted s = Text.head s `elem` ("!&?%@`'\",|\\+*-~[]{}>" :: [Char])
        || Text.any (`elem` (":#" :: [Char])) s
        || isSpace (Text.head s)
        || isSpace (Text.last s)
        || isBooleanish s
        || Text.all (\c -> isNumber c || c == '.' || c == 'e') s
    isBooleanish s = s `elem`
        [ "y", "Y", "yes", "Yes", "YES"
        , "n", "N", "no", "No", "NO"
        , "true", "True", "TRUE"
        , "false", "False", "FALSE"
        , "on", "On", "ON"
        , "off", "Off", "OFF" ]
    reflow :: Text -> Doc a
    reflow t = case Text.break (== ' ') t of
        (word, t') -> case Text.span (== ' ') t' of
            (spaces, t'')
                | Text.null t             -> mempty
                | Text.length spaces == 1 -> pretty word <> softline      <> reflow t''
                | otherwise               -> pretty word <> pretty spaces <> reflow t''

prettyNumber :: Scientific -> Doc a
prettyNumber = undefined

encloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
encloseSep left right sep docs = left <> mconcat (intersperse (line' <> sep) docs) <> right
