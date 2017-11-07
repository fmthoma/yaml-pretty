{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Yaml.Pretty where

import           Data.Bool                      (bool)
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
    | Text.null s      = "\"\""
    | mustBeEscaped s  = "\"" <> pretty s <> "\""
    | otherwise        = softQuote <> reflow s <> softQuote
  where
    softQuote = flatAlt "\"" ""
    mustBeEscaped s =
        Text.any (`elem` ("{}[]:" :: [Char])) s
        || Text.head s == ' '
        || Text.last s == ' '
        

prettyNumber :: Scientific -> Doc a
prettyNumber = undefined

encloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
encloseSep left right sep docs = left <> mconcat (intersperse (line' <> sep) docs) <> right
