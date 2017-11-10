{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Yaml.Pretty
    ( prettyYaml
    ) where

import           Data.Bool                      (bool)
import           Data.Char
import qualified Data.HashMap.Lazy              as Map
import           Data.List                      (intersperse)
import           Data.Scientific                (FPFormat (..), Scientific,
                                                 formatScientific)
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Data.Text.Prettyprint.Doc      hiding (encloseSep)
import           Data.Text.Prettyprint.Doc.Util (reflow)
import qualified Data.Vector                    as Vector
import           Data.Yaml

prettyYaml :: ToJSON a => a -> Doc ()
prettyYaml = prettyValue MultiLine. toJSON

data Layout = OneLine | MultiLine

prettyValue :: Layout -> Value -> Doc a
prettyValue layout = \case
  Object o -> prettyObject layout o
  Array a  -> prettyArray layout a
  String s -> prettyString layout s
  Number n -> prettyNumber layout n
  Bool b   -> bool "false" "true" b
  Null     -> "null"

prettyObject :: Layout -> Object -> Doc a
prettyObject layout = render . Map.toList
  where
    render kvps
        | null kvps         = "{}"
        | OneLine <- layout = oneLineObject kvps
        | otherwise         = flatAlt (multiLineObject kvps) (oneLineObject kvps)
    oneLineObject kvps = "{" <+> mconcat (intersperse ", " (fmap (renderKeyValuePair OneLine) kvps)) <+> "}"
    multiLineObject kvps = mconcat (intersperse hardline (fmap (group . renderKeyValuePair MultiLine) kvps))
    renderKeyValuePair l (k, v) = renderKey k <> ":" <+> nest 2 (flatAlt line "" <> prettyValue l v)
    renderKey k
        | Text.null k        = "\"\""
        | mustBeQuotedKey k  = "\"" <> pretty (escape k) <> "\""
        | otherwise          = pretty k

prettyArray :: Layout -> Array -> Doc a
prettyArray layout = render . Vector.toList
  where
    render vs
        | null vs           = "[]"
        | OneLine <- layout = oneLineArray vs
        | otherwise         = flatAlt (multiLineArray vs) (oneLineArray vs)
    oneLineArray vs = "[" <+> mconcat (intersperse ", " (fmap (prettyValue OneLine) vs)) <+> "]"
    multiLineArray vs = "-" <+> mconcat (intersperse (hardline <> "- ") (fmap (group . nest 2 . prettyValue MultiLine) vs))


prettyString :: Layout -> Text -> Doc a
prettyString layout s
    | Text.null s           = "\"\""
    | mustBeTagged s        = "! '" <> pretty (Text.replace "'" "''" s) <> "'"
    | OneLine <- layout     = "\"" <> pretty (escape s) <> "\""
    | mustBeQuoted s        = "\"" <> reflow (escape s) <> "\""
    | otherwise             = flatAlt ("\"" <> reflow (escape s) <> "\"")
                                      (pretty s)
  where
    softQuote = flatAlt "\"" ""
    reflow :: Text -> Doc a
    reflow t = case Text.break (== ' ') t of
        (word, t') -> case Text.span (== ' ') t' of
            (spaces, t'')
                | Text.null t             -> mempty
                | Text.length spaces == 1 -> pretty word <> softline      <> reflow t''
                | otherwise               -> pretty word <> pretty spaces <> reflow t''

escape = Text.replace "\"" "\\\""
        . Text.replace "\\" "\\\\"

mustBeTagged s = Text.head s `elem` ("" :: [Char])

mustBeQuoted s = Text.head s `elem` ("!&?%@`'\",|\\+*-~[]{}>" :: [Char])
    || Text.any (`elem` (":#" :: [Char])) s
    || isSpace (Text.head s)
    || isSpace (Text.last s)
    || isBooleanish s
    || isNumerical s

mustBeQuotedKey s = mustBeQuoted s
    || Text.any (`elem` ("?,{}[]" :: [Char])) s

isNumerical = Text.all (\c -> isNumber c || c == '.' || c == 'e' || c == 'E')

isBooleanish s = s `elem`
    [ "y", "Y", "yes", "Yes", "YES"
    , "n", "N", "no", "No", "NO"
    , "true", "True", "TRUE"
    , "false", "False", "FALSE"
    , "on", "On", "ON"
    , "off", "Off", "OFF" ]

prettyNumber :: Layout -> Scientific -> Doc a
prettyNumber _ = pretty . formatScientific Generic Nothing

encloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
encloseSep left right sep docs = left <> mconcat (intersperse (line' <> sep) docs) <> right
