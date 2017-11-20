{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Yaml.Pretty
    ( prettyYaml
    , Tag (..)
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

prettyYaml :: ToJSON a => a -> Doc Tag
prettyYaml = prettyValue MultiLine. toJSON

data Layout = OneLine | MultiLine
data Tag = Key | StringV | NumberV | BoolV | NullV | Syntax

prettyValue :: Layout -> Value -> Doc Tag
prettyValue layout = \case
  Object o -> prettyObject layout o
  Array a  -> prettyArray layout a
  String s -> prettyString layout s
  Number n -> prettyNumber layout n
  Bool b   -> annotate BoolV (bool "false" "true" b)
  Null     -> annotate NullV "null"

prettyObject :: Layout -> Object -> Doc Tag
prettyObject layout = render . Map.toList
  where
    render kvps
        | null kvps         = syntax "{}"
        | OneLine <- layout = oneLineObject kvps
        | otherwise         = flatAlt (multiLineObject kvps) (oneLineObject kvps)
    oneLineObject kvps = curlyBraces (commaSep (fmap (renderKeyValuePair OneLine) kvps))
    multiLineObject kvps = lineSep (fmap (group . renderKeyValuePair MultiLine) kvps)
    renderKeyValuePair l (k, v) = renderKey k <> syntax ":" <> nest 2 (line <> prettyValue l v)
    renderKey k
        | Text.null k        = quoted mempty
        | mustBeQuotedKey k  = quoted (key (escape k))
        | otherwise          = key k

prettyArray :: Layout -> Array -> Doc Tag
prettyArray layout = render . Vector.toList
  where
    render vs
        | null vs           = syntax "[]"
        | OneLine <- layout = oneLineArray vs
        | otherwise         = flatAlt (multiLineArray vs) (oneLineArray vs)
    oneLineArray vs = squareBrackets (commaSep (fmap (prettyValue OneLine) vs))
    multiLineArray vs = lineSep (fmap ((syntax "-" <+>) . group . nest 2 . prettyValue MultiLine) vs)


prettyString :: Layout -> Text -> Doc Tag
prettyString layout s
    | Text.null s           = quoted mempty
    | OneLine <- layout     = quoted (stringV (escape s))
    | mustBeQuoted s        = quoted (annotate StringV (reflow (escape s)))
    | otherwise             = flatAlt (quoted (annotate StringV (reflow (escape s))))
                                      (stringV s)
  where
    reflow :: Text -> Doc Tag
    reflow t = case Text.break (== ' ') t of
        (word, t') -> case Text.span (== ' ') t' of
            (spaces, t'')
                | Text.null t             -> mempty
                | Text.length spaces == 1 -> pretty word <> softline      <> reflow t''
                | otherwise               -> pretty word <> pretty spaces <> reflow t''

escape = Text.replace "\"" "\\\""
        . Text.replace "\\" "\\\\"

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

prettyNumber :: Layout -> Scientific -> Doc Tag
prettyNumber _ = annotate NumberV . pretty . formatScientific Generic Nothing

encloseSep :: Doc Tag -> Doc Tag -> Doc Tag -> [Doc Tag] -> Doc Tag
encloseSep left right sep docs = left <> mconcat (intersperse (line' <> sep) docs) <> right

quoted :: Doc Tag -> Doc Tag
quoted doc = syntax "\"" <> doc <> syntax "\""

curlyBraces :: Doc Tag -> Doc Tag
curlyBraces doc = syntax "{" <+> doc <+> syntax "}"

squareBrackets :: Doc Tag -> Doc Tag
squareBrackets doc = syntax "[" <+> doc <+> syntax "]"

commaSep :: [Doc Tag] -> Doc Tag
commaSep = mconcat . intersperse (syntax ", ")

lineSep :: [Doc Tag] -> Doc Tag
lineSep = mconcat . intersperse hardline

syntax :: Text -> Doc Tag
syntax = annotate Syntax . pretty

key :: Text -> Doc Tag
key = annotate Key . pretty

stringV :: Text -> Doc Tag
stringV = annotate StringV . pretty
