{-# LANGUAGE LambdaCase #-}
module Main where

import           Data.Text.Encoding                        as Text
import qualified Data.Text.IO                              as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Yaml
import           Text.Yaml.Pretty

main :: IO ()
main = do
    input <- Text.getContents
    case decodeEither (Text.encodeUtf8 input) :: Either String Value of
        Left err   -> putStrLn err
        Right yaml -> putDocS (syntaxHighlighting (layout 80 (prettyYaml yaml)))
    pure ()

layout :: Int -> Doc a -> SimpleDocStream a
layout w = layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }

syntaxHighlighting :: SimpleDocStream Tag -> SimpleDocStream AnsiStyle
syntaxHighlighting = reAnnotateS ansiColors
  where
    ansiColors = \case
        Syntax  -> mempty
        StringV -> colorDull Green
        NumberV -> colorDull Magenta
        BoolV   -> colorDull Red
        NullV   -> colorDull Cyan
        Key     -> colorDull Blue <> bold

putDocS :: SimpleDocStream AnsiStyle -> IO ()
putDocS = Text.putStrLn . renderStrict
