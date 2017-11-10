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
        Syntax  -> colorDull Green
        StringV -> color Green <> bold
        NumberV -> colorDull Cyan
        BoolV   -> colorDull White
        NullV   -> colorDull Red
        Key     -> colorDull Blue

putDocS :: SimpleDocStream AnsiStyle -> IO ()
putDocS = Text.putStrLn . renderStrict
