module Main where

import Data.Yaml
import Text.Yaml.Pretty
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text.IO as Text
import Data.Text.Encoding as Text

main :: IO ()
main = do
    input <- Text.getContents
    case decodeEither (Text.encodeUtf8 input) :: Either String Value of
        Left err -> putStrLn err
        Right yaml -> Text.putStrLn (renderStrict (layout 80 (prettyYaml yaml)))
    pure ()

layout w = layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }
