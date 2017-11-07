import qualified Data.ByteString.Char8                   as BS
import           Data.Char                               (isPrint)
import           Data.Text                               (Text)
import qualified Data.Text                               as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Vector                             (Vector)
import qualified Data.Vector                             as Vector
import           Data.Yaml                               (Value (..),
                                                          decodeEither)
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary

import           Text.Yaml.Pretty

main :: IO ()
main = quickCheck $ forAll (choose (10,100)) $ \width ->
    forAll arbitraryYaml $ \yaml -> decodeEither (BS.pack (renderString (layout width (prettyYaml yaml)))) === Right yaml
  where
    layout w = layoutPretty LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }

arbitraryYaml :: Gen Value
arbitraryYaml = frequency
    [ (10, fmap String arbitraryString)
    , ( 1, fmap Array  arbitraryArray) ]

arbitraryString :: Gen Text
arbitraryString = fmap Text.pack (listOf (choose ('\0', '\127') `suchThat` isPrint))

arbitraryArray :: Gen (Vector Value)
arbitraryArray = fmap Vector.fromList (listOf arbitraryYaml)
