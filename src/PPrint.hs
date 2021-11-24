module PPrint where

import CTL
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Data.Text.Prettyprint.Doc

errorColor = annotate (color Red)
promptColor = annotate (color Magenta)
resultColor = annotate (color Green)

-- | Pretty printer de nombres (Doc)
prompt2doc :: String -> Doc AnsiStyle
prompt2doc n = promptColor (pretty n)

result2doc :: String -> Doc AnsiStyle
result2doc n = resultColor (pretty n)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppPrompt :: String -> String
ppPrompt x  = render $ prompt2doc x

ppResult :: String -> String
ppResult x = render $ result2doc x

ppModel :: SModel -> String
ppModel = undefined