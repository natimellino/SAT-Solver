{-# LANGUAGE UnicodeSyntax #-}
module PPrint where

import CTL
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Data.Text.Prettyprint.Doc


-- Colors

errorColor = annotate (color Red)
promptColor = annotate (color Magenta)
resultColor = annotate (color Green)
ctlColor = annotate (color Yellow)

-- Unicode symbols for CTL formula

unot = "\172"
uemptyset = "\216"
uexists = "\398"
uand = " \708 "
uor = " \709 "
uthen = " \x2192 "
uall = "\x2200"
usquare = "\x218A"
urombo = "\x25CA"
ubottom = "\x22A5"
utop = "\x22A4"
ucircle = "\x25EF"
uunion = " \x222A "

-- | Pretty printer de nombres (Doc)
prompt2doc :: String -> Doc AnsiStyle
prompt2doc n = promptColor (pretty n)

result2doc :: String -> Doc AnsiStyle
result2doc n = resultColor (pretty n)

error2doc :: String -> Doc AnsiStyle
error2doc n = errorColor (pretty n)

ctl2doc :: String -> Doc AnsiStyle
ctl2doc n = ctlColor (pretty n)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

ppError :: String -> String
ppError x = render $ error2doc x

ppPrompt :: String -> String
ppPrompt x  = render $ prompt2doc x

ppResult :: String -> String
ppResult x = render $ result2doc x

ppFormula :: SCTL -> String
ppFormula x = render $ ctl2doc (ppCTL x)

ppCTL :: SCTL -> String
-- First, we take care of the synonims patterns so they print correctly
ppCTL (SAF p) = parenss $ uall ++ urombo ++ (ppCTL p) -- AF pattern
ppCTL (SEF p) = parenss $ uexists ++ urombo ++ (ppCTL p) -- EF pattern
ppCTL (SAG p) = parenss $ uall ++ usquare ++ (ppCTL p) -- AG pattern
ppCTL (SEG p) = parenss $ uexists ++ usquare ++ (ppCTL p) -- EG pattern
ppCTL (STHEN p q) = parenss $ (ppCTL p) ++ uthen ++ (ppCTL q)
ppCTL STOP = utop
-- Now the rest is the usual pattern matching
ppCTL SBottom = ubottom
ppCTL (SNot p) = unot ++ (ppCTL p)
ppCTL (SAtomic v) = v
ppCTL (SAnd p q) = parenss $ (ppCTL p) ++ uand ++ (ppCTL q)
ppCTL (SOr p q) = parenss $  (ppCTL p) ++ uor ++ (ppCTL q)
-- ppCTL (Then p q) = (ppCTL p) ++ uthen ++ (ppCTL q)
ppCTL (SAX p) =  parenss $ uall ++ ucircle ++ (ppCTL p)
ppCTL (SEX p) = parenss $  uexists ++ ucircle ++ (ppCTL p)
ppCTL (SAU p q) = parenss $  uall ++ "[" ++ (ppCTL p) ++ uunion ++ (ppCTL q) ++ "]"
ppCTL (SEU p q) = parenss $  uexists ++ "[" ++ (ppCTL p) ++ uunion ++ (ppCTL q) ++ "]"

parenss :: String -> String
parenss str = "(" ++ str ++ ")"