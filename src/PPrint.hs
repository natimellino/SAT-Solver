module PPrint where

import CTL
import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Data.Text.Prettyprint.Doc

 {-# LANGUAGE UnicodeSyntax #-}

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

ppFormula :: CTL -> String
ppFormula x = render $ ctl2doc (ppCTL x)

ppCTL :: CTL -> String
ppCTL (Atomic v) = v
ppCTL Bottom = ubottom
ppCTL Top = utop
ppCTL (Not p) = unot ++ (ppCTL p)
ppCTL (And p q) = (ppCTL p) ++ uand ++ (ppCTL q)
ppCTL (Or p q) = (ppCTL p) ++ uor ++ (ppCTL q)
ppCTL (Then p q) = (ppCTL p) ++ uthen ++ (ppCTL q)
ppCTL (AX p) =  uall ++ ucircle ++ (ppCTL p)
ppCTL (EX p) =  uexists ++ ucircle ++ (ppCTL p)
ppCTL (AU p q) =  uall ++ "[" ++ (ppCTL p) ++ uunion ++ (ppCTL q) ++ "]"
ppCTL (EU p q) =  uexists ++ "[" ++ (ppCTL p) ++ uunion ++ (ppCTL q) ++ "]"
ppCTL (AF p) = uall ++ urombo ++ (ppCTL p)
ppCTL (EF p) = uexists ++ urombo ++ (ppCTL p)
ppCTL (AG p) = uall ++ usquare ++ (ppCTL p)
ppCTL (EG p) = uexists ++ usquare ++ (ppCTL p)


ppModel :: SModel -> String
ppModel = undefined