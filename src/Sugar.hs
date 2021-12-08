module Sugar where

import CTL

desugar :: SCTL -> CTL
desugar (SAtomic var) = Atomic var
desugar Top = Not Bottom
desugar SBottom = Bottom
desugar (SNot p) = Not (desugar p)
desugar (SAnd p q) = And (desugar p) (desugar q)
desugar (SOr p q) = Or (desugar p) (desugar q)
desugar (SANext p) = AX (desugar p)
desugar (SENext p) = EX (desugar p)
desugar (SEU p q) = EU (desugar p) (desugar q)
desugar (SAU p q) = AU (desugar p) (desugar q)
desugar (SThen p q) = Or (Not (desugar p)) (desugar q)
desugar (SAF p) = AU (Not Bottom) (desugar p)
desugar (SEF p) = EU (Not Bottom) (desugar p)
desugar (SAG p) = Not (EU (Not Bottom) (Not (desugar p)))
desugar (SEG p) = Not (AU (Not Bottom) (Not (desugar p)))
 