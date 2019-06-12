module Desugar 
(
    desugar
)
where

import AST as A
import SAST as S

desugar :: SAST -> AST
desugar (S.Var s) = A.Var s
desugar (S.Abs s a) = A.Abs s (desugar a)
desugar (S.App a b) = A.App (desugar a) (desugar b)
desugar S.Unit = A.Unit
