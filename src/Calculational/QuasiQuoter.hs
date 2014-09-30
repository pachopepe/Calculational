{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}

-----------------------------------------------------------------------------
-- | Módulo Quantifier.Operator: funciones para utilizar la sintaxis de la
--   lógica calculatoria. 
--
-- Autor: Francsco J. Cháves
--
-- 
-----------------------------------------------------------------------------


module Calculational.QuasiQuoter (
  calc
)
where
 
import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Text.Parsec.Pos (newPos)

import Calculational.Parser (parseExpr)
import Calculational.MonoidExt

-- quoteExprPat :: String -> TH.PatQ
 
calc  :: QuasiQuoter
calc =  QuasiQuoter quoteExprExp undefined undefined undefined

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  newPos (TH.loc_filename loc)
                                        (fst $ TH.loc_start loc)
                                        (snd $ TH.loc_start loc)
                      qExpr <- parseExpr pos s
                      expr <- qExpr
                      return expr
                      
