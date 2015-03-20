{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Quantifier.Expr
Description : Expressions parser
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

A expression parser for Dijkstra style like expressions. 
-}
module Calculational.Parser
  (parseExpr)
where

import Data.Generics
import Data.Ratio
import Language.Haskell.TH 
import Language.Haskell.TH.Syntax 
import qualified Data.Set as Set
import qualified Data.MultiSet as MultiSet
import Data.Monoid
import Data.Foldable 
import Control.Monad
import Control.Monad.Identity (Identity,runIdentity)
import Control.Applicative ((<*>),(<*),(*>),(<$>),(<|>),(<$))
import qualified Control.Applicative as A
import Text.ParserCombinators.Parsec hiding ((<|>),token,tokens)
import Text.Parsec.Prim (Reply(..),Consumed(..),ParsecT(..))
import Data.Char (isLower,isUpper,isAlpha)

import Calculational.MonoidExt
import Calculational.Definitions
import Calculational.Lexer

data QState = QState { }

-- | The parser type of state 'b', and type 'a'
type TParser b a = ParsecT [Either ParseError TokenPos] b Identity a

-- | Advance the token position 
advance :: SourcePos -> t -> [Either ParseError TokenPos] -> SourcePos
advance _ _ (Right (_, pos) : _) = pos
advance _ _ (Left err : _) = errorPos err
advance pos _ [] = pos

-- | @'sat' p@ reads a new token and check if satisfies the predicate @p@
sat :: (Token -> Bool) -> TParser a Token
sat p = do x <- lookAhead anyToken
           case x of
                Right (t,pos) -> tokenPrim showTok
                                    advance
                                    test
                Left err -> error . show $ err
      where test (Right (t,pos)) = if p t then Just t else Nothing

-- | Used as a show for tokenPrim function. 
showTok :: Either ParseError TokenPos -> String
showTok (Right (Token cat s,pos)) = show cat ++ ": "++s
showTok (Left err) = show err 

-- | @'AcceptTk' t@ accepts if the next token is equal to @t@
acceptTk :: Token -> TParser a ()
acceptTk tt = sat (== tt) >> return ()  
            <?> "Assert fail: expected '"++ show tt ++ "'"

-- | @'getCat' cat@ reads a new token if it is of category @cat@
getCat :: TkCategory -> TParser a String
getCat cat = tokenPrim showTok
                  advance
                  test
      where test (Right (Token cat' s,pos)) = if cat == cat'
                                              then Just s
                                              else Nothing
            test (Left err) = Nothing

-- | 'skip' one token
skip :: TParser a ()
skip = () <$ sat (const True) 

-- | 'accept' the token with category @tc@ and element @s@
accept :: TkCategory -> String -> TParser QState ()
accept tc s = acceptTk (Token tc s) 

-- | @parens p@ parses p enclosed by parenthesis
parens :: TParser QState a -> TParser QState a
parens = between (getCat LEFTP) (getCat RIGHTP) 

-- | @tkOpen open@ parses the @open@ bracket.
-- Some open brackets are "[", "{", etc.
tkOpen :: String -> TParser QState ()
tkOpen = accept OPEN

-- | @tkClose close@ parses the @close@ bracket.
-- Some close brackets are "]", "}", etc.
tkClose :: String -> TParser QState ()
tkClose = accept CLOSE

-- | @enclosed open close p@ parses @p@ enclosed between @open@ and @close@  
enclosed :: String -> String -> TParser QState a -> TParser QState a
enclosed open close = between (tkOpen open) (tkClose close) 

-- | @symbol s@ parses the symbol @s@
symbol :: String -> TParser QState ()
symbol = accept SYMBOL

-- | @name n@ accepts the name @n@
name :: String -> TParser QState ()
name = accept NAME

-- | @expr@ parses an expression
expr :: TParser QState ExpQ
expr = dolarExpr
     . boolExpr
     . boolImpExpr
     . boolConsExpr
     . boolConjDisExpr
     . conjuntiveExpr
     . concatExpr
     . appendExpr
     . prependExpr
     . countExpr
     . maxminExpr
     . aritExpr
     . infixExpr expr
     . unionInterExpr
     . termExpr
     . powExpr
     . compExpr
     . appExpr
     . factor $ expr

parseLS :: [([String],ExpQ)] -> TParser QState ExpQ -> TParser QState ExpQ
parseLS xs = (`chainl1` ops (map (\(ys,r) -> (ys,getOp r)) xs))

parseRS :: [([String],ExpQ)] -> TParser QState ExpQ -> TParser QState ExpQ
parseRS xs = (`chainr1` ops (map (\(ys,r) -> (ys,getOp r)) xs))

parseR :: [String] -> ExpQ -> TParser QState ExpQ -> TParser QState ExpQ
parseR xs r = parseRS [(xs,r)]

parseL :: [String] -> ExpQ -> TParser QState ExpQ -> TParser QState ExpQ
parseL xs r = parseLS [(xs,r)]

assocSamePrecExpr :: [([String],ExpQ)] -> TParser QState ExpQ -> TParser QState ExpQ
assocSamePrecExpr xs p = do 
  e0 <- p
  (do (op,es) <- choice $ map (\(ys,r)->
                            (getOp r,) <$> many1  ((choice . map choiceSymbol $ ys) *> p)) xs 
      return $ Prelude.foldl1 op (e0:es)
   <|> return e0)

-- | @retOp@ parses an operator
retOp :: Q Exp -> TParser QState ( Q Exp -> Q Exp -> Q Exp)
retOp = return . getOp 

dolarExpr :: TParser QState ExpQ -> TParser QState ExpQ
dolarExpr = parseR ["$"] [| ($) |]

-- | @appInfix@ parses infix expressions between back quotes
infixExpr :: TParser QState ExpQ -> TParser QState ExpQ -> TParser QState ExpQ
infixExpr p = (`chainl1` (between (symbol "`") (symbol "`") (factor p) >>= retOp )) 

-- | @boolExpr@ parses a booleanExpr
boolExpr :: TParser QState ExpQ -> TParser QState ExpQ 
boolExpr = parseLS [(["===","≡"],[| (==) |]),(["=/=","≢"],[| (/=) |])]

-- | @boolImpExpr@ parses an implication
boolImpExpr :: TParser QState ExpQ -> TParser QState ExpQ
boolImpExpr = parseR ["==>","⇒","⟹"] [| implies |]

-- | @boolCons@ parses a consequence
boolConsExpr :: TParser QState ExpQ ->  TParser QState ExpQ
boolConsExpr = parseL ["<==","⇐","⟸"] [| (flip implies) |]

-- | @boolConjDisExpr@ parses a conjunction or disjunction
boolConjDisExpr :: TParser QState ExpQ -> TParser QState ExpQ
boolConjDisExpr = assocSamePrecExpr [(["/\\","⋀"], [| (&&) |]),(["\\/","⋁"], [| (||) |])]
                           
-- | @conjuntiveExpr@ parses conjuntive relations
conjuntiveExpr :: TParser QState ExpQ -> TParser QState ExpQ
conjuntiveExpr p = do
  e0 <- p
  exps <- many ((,) <$> relOp <*> p)
  case exps of
       [] -> return e0
       _ -> return $ Prelude.foldl1 (getOp [| (&&) |]) $ conjuntive e0 exps
 where conjuntive e0 [] = []
       conjuntive e ((op,e'):es) = [| $op $e $e' |] : conjuntive e' es 

-- | @relOp@ parses a conjuntive relational symbol
relOp :: TParser QState ExpQ
relOp = ops relSymbols

-- | Conjuntive relational symbols
relSymbols :: [([String],ExpQ)]
relSymbols = [ (["/="],[| (/=) |])
             , (["=="],[| (==) |])
             -- , ("=",[| (==) |])
             , (["<="],[| (<=) |])
             , ([">="],[| (>=) |])
             , (["<"],[| (<) |])
             , ([">"],[| (>) |])
             , (["⊂"],[| subset |])
             , (["⊆"],[| subsetEq |])
             , (["⊃"],[| superSet |])
             , (["⊇"],[| superSetEq |])
             , (["∊"],[| member |])
             ]

-- | @termConcat@ parses concatenation of expressions
concatExpr :: TParser QState ExpQ -> TParser QState ExpQ
concatExpr =  parseL ["++"] [| (++) |]

-- | @appendExpr@ parses append expressions
appendExpr :: TParser QState ExpQ -> TParser QState ExpQ
appendExpr = parseL ["|>","⊳"] [| (\xs y -> xs ++ [y])  |]

-- | @prependExpr@ parses prepend expressions
prependExpr ::  TParser QState ExpQ -> TParser QState ExpQ
prependExpr = parseR ["<|", "⊲"] [| (:) |] 

-- | @term@ parses multiplicative expressions
countExpr :: TParser QState ExpQ -> TParser QState ExpQ
countExpr = parseL ["#"] [| MultiSet.occur |]

-- | @prependExpr@ parses prepend expressions
maxminExpr ::  TParser QState ExpQ -> TParser QState ExpQ
maxminExpr p = do
  e0 <- p
  (op,es) <-     ((getOp [| max |],) <$> many1 (symbol "↑" *> p))
             <|> ((getOp [| min |],) <$> many  (symbol "↓" *> p))
  return $ Prelude.foldl op e0 es

-- | @aritExpr@ parses additive expressions
aritExpr :: TParser QState ExpQ -> TParser QState ExpQ
aritExpr =  parseLS [(["+"], [| (+) |]),(["-"],[| (-) |])]

unionInterExpr :: TParser QState ExpQ -> TParser QState ExpQ
unionInterExpr = assocSamePrecExpr [(["∪","⋃"],[| union |]),(["∩","⋂"],[| intersection |])]

-- | @term@ parses multiplicative expressions
termExpr :: TParser QState ExpQ -> TParser QState ExpQ
termExpr = parseLS [(["*"],[| (*) |]),
                    (["/"],[| (/) |]),
                    (["÷"],[| div |]),
                    (["\\"],[| difference |])]

-- | @powExpr@ parses power expressions
powExpr ::  TParser QState ExpQ -> TParser QState ExpQ
powExpr = parseLS [(["^"],[| (^) |]),(["**"],[| (**) |])]

compExpr :: TParser QState ExpQ -> TParser QState ExpQ
compExpr = parseR ["."] [| (.) |]

-- | @appExpr@ parses applicative expressions
appExpr :: TParser QState ExpQ -> TParser QState ExpQ
appExpr = (`chainl1` (notFollowedBy
                      (choice . map symbol $ ["-","#","\\","~","¬"])
                            *> return (\e1 e2 -> [| $e1 $e2 |])))

-- | parses a @factor@ 
factor ::  TParser QState ExpQ -> TParser QState ExpQ
factor p = stringExpr <|> charExpr <|> number p <|> boolConstant 
        <|> ifExpr p
        <|> notBoolExpr p <|> iverLengthExpr p 
        <|> parentExpr p <|> ident <|> constructor
        <|> setExpr p <|> listExpr p <|> bagExpr p <|> lambdaExpr
           
-- | @boolConstant@ parses a boolConstant 
boolConstant :: TParser QState ExpQ
boolConstant =   name "True" *> return [| True |]
             <|> name "False" *> return [| False |]

-- | Parses a number
number :: TParser QState ExpQ -> TParser QState ExpQ
number p = do n1 <- (num <|> negExpr p) 
              ((\n2 -> [| $n1 % $n2 |]) <$ symbol "%" <*> (num <|> negExpr p)
               <|> return n1)

sIf :: String
sIf = "if"

sThen :: String
sThen = "then"

sElse :: String
sElse = "else"

sNot :: String
sNot = "not"

-- | @ifExpr@ Parses an if then else expression
ifExpr :: TParser QState ExpQ -> TParser QState ExpQ
ifExpr p = (\cnd e1 e2 -> [| if $cnd then $e1 else $e2 |])
           <$ name sIf   <*> p 
           <* name sThen <*> p
           <* name sElse <*> p

-- | @notBoolExpr@ parse unary not
notBoolExpr :: TParser QState ExpQ -> TParser QState ExpQ
notBoolExpr p = appE [| not |]
                <$ (name sNot <|> symbol "¬" <|> symbol "~")
                <*> factor p

-- | @notBoolExpr@ parse unary neg
negExpr :: TParser QState ExpQ -> TParser QState ExpQ
negExpr p =  appE [| negate |] <$ symbol "-" <*> factor p

-- | @iverExpr@ parse unary sharp (converts a boolean to a number)
iverLengthExpr :: TParser QState ExpQ -> TParser QState ExpQ
iverLengthExpr p = appE [| iver |] <$ symbol "#" <*> factor p 

-- | @stringExpr@ parse an String
stringExpr :: TParser QState ExpQ
stringExpr = litE . StringL <$> getCat STRING
                 
-- | @stringExpr@ parse a Char
charExpr :: TParser QState ExpQ
charExpr = litE . CharL . head  <$> getCat CHAR

{-
antiExpr = lexeme $ do{ symbol "$";e <- between "[|" "|]" ; return $ return e }
-}

-- | @parentExpr@ parses a parenthesized expression
parentExpr p = parens (quantifierExpr p <|> tupleExpr p)

-- | parses a tuple
tupleExpr :: TParser QState ExpQ -> TParser QState ExpQ 
tupleExpr p = tupE <$> p `sepBy1` symbol "," 

-- | parses a quantifier
quantifierExpr :: TParser QState ExpQ -> TParser QState ExpQ
quantifierExpr p = do {
  (op,cnv) <- qop;
  code <- bindExpr p op;
  return [| $cnv $code |] 
  }

-- | @exprList@ parses an extensional list
exprList :: TParser QState ExpQ
exprList = try (extList <$> expr  
                        <*> optionMaybe (symbol "," *> expr)
                        <* symbol ".." <*> optionMaybe expr)
         <|> listE <$> expr `sepBy1` symbol ","
       where extList e1 Nothing Nothing     = [| [$e1 .. ] |]
             extList e1 Nothing (Just e3)   = [| [$e1 .. $e3] |]
             extList e1 (Just e2) Nothing   = [| [$e1, $e2 .. ] |]
             extList e1 (Just e2) (Just e3) = [| [$e1, $e2 .. $e3] |]

         
-- | @listExpr@ parses a list comprehension
listExpr :: TParser QState ExpQ -> TParser QState ExpQ
listExpr p = collectionExpr p "[" "]" [| [] |] [| (:[]) |] (\el -> [| $el |]) 

-- | @setExpr@ parses a set
setExpr :: TParser QState ExpQ -> TParser QState ExpQ
setExpr p = collectionExpr p "{" "}" [| Set.empty |] [| Set.singleton |]
                (\el -> [| (Set.fromList $el) |])

-- | @bagExpr@ parses a bag
bagExpr :: TParser QState ExpQ -> TParser QState ExpQ
bagExpr p = collectionExpr p "{|" "|}" [| MultiSet.empty |]
            [| MultiSet.singleton |] (\el -> [| (MultiSet.fromList $el) |])

-- | @lambdaExpr@ parses a lambda
lambdaExpr :: TParser QState ExpQ
lambdaExpr = lambda <$ symbol "\\" <*> patterns <* symbol "->" <*> expr

patterns :: TParser QState [Pat]
patterns = many1 pattern

pattern :: TParser QState Pat
pattern = infixPattern
    where simpleP = patternId
                  <|> LitP <$> literal
                  <|> WildP <$ wildIdentifier 
                  <|> ConP <$> (mkName <$> constructorName) <*> patterns
                  <|> TupP <$> parens (sepBy pattern (symbol ","))
                  <|> ListP <$> enclosed "[" "]" (sepBy pattern (symbol ","))
          infixPattern = do sp <- simpleP
                            (InfixP sp <$> inf <*> simpleP
                             <|> return sp)
          inf = do smb@(x:xs) <- lookAhead (getCat SYMBOL)
                   guard (x == ':' && (length xs == 1 || xs /= ":"))
                   skip
                   return $ mkName smb

patternId :: TParser QState Pat
patternId = do
  ident <- identifier
  let name = mkName ident
  (AsP name <$ symbol "@" <*> pattern
       <|> (return $ VarP name)) 

literal :: TParser QState Lit
literal =   CharL . head <$> getCat CHAR
        <|> StringL <$> getCat STRING
        <|> IntegerL . read <$> getCat NUMBER
  

-- | @collectionExpr@ parses a collection
collectionExpr :: TParser QState ExpQ -> String -> String -> ExpQ -> ExpQ ->
                     (ExpQ -> ExpQ) -> TParser QState ExpQ 
collectionExpr p open close empty singleton fromList =
  enclosed open close (lookAhead (tkClose close) *> return empty 
     <|> try (bindExpr p singleton) 
     <|> fromList <$> exprList)

-- | @patternToBody@ 
patternToBody :: Pat -> Exp
patternToBody (LitP lit)  = LitE lit         
patternToBody (VarP name) = VarE name        
patternToBody (TupP pats) = TupE (map patternToBody pats)            
patternToBody (ConP name pats) = Prelude.foldl AppE (ConE name) (map patternToBody pats)  
patternToBody (InfixP pat1 name pat2) = InfixE (Just $ patternToBody pat1) (ConE name) (Just $ patternToBody pat2) 
patternToBody (AsP name pat) = VarE name -- (patternToBody pat) 
patternToBody (RecP name fieldPats) = RecConE name  [(name,patternToBody pat) | (name,pat) <- fieldPats]
patternToBody (ListP pats) = ListE (map patternToBody pats)
patternToBody (SigP pat ttype) = (patternToBody pat)
patternToBody (TildeP pat) = error "not supported pat TildeP ~ in Body"
patternToBody (BangP pat) = error "not supported pat BangP ! in Body"
patternToBody (ViewP exp pat) = error "not supported pat ViewP in Body"
patternToBody (WildP) = error "Wildcar _ can't be used in body"

-- | parses an expression of shape "pat <- t | r : e"
bindExpr :: TParser QState ExpQ -> Q Exp -> TParser QState ExpQ
bindExpr exprP m = do {  
                bindVars <- bindVarList exprP;
                (p,hasBody) <- ([| True |],True) <$ (symbol "|:" <|> symbol "::") 
                               <|> (,) <$ (symbol "|" <|> symbol ":")
                                       <*> rangeExpr exprP <*> option False (True <$ symbol ":"); 
                case bindVars of
                     [] -> -- There are no variables
                           do when hasBody (() <$ exprP)
                              return $ nestedBind m bindVars [| False |] [| undefined |]
                     [(pat,s)] -> do e <- parseBody hasBody (return $ return $ patternToBody pat)
                                     return $ nestedBind m bindVars p e
                     _ -> -- More than one variable, must be a body
                          nestedBind m bindVars p <$> exprP
                }
             where parseBody hasBody' e1 = if hasBody' then exprP else e1

-- | @nestedBind m vars p e@ create the code of the quantifier
-- @vars@ is the list (pattern,elements), f is the body function, p is the range
nestedBind f vars r e = auxNested vars 
                  where auxNested [] = [| if $r then $f $e else mempty |]
                        auxNested ((px,s):vars') = [| $s `bind` $(lambda [px] (auxNested vars')) |] 

-- | @qSymbols@ List of quantifier symbols and injective funtions to convert between the values and the monoid 
qSymbols :: [([String], (Q Exp, Q Exp))]
qSymbols = [ (["forall","/\\","⋀","∀"],([| All |], [| getAll |]))
           , (["\\/","exists","⋁","∃"],([| Any |], [| getAny |]))
           , (["≡","==="],([| Equiv |], [| getEquiv |]))
           , (["≢","=/="],([| NEquiv |], [| getNEquiv |]))
           , (["++","concat"],([| id |], [| id |]))
           , (["+","∑","Ʃ","Σ","sum"],([| Sum |],[| getSum |]))
           , (["*","∏","Π","product"],([| Product |], [| getProduct |]))
           , (["#"],([| Sum . iver |], [| getSum |]))
           , (["∪","⋃","union"],([| Union |], [| getUnion |]))
           , (["∩","⋂","intersection"],([| Intersection |], [| getIntersection |]))
           , (["↑","max"],([| Maximum |], [| getMaximum |]))
           , (["↓","min"],([| Minimum |], [| getMinimum |]))
           , (["avg"],([| Average . (\x -> (Sum x,Sum 1)) |], [| id |]))
           ]

-- | @ops@ creates a parser of operators 
ops :: [([String],a)] -> TParser QState a
ops [] = fail "not found"
ops ((s,v):xs) = v <$ (choice . map choiceSymbol $ s)
                <|> ops xs

choiceSymbol :: String -> TParser QState ()
choiceSymbol str@(c:_) = if isAlpha c
                         then name str
                         else symbol str

-- | @qop@ creates the parser of quantifier operators
qop :: TParser QState (ExpQ,ExpQ)
qop =  ops qSymbols
-- qop = do s <- lookahead strs
--          do types <- getInstanceNames ''Monoid

-- | @bindVar@ parses a bounded variable and a collection, the variable takes
--   the values from the collection
bindVar :: TParser QState ExpQ -> TParser QState (Pat,ExpQ)
bindVar p = (,) <$> pattern <* symbol "<-" <*> p
{-
  where pattern = VarP . mkName <$> identifier
                       <|> WildP <$ wildIdentifier
                       <|> TupP <$> parens (sepBy pattern (symbol ",")) 
-}

-- | @bindVarList@ parses the list of bounded variables and the collections 
--   where the bounded variables takes their values
bindVarList :: TParser QState ExpQ -> TParser QState [(Pat,ExpQ)]
bindVarList p = sepBy (bindVar p) (symbol ",")

-- | @rabgeExpr@ parses a range expression
rangeExpr :: TParser QState ExpQ -> TParser QState ExpQ
rangeExpr p = p <|> return [| True |]

-- | @getOp op e1 e2@ apply the arguments to a binary operator
getOp :: Q Exp -> Q Exp -> Q Exp -> Q Exp
getOp op e1 e2 = [| $op $e1 $e2 |]

-- | @concatOp@ parses a number
num  ::  TParser QState ExpQ
num  =   do{ ds <- getCat NUMBER;
             if '.' `Prelude.elem` ds
             then return $ [| (read ds)::Double |] 
             else return $ litE (IntegerL (read ds)) }

-- | @parseSatCat@ parses a specified category if satisfied @p@
parseSatCat :: TkCategory -> (String -> Bool) -> TParser QState String
parseSatCat cat p = do
  str <- lookAhead (getCat NAME)
  guard (p $ str)
  skip
  return str


-- | @identifier@ parses an identifier
identifier  :: TParser QState String
identifier  = do id <- parseSatCat NAME (isLower . head . last . splitDots)
                 if reservedWord id
                 then fail ("reserved word " ++ id)
                 else return id
                                         
-- | @wildIdentifier@ parses an identifier that begins with '_'
wildIdentifier :: TParser QState String
wildIdentifier = parseSatCat NAME ((=='_') . head)

-- | @constructor@ parses a constructor name
constructorName  :: TParser QState String
constructorName = parseSatCat NAME (isUpper . head . last . splitDots)

splitDots :: String -> [String]
splitDots [] =  [[]]
splitDots ('.':xs) = []:splitDots xs
splitDots (x:xs) = (x:ys):yss
                 where (ys:yss) = splitDots xs
                                           
-- | @reservedWord@ parses a reservedWord
reservedWord :: String -> Bool
reservedWord s = s `Prelude.elem` [sIf,sThen,sElse,sNot]

-- | @ident@ parses an identifier
ident ::  TParser QState ExpQ
ident = do id <- identifier
           let x = mkName id
           return $ varE x


-- | @constructor@ parses a constructor
constructor ::  TParser QState ExpQ
constructor = do { ct <- constructorName ;
                   let { x = mkName ct };
                   return . return $ ConE x
                 }


-- | creates a lambda abstraction
lambda :: Monad m => [Pat] -> m Exp -> m Exp
lambda px mBody = mBody >>= (\body -> return $ LamE px body)

-- | parses an expression
parseExpr :: Monad m => SourcePos -> String -> m ExpQ
parseExpr pos s =
    case runParser p (QState {}) "" stream of
      Left err  -> fail $ show err
      Right e   -> return e
  where
     stream = tokenizer pos s
     p = do e <- expr
            getCat END
            return e
