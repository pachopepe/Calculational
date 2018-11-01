{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-|
Module      : Calculational.Parser
Description : Expressions parser
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

A expression parser for Dijkstra-Sholten style like expressions. 
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
import Data.Semigroup (Max(..),Min(..))
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

-- | @'sat' p@ reads a new token and check if it satisfies the predicate @p@
sat :: (Token -> Bool) -> TParser a Token
sat p = do x <- lookAhead anyToken
           case x of
                Right (t,pos) -> tokenPrim showTok
                                    advance
                                    test
                Left err -> error . show $ err
      where test (Right (t,pos)) = if p t then Just t else Nothing

-- | Used to show a token. 
showTok :: Either ParseError TokenPos -> String
showTok (Right (Token cat s,pos)) = show cat ++ ": "++s
showTok (Left err) = show err 

-- | The @'AcceptTk' t@ expression accepts if the token is equal to @t@
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
     . extBinOpExpr 
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
     . infixExpr
     . unionInterExpr
     . diffExpr
     . addExpr
     . multExpr
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

-- | @dolarExpr@ parses an explicit function application 'f $ e'
dolarExpr :: TParser QState ExpQ -> TParser QState ExpQ
dolarExpr = parseR ["$"] [| ($) |]

-- | @infixExpr@ parses infix expressions between back quotes
infixExpr :: TParser QState ExpQ -> TParser QState ExpQ
infixExpr = (`chainl1` (between (symbol "`") (symbol "`") identifierExpr >>= retOp )) 

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
             , (["⊄"],[| notSubset |])
             , (["⊈"],[| notSubsetEq |])
             , (["⊅"],[| notSuperSet |])
             , (["⊉"],[| notSuperSetEq |])
             , (["∊"],[| member |])
             , (["∈"],[| member |])
             , (["∉"],[| notMember |])
             ]

-- | @concatExpr@ parses concatenation of expressions
concatExpr :: TParser QState ExpQ -> TParser QState ExpQ
concatExpr =  parseL ["++"] [| (++) |]

-- | @appendExpr@ parses append expressions
appendExpr :: TParser QState ExpQ -> TParser QState ExpQ
appendExpr = parseL ["|>","⊳"] [| (\xs y -> xs ++ [y])  |]

-- | @prependExpr@ parses prepend expressions
prependExpr ::  TParser QState ExpQ -> TParser QState ExpQ
prependExpr = parseR ["<|", "⊲"] [| (:) |] 

-- | @countExpr@ parses count expressions
countExpr :: TParser QState ExpQ -> TParser QState ExpQ
countExpr = parseL ["#"] [| MultiSet.occur |]

-- | @prependExpr@ parses prepend expressions
maxminExpr ::  TParser QState ExpQ -> TParser QState ExpQ
maxminExpr p = do
  e0 <- p
  (op,es) <-     ((getOp [| max |],) <$> many1 (symbol "↑" *> p))
             <|> ((getOp [| min |],) <$> many  (symbol "↓" *> p))
  return $ Prelude.foldl op e0 es

-- | @unionInterExpr@ parses union and intersection expressions
unionInterExpr :: TParser QState ExpQ -> TParser QState ExpQ
unionInterExpr = assocSamePrecExpr [(["∪","⋃"],[| union |]),(["∩","⋂"],[| intersection |])]

diffExpr :: TParser QState ExpQ -> TParser QState ExpQ
diffExpr = parseL ["\\"] [| difference |]

-- | @addExpr@ parses additive expressions
addExpr :: TParser QState ExpQ -> TParser QState ExpQ
addExpr =  parseLS [(["+"], [| (+) |]),(["-"],[| (-) |])]

-- | @mult@ parses multiplicative expressions
multExpr :: TParser QState ExpQ -> TParser QState ExpQ
multExpr = parseLS [(["*"],[| (*) |]),
                    (["/"],[| (/) |]),
                    (["÷"],[| div |]),
                    (["%"],[| (%) |])]

-- | @powExpr@ parses power expressions
powExpr ::  TParser QState ExpQ -> TParser QState ExpQ
powExpr = parseLS [(["^"],[| (^) |]),(["**"],[| (**) |])]

-- | @compExpr@ parses function composition expressions
compExpr :: TParser QState ExpQ -> TParser QState ExpQ
compExpr = parseR ["."] [| (.) |]

-- | @appExpr@ parses applicative expressions
appExpr :: TParser QState ExpQ -> TParser QState ExpQ
appExpr = (`chainl1` (notFollowedBy
                      -- (choice . map symbol $ ["-","#","\\","~","¬"])
                      (getCat SYMBOL)
                            *> return (\e1 e2 -> [| $e1 $e2 |])))

-- | @extBinOpExpr@ parses operators defined in the haskell environment
extBinOpExpr :: TParser QState ExpQ -> TParser QState ExpQ
extBinOpExpr = (`chainl1` symbolOp)
             where symbolOp = do 
                           sop <- parseSatCat SYMBOL notDelim 
                           let op' = theOp sop
                           retOp op'
                   notDelim x = Prelude.and $ map (/= x) [":","|","|:","::","$",",",".."]
                   theOp s =  do mn <- lookupValueName s
                                 case mn of
                                      Nothing -> fail ("undefined operator '"++s++"'")
                                      Just name -> varE name
                                         

-- | parses a @factor@ 
factor ::  TParser QState ExpQ -> TParser QState ExpQ
factor p = stringExpr <|> charExpr <|> numberExpr p 
        <|> emptyExpr
        <|> ifExpr p
        <|> notExpr p <|> sharpExpr p  <|> negExpr p 
        <|> identifierExpr <|> constructorExpr <|> lambdaExpr
        <|> parentExpr p <|> setExpr p <|> listExpr p <|> bagExpr p
           
-- | @emptyConstant@ parses the emptyConstant 
emptyExpr :: TParser QState ExpQ
emptyExpr =   symbol "∅" *> return [| emptyCollection |]

-- | @numberExpr@ Parses a number
numberExpr :: TParser QState ExpQ -> TParser QState ExpQ
numberExpr p = num

-- | @sIf@ is the 'if' 'String'
sIf :: String
sIf = "if"

-- | @sThen@ is the 'then' 'String'
sThen :: String
sThen = "then"

-- | @sElse@ is the 'else' 'String'
sElse :: String
sElse = "else"

-- | @ifExpr@ Parses an if then else expression
ifExpr :: TParser QState ExpQ -> TParser QState ExpQ
ifExpr p = (\cnd e1 e2 -> [| if $cnd then $e1 else $e2 |])
           <$ name sIf   <*> p 
           <* name sThen <*> p
           <* name sElse <*> p

-- | @notExpr@ parse unary not
notExpr :: TParser QState ExpQ -> TParser QState ExpQ
notExpr p = appE [| not |]
            <$ (symbol "¬" <|> symbol "~")
            <*> factor p

-- | @negExpr@ parse unary neg
negExpr :: TParser QState ExpQ -> TParser QState ExpQ
negExpr p =  appE [| negate |] <$ symbol "-" <*> factor p

-- | @sharpExpr@ parse unary sharp (converts a boolean to a number)
sharpExpr :: TParser QState ExpQ -> TParser QState ExpQ
sharpExpr p = appE [| sharp |] <$ symbol "#" <*> factor p 

-- | @stringExpr@ parse an String
stringExpr :: TParser QState ExpQ
stringExpr = litE . StringL <$> getCat STRING
                 
-- | @stringExpr@ parse a Char
charExpr :: TParser QState ExpQ
charExpr = litE . CharL . head  <$> getCat CHAR

-- | @parentExpr@ parses a parenthesized expression
parentExpr p = parens (quantifierExpr p <|> tupleExpr p)

-- | parses a tuple
tupleExpr :: TParser QState ExpQ -> TParser QState ExpQ 
tupleExpr p = tupE <$> p `sepBy` symbol "," 

-- | parses a quantifier
quantifierExpr :: TParser QState ExpQ -> TParser QState ExpQ
quantifierExpr p = do {
  (op,cnv) <- quantifierOp;
  code <- comprehensionExpr p op;
  return [| $cnv $code |] 
  }

-- | @extensionalExpr@ parses an extensional range and returns a list of elements
extensionalExpr :: TParser QState ExpQ
extensionalExpr = try (extList <$> expr  
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
                  <|> LitP <$> patternLiteral
                  <|> WildP <$ wildIdentifier 
                  <|> ConP <$> (mkName <$> constructorName) <*> patterns
                  <|> TupP <$> parens (sepBy pattern (symbol ","))
                  <|> ListP <$> enclosed "[" "]" (sepBy pattern (symbol ","))
          infixPattern = do sp <- simpleP
                            (InfixP sp <$> patternOp <*> pattern
                             <|> return sp)
          patternOp = do smb@(x:xs) <- lookAhead (getCat SYMBOL)
                         guard (x == ':' && (length xs == 1 || xs /= ":"))
                         skip
                         return $ mkName smb

-- | @patternId@ parses an identifier pattern
patternId :: TParser QState Pat
patternId = do
  ident <- identifier
  let name = mkName ident
  (AsP name <$ symbol "@" <*> pattern
       <|> (return $ VarP name)) 

-- | @patternId@ parses a pattern literal
patternLiteral :: TParser QState Lit
patternLiteral =   CharL . head <$> getCat CHAR
        <|> StringL <$> getCat STRING
        <|> IntegerL . read <$> getCat NUMBER

-- | @collectionExpr@ parses a collection
collectionExpr :: TParser QState ExpQ -> String -> String -> ExpQ -> ExpQ ->
                     (ExpQ -> ExpQ) -> TParser QState ExpQ 
collectionExpr p open close empty singleton fromList =
  enclosed open close (lookAhead (tkClose close) *> return empty 
     <|> try (comprehensionExpr p singleton) 
     <|> fromList <$> extensionalExpr)

-- | parses an expression of the form "pat <- t | r : e"
comprehensionExpr :: TParser QState ExpQ -> Q Exp -> TParser QState ExpQ
comprehensionExpr exprP m = do {  
                bindVars <- bindExprList exprP;
                (p,hasBody) <- -- There is not explicit range, it is True
                               ([| True |],True) <$ (symbol "|:" <|> symbol "::") 
                               -- There is a range
                               <|> (,) <$ (symbol "|" <|> symbol ":")
                                       <*> rangeExpr exprP 
                                       -- parse the body
                                       <*> option False (True <$ symbol ":"); 
                case bindVars of
                     [] -> -- There are no variables
                           do when hasBody (() <$ exprP)
                              return $ nestedBind m [] [| False |] [| undefined |]
                     [(pat,s)] -> if hasBody
                                  then -- There is a body in the quantifier
                                       nestedBind m bindVars p <$> exprP
                                  else -- There is no body in the quantifier
                                       return $ noBodyBind m pat s p  
                     _ -> -- More than one variable, must have a body
                          nestedBind m bindVars p <$> exprP
                }
             where endBind f r e = [| if $r then $f $e else mempty |]
                   noBodyBind f px@(AsP name _) s r =
                        [| $s `bind` $(lambda [ px ] (endBind f r (return $ VarE name))) |] 
                   noBodyBind f pat s r = do 
                         name <- newName "_v"
                         [| $s `bind` $(lambda [ (AsP name pat) ] (endBind f r (return $ VarE name))) |] 
                   -- | @nestedBind m vars p e@ create the code of the quantifier
                   -- @vars@ is the list (pattern,elements), f is the body function, p is the range
                   nestedBind f vars r e = auxNested vars 
                        where auxNested [] = endBind f r e
                              auxNested ((px,s):vars') = 
                                    [| $s `bind` $(lambda [ px ] (auxNested vars')) |] 

-- | @qSymbols@ List of quantifier symbols and injective funtions to convert between the values and the monoid 
qSymbols :: [([String], (Q Exp, Q Exp))]
qSymbols = [ (["forall","/\\","⋀","∀"],([| All |], [| getAll |]))
           , (["\\/","exists","⋁","∃"],([| Any |], [| getAny |]))
           , (["≡","==="],([| Equiv |], [| getEquiv |]))
           , (["≢","=/="],([| NEquiv |], [| getNEquiv |]))
           , (["++","concat"],([| id |], [| id |]))
           , (["+","∑","Ʃ","Σ","sum"],([| Sum |],[| getSum |]))
           , (["*","∏","Π","product"],([| Product |], [| getProduct |]))
           , (["#"],([| Sum . sharp |], [| getSum |]))
           , (["∪","⋃","union"],([| Union |], [| getUnion |]))
           , (["∩","⋂","intersection"],([| Intersection |], [| getIntersection |]))
           , (["↑","max"],([| Max |], [| getMax |]))
           , (["↓","min"],([| Min |], [| getMin |]))
           , (["avg"],([| Average . (\x -> (Sum x,Sum 1)) |], [| id |]))
           ]

-- | @ops@ creates a parser of operators 
ops :: [([String],a)] -> TParser QState a
ops [] = fail "not found"
ops ((s,v):xs) = v <$ (choice . map choiceSymbol $ s)
                <|> ops xs

-- | @choiceSymbol@ parse a function name or symbol depending the first character of the string 
choiceSymbol :: String -> TParser QState ()
choiceSymbol str@(c:_) = if isAlpha c
                         then name str
                         else symbol str

-- | @qop@ creates the parser of quantifier operators
quantifierOp :: TParser QState (ExpQ,ExpQ)
quantifierOp =  ops qSymbols
-- qop = do s <- lookahead strs
--          do types <- getInstanceNames ''Monoid

-- | @bindExpr@ parses a bounded variable and a collection, the variable takes
--   the values from the collection
bindExpr :: TParser QState ExpQ -> TParser QState (Pat,ExpQ)
bindExpr p = (,) <$> pattern <* symbol "<-" <*> p

-- | @bindExprList@ parses the list of bounded variables and collections 
-- x_0 <- s_0, x1 <- s_1, ..., x_n <- s_n
--   where bounded variable x_i takes their values from collection s_i, 0 <= i <= n
bindExprList :: TParser QState ExpQ -> TParser QState [(Pat,ExpQ)]
bindExprList p = sepBy (bindExpr p) (symbol ",")

-- | @rangeExpr@ parses a range expression
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
  str <- lookAhead (getCat cat)
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


-- | @splitSots@ split an string with dots in a list of strings 
splitDots :: String -> [String]
splitDots [] =  [[]]
splitDots ('.':xs) = []:splitDots xs
splitDots (x:xs) = (x:ys):yss
                 where (ys:yss) = splitDots xs
                                           
-- | @reservedWord@ parses a reservedWord
reservedWord :: String -> Bool
reservedWord s = s `Prelude.elem` [sIf,sThen,sElse]

-- | @ident@ parses an identifier
identifierExpr ::  TParser QState ExpQ
identifierExpr = do id <- identifier
                    let x = mkName id
                    return $ varE x


-- | @constructorExpr@ parses a constructor
constructorExpr ::  TParser QState ExpQ
constructorExpr = do { ct <- constructorName ;
                   let { x = mkName ct };
                   return . return $ ConE x
                 }


-- | @lambda@ creates a lambda abstraction
lambda :: Monad m => [Pat] -> m Exp -> m Exp
lambda px mBody = mBody >>= (\body -> return $ LamE px body)

-- | @parseExpr@ parses an expression
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
