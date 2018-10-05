{-# LANGUAGE Safe #-}

{-|
Module      : Calculational.Lexer
Description : Lexer
Copyright   : (c) Francisco J Cháves, 2012
License     : MIT
Maintainer  : pachopepe@gmail.com
Stability   : experimental

The lexer for the Dijkstra-Sholten style parser expressions. 
-}
module Calculational.Lexer where

import Text.ParserCombinators.Parsec hiding ((<|>),token,tokens)
import Text.ParserCombinators.Parsec.Char (letter,alphaNum)
import Text.Parsec.Pos (initialPos,sourceName)
import qualified Text.Parsec.Prim as N
import Data.Char (isSymbol, isPunctuation,generalCategory,GeneralCategory(..))
import Control.Applicative ((<*>),(<*),(*>),(<$>),(<|>),(<$))
import Control.Monad.Identity


-- | Token classificaion by categories
data TkCategory = SYMBOL -- ^ The token is a symbol
                | LEFTP  -- ^ Left parenthesis
                | RIGHTP -- ^ Right parenthesis 
                | OPEN   -- ^ Unicode open symbol 
                | CLOSE  -- ^ Unicode close symbol 
                | NUMBER -- ^ A number
                | NAME   -- ^ A name is an identifier, begining by upper or lower case letter
                | STRING -- ^ An string (delimited by double quotes)
                | CHAR   -- ^ A char (delimited by quotes)
                | END    -- ^ The end of file token
                deriving (Show,Eq)

-- | A 'Token' has a classification and the string of the token
data Token = Token TkCategory String
           deriving (Show,Eq)
                      
-- | Inspects if a char is member of an UTF8 Category
isCategory :: GeneralCategory -> Char -> Bool
isCategory cat c = generalCategory c == cat

-- | gets one character  
oneChar :: CharParser () Char
oneChar = do c <- anyToken
             if c == '\\'
             then do c' <- anyToken
                     return . maybe c' id $ lookup c' [('n','\n'),('t','\t'),
                                                       ('r','\r'),
                                                       ('\\','\\'),('\'','\''),('"','"')]      
             else return c

-- | An string token
getString :: CharParser () String
getString = do c <- lookAhead anyToken 
               case c of
                 '\"' -> return ""
                 _ -> (:) <$> oneChar <*> getString

-- | An optional bar for open and close delimiter token
optionalBar :: CharParser () String
optionalBar = option "" (string "|")

-- | Gets one token 
token :: CharParser () Token
token =   Token CHAR . return <$ char '\'' <*> oneChar <* char '\''
      <|> Token STRING <$ char '\"' <*> getString <* char '\"'
      <|> Token LEFTP  <$> string "("
      <|> Token RIGHTP <$> string ")"
      <|> (\c bs -> Token OPEN (c:bs)) <$> satisfy (isCategory OpenPunctuation)
                     <*> optionalBar 
      <|> Token CLOSE <$> try ((\bs c -> bs++[c]) <$> optionalBar <*> 
                               satisfy (isCategory ClosePunctuation))
      <|> Token NAME <$> ((:) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_' <|> char '\'' <|> char '.'))
      <|> Token NUMBER <$> ((++) <$> many1 digit <*> option ""
                        ((:) <$> try (char '.' <* notFollowedBy (char '.'))
                             <*> many1 digit))
      <|> Token SYMBOL <$>
                ((return <$> char ',') <|>
                many1 (satisfy (\c -> isSymbol c
                                      || isCategory OtherPunctuation c
                                      || isCategory DashPunctuation c)))

-- | Gets the end of file token 
end :: CharParser () Token
end = Token END "" <$ eof

-- | Token with position
type TokenPos = (Token, SourcePos)

-- | Mix the token with his position
parsePos :: Parser Token -> Parser TokenPos
parsePos p = flip (,) <$> getPosition <*> p

-- | Gets a list of tokens
tokens :: Parser [TokenPos]
tokens = spaces *> many (parsePos token <* spaces)
       <|> return <$> parsePos end

-- | Tokenize an string as a list of tokens 
tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser (do { xs <- tokens ; x <- parsePos end; return (xs++[x]) }) ()

-- | Tokenize an string as a list of tokens setting the positiion to 'pos' 
tokenizer :: SourcePos -> String -> [Either ParseError TokenPos]
tokenizer pos str = loop initialState
  where runParser = runPT' str (spaces *> parsePos (token <|> end))
        src = sourceName pos
        initialState = (State str pos ())
        loop st = case runIdentity $ runParser st of
                       Right (tkp@(Token END _,_),_) -> [Right tkp]
                       Right (x,st') -> Right x:loop st' 
                       Left  err     -> [Left err]
{-
runPT'
  :: Monad m =>
     t
     -> ParsecT s u m t1
     -> State s u
     -> m (Either ParseError (t1, State s u))
-}
-- | Funtion to run the lexer independently
runPT' :: Monad m => t -> N.ParsecT s u m t1 -> State s u -> m (Either ParseError (t1, State s u))
runPT'  src p st0
    = do res <- N.runParsecT p st0
         r <- parserReply res
         case r of
           N.Ok x st _  -> return (Right (x,st))
           N.Error err -> return (Left err)
    where
        parserReply res
            = case res of
                N.Consumed r -> r
                N.Empty    r -> r

