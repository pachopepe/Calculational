
module Calculational.Lexer where

import Text.ParserCombinators.Parsec hiding ((<|>),token,tokens)
import Text.ParserCombinators.Parsec.Char (letter,alphaNum)
import Text.Parsec.Pos (initialPos,sourceName)
import qualified Text.Parsec.Prim as N
import Data.Char (isSymbol, isPunctuation,generalCategory,GeneralCategory(..))
import Control.Applicative ((<*>),(<*),(*>),(<$>),(<|>),(<$))
import Control.Monad.Identity



data TkCategory = SYMBOL
                | LEFTP
                | RIGHTP  
                | OPEN
                | CLOSE
                | NUMBER
                | NAME
                | STRING
                | CHAR
                | END
                deriving (Show,Eq)

data Token = Token TkCategory String
           deriving (Show,Eq)
                      
-- | Inspects if a char is member of a UTF8 Category
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

-- | Parse a Character or quote 
getString :: CharParser () String
getString = do c <- lookAhead anyToken 
               case c of
                 '\"' -> return ""
                 _ -> (:) <$> oneChar <*> getString

optionalBar :: CharParser () String
optionalBar = option "" (string "|")

-- | Parser of one token 
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

-- | 
tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser (do { xs <- tokens ; x <- parsePos end; return (xs++[x]) }) ()

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

