module CmdArgsParser
  ( parseArgs
  ) where

{-import Data.List (words)-}
import Text.Parsec
import Text.Parsec.String

{-
-- TODO: blog about this little thingy here
import Text.Parsec
import Text.Parsec.String
let escape :: Parser String; escape = do d <- char '\\'; c <- oneOf "\"'"; return [d, c]
let nonEscape :: Parser Char; nonEscape = noneOf "\\\"'"
let character :: Parser String; character = fmap return nonEscape <|> escape
let singleQuotedValue :: Parser String; singleQuotedValue = do skipMany $ char ' '; string "'"; x <- many character; string "'"; skipMany $ char ' '; return $ concat x
let doubleQuotedValue :: Parser String; doubleQuotedValue = do skipMany $ char ' '; char '"'; x <- many character; char '"'; skipMany $ char ' '; return $ concat x
let unquotedValue :: Parser String; unquotedValue = do skipMany $ char ' '; lookAhead (noneOf "\"'"); x <- manyTill anyChar $ ((try $ lookAhead $ string " ") <|> (eof >> string "")); skipMany $ char ' '; return x
let p :: Parser String; p = singleQuotedValue <|> doubleQuotedValue <|> unquotedValue
let parse' :: Parser a -> String -> Either ParseError (a, String); parse' p = do parse ((,) <$> p <*> (manyTill anyToken eof)) ""
parse' p "qwe asd '123 234' '123 \\' 234' \"123 234\" \"123 \\\" 234\" zxc "
 -}

parseArgs :: String -> Either String [String]
{-parseArgs line = Right $ words line-}
parseArgs line = loop line []
  where
    loop :: String -> [String] -> Either String [String]
    loop "" aux = Right aux
    loop line aux =
      case parseArg line of
        Right (value, restLine) -> loop restLine $ aux ++ [value]
        Left err -> Left $ show err

parseArg :: String -> Either ParseError (String, String)
parseArg = parse ((,) <$> value <*> (manyTill anyToken eof)) ""

escape :: Parser String
escape = do
  d <- char '\\'
  c <- oneOf "\"'"
  return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"'"

character :: Parser String
character = fmap return nonEscape <|> escape

singleQuotedValue :: Parser String
singleQuotedValue = do
  skipMany $ char ' '
  string "'"
  x <- many character
  string "'"
  skipMany $ char ' '
  return $ concat x

doubleQuotedValue :: Parser String
doubleQuotedValue = do
  skipMany $ char ' '
  char '"'
  x <- many character
  char '"'
  skipMany $ char ' '
  return $ concat x

unquotedValue :: Parser String
unquotedValue = do
  skipMany $ char ' '
  lookAhead (noneOf "\"'")
  x <- manyTill anyChar $ ((try $ lookAhead $ string " ") <|> (eof >> string ""))
  skipMany $ char ' '
  return x

value :: Parser String
value = singleQuotedValue <|> doubleQuotedValue <|> unquotedValue
