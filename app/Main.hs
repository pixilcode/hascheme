module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseChar

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (escapeChar <|> noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> (noPrefix <|> withPrefix)
    where noPrefix = many1 digit
          withPrefix = do
                         char '#'
                         prefix <- oneOf "odx"
                         case prefix of
                            'o' -> fmap (show . fst . head . readOct) (many1 octDigit)
                            'd' -> many1 digit
                            'x' -> fmap (show . fst . head . readHex) (many1 hexDigit)
                            _   -> error "Unreachable case (parseNumber)"
  
parseChar :: Parser LispVal
parseChar = do
              char '\''
              c <- escapeChar <|> noneOf "'"
              char '\''
              return $ Character c

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do
               char '\\'
               escaped <- oneOf "'\"nrt\\"
               return $ case escaped of
                   '\\' -> '\\'
                   '"'  -> '"'
                   'n'  -> '\n'
                   'r'  -> '\r'
                   't'  -> '\t'
                   '\'' -> '\''

                   -- all cases should be handled above
                   _   -> error "Unreachable case (escapeChar)"
