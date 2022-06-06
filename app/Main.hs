module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Array
import Numeric
import Text.Parsec (optionMaybe)

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number LispNumber
             | String String
             | Bool Bool
             | Character Char

instance Show LispVal where show = showVal

data LispNumber = LispInt Int
                | LispReal Float
                | LispComplex Float

instance Show LispNumber where show = showNum

-- DISPLAY

showVal :: LispVal -> String
showVal (String contents)  = "\"" ++ contents ++ "\""
showVal (Character char) = show char
showVal (Atom name) = name
showVal (Number val) = show val
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents)  = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Vector contents) = "#(" ++ (unwordsList . elems) contents ++ ")"

showNum :: LispNumber -> String
showNum (LispInt val) = show val
showNum (LispReal val) = show val
showNum (LispComplex val) = show val ++ "i"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- PARSING

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val

parseExpr :: Parser LispVal
parseExpr =
         parseGroup
         <|> parseString
         <|> parseNumber
         <|> try parseChar
         <|> parseQuoted
         <|> parseAtom

parseGroup :: Parser LispVal
parseGroup = do
               vectorSymbol <- optionMaybe $ char '#'
               char '('
               group <- case vectorSymbol of
                   Just _  -> parseVector
                   Nothing -> parseList
               char ')'
               return group

parseVector :: Parser LispVal
parseVector = do vecList <- parseExpr `sepBy` spaces
                 let listLength = length vecList
                 return $ Vector (listArray (0, listLength - 1) vecList)

parseList :: Parser LispVal
parseList = do
              head <- parseExpr `sepEndBy` spaces
              tail <- optionMaybe $ char '.' >> spaces >> parseExpr
              return $ case tail of
                  Just tail -> DottedList head tail
                  Nothing   -> List head

parseQuoted :: Parser LispVal
parseQuoted = do
                quote <- oneOf "'`,"
                expr <- parseExpr
                return $ case quote of
                    '\'' -> List [Atom "quote", expr]
                    '`'  -> List [Atom "quasiquote", expr]
                    ','  -> List [Atom "unquote", expr]
                    _    -> error "Unreachable case (parseQuoted)"

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
parseNumber = Number <$> (noPrefix <|> withPrefix)
    where noPrefix = do
                       int <- decimal
                       float <- optionMaybe float
                       imaginary <- optionMaybe $ char 'i'
                       return $ case (imaginary, float) of
                           (Just _,  _      ) ->
                               LispComplex $ read (int ++ fromMaybe "" float)
                           (Nothing, Just f ) ->
                               LispReal $ read (int ++ f)
                           (Nothing, Nothing) ->
                               LispInt $ read int

          withPrefix = do
                         char '#'
                         prefix <- oneOf "odx"
                         int <- case prefix of
                            'o' -> fst . head . readOct <$> many1 octDigit
                            'd' -> read <$> many1 digit
                            'x' -> fst . head . readHex <$> many1 hexDigit
                            _   -> error "Unreachable case (parseNumber)"
                         return $ LispInt int
          decimal = many1 digit
          float = do
                    char '.'
                    many1 digit
          
  
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
