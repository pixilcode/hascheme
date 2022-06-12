module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Except
import Data.Maybe (fromMaybe)
import Data.Array
import Data.Bifunctor
import Data.Functor
import Numeric
import Text.Parsec (optionMaybe)
import GHC.ExecutionStack (Location(functionName))

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

-- DATA STRUCTURES

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
             | Number Int
             | String String
             | Bool Bool
             | Character Char

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++  show parseErr

-- PARSING

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

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
    where noPrefix = read <$> decimal
          withPrefix = do
                         char '#'
                         prefix <- oneOf "odx"
                         case prefix of
                            'o' -> fst . head . readOct <$> many1 octDigit
                            'd' -> read <$> many1 digit
                            'x' -> fst . head . readHex <$> many1 hexDigit
                            _   -> error "Unreachable case (parseNumber)"
          decimal = many1 digit
    
--parseNumber :: Parser LispVal
--parseNumber = Number <$> (noPrefix <|> withPrefix)
--    where noPrefix = do
--                       int <- decimal
--                       float <- optionMaybe float
--                       imaginary <- optionMaybe $ char 'i'
--                       return $ case (imaginary, float) of
--                           (Just _,  _      ) ->
--                               LispComplex $ read (int ++ fromMaybe "" float)
--                           (Nothing, Just f ) ->
--                               LispReal $ read (int ++ f)
--                           (Nothing, Nothing) ->
--                               LispInt $ read int
--
--          withPrefix = do
--                         char '#'
--                         prefix <- oneOf "odx"
--                         int <- case prefix of
--                            'o' -> fst . head . readOct <$> many1 octDigit
--                            'd' -> read <$> many1 digit
--                            'x' -> fst . head . readHex <$> many1 hexDigit
--                            _   -> error "Unreachable case (parseNumber)"
--                         return $ LispInt int
--          decimal = many1 digit
--          float = do
--                    char '.'
--                    many1 digit
          
  
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


-- EVALUATION

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (Vector v) = Vector <$> mapM eval v
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- PRIMITIVES

type PrimitiveDict = [(String, [LispVal] -> ThrowsError LispVal)]

primitives :: PrimitiveDict
primitives = numericBinops ++ typeChecks

-- BINARY OPERATOR PRIMITIVES

numericBinops :: PrimitiveDict
numericBinops = map (second numericBinop) [("+", (+)),
                                           ("-", (-)),
                                           ("*", (*)),
                                           ("/", div),
                                           ("mod", mod),
                                           ("quotient", quot),
                                           ("remainder", rem)]


numericBinop :: (Int -> Int -> Int) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Int
unpackNum (Number i) = return i
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- TYPE CHECKING PRIMITIVES
--   These primitives take a list of values as arguments and return `true`
--   if all of the given values are of the type

typeChecks :: PrimitiveDict
typeChecks = fmap (second listIsType) [("boolean?", isBoolean),
                                       ("pair?", isPair),
                                       ("null?", isNull),
                                       ("list?", isList),
                                       ("symbol?", isSymbol),
                                       ("char?", isChar),
                                       ("string?", isString),
                                       ("number?", isNumber),
                                       ("vector?", isVector)]

listIsType :: (LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
listIsType isType = return . Bool . all isType

isBoolean :: LispVal -> Bool
isBoolean (Bool _) = True
isBoolean _ = False

isPair :: LispVal -> Bool
isPair (List (_:_)) = True
isPair (DottedList _ _) = True
isPair _ = False

isNull :: LispVal -> Bool
isNull (List l) = null l
isNull _ = False

isList :: LispVal -> Bool
isList (List _) = True
isList _ = False

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _ = False

isChar :: LispVal -> Bool
isChar (Character _) = True
isChar _ = False

isString :: LispVal -> Bool
isString (String _) = True
isString _ = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber _ = False

isVector :: LispVal -> Bool
isVector (Vector _) = True
isVector _ = False

-- SYMBOL HANDLING PRIMITIVES

symbolHandling :: PrimitiveDict
symbolHandling = [("symbol->string", symbolToString),
                  ("string->symbol", stringToSymbol)]

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [Atom symbol] = return $ String symbol
symbolToString [value]       = throwError $ TypeMismatch "symbol" value
symbolToString params        = throwError $ NumArgs 1 params

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [String string] = return $ Atom string
stringToSymbol [value]         = throwError $ TypeMismatch "string" value
stringToSymbol params          = throwError $ NumArgs 1 params

-- ERROR HANDLING

trapError action = catchError action (return . show)

-- `extractValue` is intended for use directly after `catchError`,
-- thus the `Left` case is never handled since misuse of this function
-- is a programmer error and we don't want to pass around garbage values
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
