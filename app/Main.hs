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

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled

-- # DATA STRUCTURES

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
--               | Default String

instance Show LispError where show = showError

type ThrowsError = Either LispError

-- # DISPLAY

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

-- # PARSING

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


-- # EVALUATION

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval (Vector v) = Vector <$> mapM eval v
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
           Bool True -> eval conseq
           Bool False -> eval alt
           _ -> throwError $ TypeMismatch "boolean" pred
eval (List (Atom "cond" : conds)) = evalConds conds
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalConds :: [LispVal] -> ThrowsError LispVal
evalConds [] = return $ Bool False
evalConds [List ((Atom "else") : exprs)] = evalExprList exprs
evalConds (cond : conds) = evalCond cond conds

evalCond :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCond (List (pred : conseqs)) rest = do
    result <- eval pred
    case result of
        Bool True -> evalExprList conseqs
        Bool False -> evalConds rest
        _ -> throwError $ TypeMismatch "boolean" pred
evalCond badArg _ = throwError $ BadSpecialForm "Invalid 'cond' branch" badArg

evalExprList :: [LispVal] -> ThrowsError LispVal
evalExprList [] = return $ Bool False
evalExprList [expr] = eval expr
evalExprList (expr : exprs) = eval expr >> evalExprList exprs

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- ## PRIMITIVES

type PrimitiveDict = [(String, [LispVal] -> ThrowsError LispVal)]

primitives :: PrimitiveDict
primitives = numericBinops ++ boolBinops ++ typeChecks

unpackNum :: LispVal -> ThrowsError Int
unpackNum (Number i) = return i
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character s) = return s
unpackChar notChar  = throwError $ TypeMismatch "char" notChar

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- ### NUMERIC BINARY OPERATOR PRIMITIVES

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

-- ### COMPARISON BINARY OPERATORS

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

boolBinops :: PrimitiveDict
boolBinops = numBoolBinops ++ boolBoolBinops ++ strBoolBinops

-- #### NUMERIC COMPARISON BINARY OPERATORS

numBoolBinop = boolBinop unpackNum

numBoolBinops :: PrimitiveDict
numBoolBinops = map (second numBoolBinop) [("=", (==)),
                                           ("<", (<)),
                                           (">", (>)),
                                           ("/=", (/=)),
                                           (">=", (>=)),
                                           ("<=", (<=))]

-- #### BOOLEAN COMPARISON BINARY OPERATORS

boolBoolBinop = boolBinop unpackBool

boolBoolBinops :: PrimitiveDict
boolBoolBinops = map (second boolBoolBinop) [("&&", (&&)),
                                             ("||", (||))]

-- #### STRING COMPARISON BINARY OPERATORS

strBoolBinop = boolBinop unpackStr

strBoolBinops :: PrimitiveDict
strBoolBinops = map (second strBoolBinop) [("string=?", (==)),
                                           ("string<?", (<)),
                                           ("string>?", (>)),
                                           ("string<=?", (<=)),
                                           ("string>=?", (>=))]

-- #### CHAR COMPARISON BINARY OPERATORS

charBoolBinop = boolBinop unpackChar

charBoolBinops :: PrimitiveDict
charBoolBinops = map (second charBoolBinop) [("char=?", (==)),
                                           ("char<?", (<)),
                                           ("char>?", (>)),
                                           ("char<=?", (<=)),
                                           ("char>=?", (>=))]

-- ### TYPE CHECKING PRIMITIVES
--
-- These primitives take a list of values as arguments and return `true`
-- if all of the given values are of the type

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
isPair (List (_ : _)) = True
isPair (DottedList (_ : _) _) = True
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

-- ### LIST HANDLING PRIMITIVES

listHandling :: PrimitiveDict
listHandling = [("car", car),
                ("cdr", cdr),
                ("cons", cons)]

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- ### EQUIVALENCE

equivalence :: PrimitiveDict
equivalence = [("eq?", eqv),
               ("eqv?", eqv)]

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Character arg1, Character arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                             all eqvPair (zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Right (Bool val) -> val
                               Left err -> False
                               _ -> False
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- ### SYMBOL HANDLING PRIMITIVES

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


-- # ERROR HANDLING

trapError action = catchError action (return . show)

-- `extractValue` is intended for use directly after `catchError`,
-- thus the `Left` case is never handled since misuse of this function
-- is a programmer error and we don't want to pass around garbage values
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
