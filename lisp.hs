-- author: sjs
-- last updated: july-10-2007
--
-- A small Scheme based on the tutorial by Jonathan Tang.
-- http://halogen.note.amherst.edu/~jdtang/scheme_in_48/tutorial/overview.html
--
-- Where possible and convenient I try to support R5RS.
-- http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-2.html

module Main where
import Char
import Control.Monad.Error
import Data.IORef
import Data.List (isPrefixOf)
import IO hiding (try)
import Monad
import Numeric
import System.Environment
import System.Random
import Text.ParserCombinators.Parsec hiding (spaces)

elSchemoVersion = "0.7"

-- If no file is named on the command line run the REPL, otherwise run the
-- file then start the REPL
main :: IO ()
main = do args <- getArgs
          putStrLn ("ElSchemo v" ++ elSchemoVersion ++ " by sjs")
          if null args
              then runRepl
              else runOneThenRepl args

-- Some very basic syntax error messages
readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr maybeSpaces)

type Name = String
type LispInt = Integer
type LispFloat = Float

-- numeric lisp data types
data LispNum = Integer LispInt
             | Float LispFloat
    deriving (Eq, Ord, Show)

-- lisp data types
data LispVal = Atom Name
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number LispNum
             | Char Char
             | String String
             | Bool Bool
             | PrimitiveFunc Name ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [Name], vararg :: (Maybe String),
                       body :: [LispVal], closure :: Env}
             | IOFunc Name ([LispVal] -> IOThrowsError LispVal)
             | Port Handle
             | Null Bool


-- make the lisp data types instances of relevant classes

lispNumPlus :: LispNum -> LispNum -> LispNum
lispNumPlus (Integer x) (Integer y) = Integer $ x + y
lispNumPlus (Integer x) (Float y)   = Float $ (fromInteger x) + y
lispNumPlus (Float x)   (Float y)   = Float $ x + y
lispNumPlus (Float x)   (Integer y) = Float $ x + (fromInteger y)

lispNumMinus :: LispNum -> LispNum -> LispNum
lispNumMinus (Integer x) (Integer y) = Integer $ x - y
lispNumMinus (Integer x) (Float y)   = Float $ (fromInteger x) - y
lispNumMinus (Float x)   (Float y)   = Float $ x - y
lispNumMinus (Float x)   (Integer y) = Float $ x - (fromInteger y)

lispNumMult :: LispNum -> LispNum -> LispNum
lispNumMult (Integer x) (Integer y) = Integer $ x * y
lispNumMult (Integer x) (Float y)   = Float $ (fromInteger x) * y
lispNumMult (Float x)   (Float y)   = Float $ x * y
lispNumMult (Float x)   (Integer y) = Float $ x * (fromInteger y)

lispNumDiv :: LispNum -> LispNum -> LispNum
lispNumDiv (Integer x) (Integer y) = Integer $ x `div` y
lispNumDiv (Integer x) (Float y)   = Float $ (fromInteger x) / y
lispNumDiv (Float x)   (Float y)   = Float $ x / y
lispNumDiv (Float x)   (Integer y) = Float $ x / (fromInteger y)

lispNumAbs :: LispNum -> LispNum
lispNumAbs (Integer x) = Integer (abs x)
lispNumAbs (Float x) = Float (abs x)

lispNumSignum :: LispNum -> LispNum
lispNumSignum (Integer x) = Integer (signum x)
lispNumSignum (Float x) = Float (signum x)

instance Num LispNum where
    (+) = lispNumPlus
    (-) = lispNumMinus
    (*) = lispNumMult
    abs = lispNumAbs
    signum = lispNumSignum
    fromInteger x = Integer x


lispNumToRational :: LispNum -> Rational
lispNumToRational (Integer x) = toRational x
lispNumToRational (Float x) = toRational x

instance Real LispNum where
    toRational = lispNumToRational


lispValEq :: LispVal -> LispVal -> Bool
lispValEq (Bool arg1) (Bool arg2) = arg1 == arg2
lispValEq (Number arg1) (Number arg2) = arg1 == arg2
lispValEq (String arg1) (String arg2) = arg1 == arg2
lispValEq (Atom arg1) (Atom arg2) = arg1 == arg2
lispValEq (DottedList xs x) (DottedList ys y) = lispValEq (List $ xs ++ [x]) (List $ ys ++ [y])
lispValEq (List arg1) (List arg2) = (length arg1 == length arg2) &&
                                    (and $ map equalPair $ zip arg1 arg2)
    where equalPair (x1, x2) = lispValEq x1 x2
lispValEq _ _ = False

instance Eq LispVal where (==) = lispValEq


-- Empty values for each LispVal, these are used for letrec

emptyValue :: LispVal -> LispVal
emptyValue (Number (Integer _)) = Number (Integer 0)
emptyValue (Number (Float _)) = Number (Float 0.0)
emptyValue (Atom _) = Atom "_"
emptyValue (List _) = List []
emptyValue (DottedList l r) = DottedList [] (emptyValue r)
emptyValue (Char _) = Char ' '
emptyValue (String _) = String ""
emptyValue (Bool _) = Bool False
emptyValue (Func params vararg _ env) = Func params vararg [PrimitiveFunc "null" nullFunc] env

nullFunc :: [LispVal] -> ThrowsError LispVal
nullFunc _ = return $ Null False


-- Parser

whitespace :: Parser ()
whitespace = skipMany1 space

maybeSpaces :: Parser ()
maybeSpaces = skipMany space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#" <?> "symbol"


-- parse a binary digit, analagous to decDigit, octDigit, hexDigit
binDigit :: Parser Char
binDigit = oneOf "01" <?> "binary digit (0 or 1)"

-- analogous to isDigit, isOctdigit, isHexDigit
isBinDigit :: Char -> Bool
isBinDigit c = (c == '0' || c == '1')

-- analogous to readDec, readOct, readHex
readBin :: (Integral a) => ReadS a
readBin = readInt 2 isBinDigit digitToInt


-- Integers, floats, characters and atoms can all start with a # so wrap those with try.
-- (Maybe left factor the grammar in the future)
parseExpr :: Parser LispVal
parseExpr = (try parseFloat   <?> "float")
        <|> (try parseInteger <?> "int")
        <|> (try parseChar    <?> "char")
        <|> (parseAtom        <?> "atom")
        <|> (parseString      <?> "string")
        <|> (parseQuoted      <?> "quoted expression")
        <|> do (char '('      <?> "expression") -- should use between?
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
        <|> (parseComment     <?> "comment")


parseComment :: Parser LispVal
parseComment = do skipMany space
                  char ';'
                  skipMany $ noneOf "\n\r"
                  return $ Null False


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom


parseSign :: Parser Char
parseSign = do try (char '-')
           <|> do optional (char '+')
                  return '+'

applySign :: Char -> LispNum -> LispNum
applySign sign n = if sign == '-' then negate n else n

-- Parse binary, octal, decimal, and hexadecimal integers according to R5RS (#b, #d, #o, #x),
-- or parse a regular integer in decimal format.
parseInteger :: Parser LispVal
parseInteger = do base <- do { char '#'; oneOf "bdox" } <|> return 'd'
                  sign <- parseSign
                  int <- parseDigits base
                  return . Number $ applySign sign $ Integer . fst . head . (reader base) $ int
    where reader base = case base of
                          'b' -> readBin
                          'd' -> readDec
                          'o' -> readOct
                          'x' -> readHex


-- Parse a string of digits in the given base.
parseDigits :: Char -> Parser String
parseDigits base = many1 d >>= return
    where d = case base of
                'b' -> binDigit
                'd' -> digit
                'o' -> octDigit
                'x' -> hexDigit


-- Parse floating point numbers, but only in decimal.
parseFloat :: Parser LispVal
parseFloat = do optional (string "#d")
                sign <- parseSign
                whole <- many1 digit
                char '.'
                fract <- many1 digit
                return . Number $ applySign sign (makeFloat whole fract)
    where makeFloat whole fract = Float . fst . head . readFloat $ whole ++ "." ++ fract


-- Parse a single character according to R5RS (#\a, #\A, #\(, ...).
parseChar :: Parser LispVal
parseChar = do char '#' >> char '\\'
               c <- letter <|> digit <|> symbol <|> oneOf "()[]{} "
               return $ Char c


-- Parse an R5RS compliant string.
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many singleChar -- <?> "character"
    char '"'
    return $ String x


escapedChars :: String
escapedChars = "n\"\\"

singleChar :: Parser Char
singleChar = noneOf "\\\""
         <|> try (do c <- char '\\' >> oneOf escapedChars
                     if c == 'n' then return '\n' else return c)
         <|> char '\\'



-- Parse lists
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr whitespace


-- Parse lists of the form (head . tail)
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr whitespace
    tail <- char '.' >> whitespace >> parseExpr
    return $ DottedList head tail


-- Parse a quoted expression, ie. '(+ 3 5)
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- Convert LispVals to strings suitable for display
showVal :: LispVal -> String
showVal (Null False) = ""
showVal (String contents) = "\"" ++ escape contents ++ "\""
showVal (Char c) = "#\\" ++ [c]
showVal (Atom name) = name
showVal (List []) = "()"
showVal (Number (Integer n)) = show n
showVal (Number (Float n)) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc name _) = "#<primitive:" ++ name ++ ">"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords(map show args) ++
      (case varargs of
          Nothing -> ""
          Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "#<IO port>"
showVal (IOFunc name _) = "#<IO primitive:" ++ name ++ ">"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal


-- xs =~ s/from/to/g
subst :: (Eq a) => [a] -> [a] -> [a] -> [a]
subst _    _  [       ] = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ drop (length from) xs
        else a : subst from to as

-- escape a string for display
escape :: String -> String
escape s = subst "\n" "\\n" (subst "\"" "\\\"" (subst "\\" "\\\\" s))

-- Evaluation
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (List [Atom "load", String filename]) = do
    load filename >>= evalExprs env
    return $ Atom ("Loaded " ++ filename ++ ".")
eval env (Null _) = return $ Null False
eval env val@(Char _) = return val
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List []) = return $ List []
eval env (List [Atom "quote", val]) = return val
eval env (List (Atom "and" : params)) = lispAnd env params
eval env (List (Atom "or" : params)) = lispOr env params
eval env (List (Atom "if" : params)) = lispIf env params
eval env (List (Atom "cond" : params)) = lispCond env params
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

eval env (List (Atom "let" : List params : body)) = do
    values <- evalArgs env params
    (liftIO $ bindVars env $ zip names values) >>= evalBody
    where evalBody env = liftM last $ evalExprs env body
          names = argNames params

eval env (List (Atom "let*" : List [List [Atom var, form]] : body)) = do
    eval env form >>= defineVar env var
    liftM last $ evalExprs env body
eval env (List (Atom "let*" : List (List [Atom var, form] : rest) : body)) = do
    eval env form >>= defineVar env var
    eval env (List (Atom "let*" : List rest : body))

eval env (List (Atom "letrec" : List params : body)) = do
     -- bind the names of vars so they are visible to everything within the letrec
    env <- liftIO $ bindVars env $ zip names emptyValues
    -- now evaluate the args and set the proper values
    values <- evalArgs env params
    setVars env $ zip names values
    -- ready to evaluate the body
    liftM last $ evalExprs env body
    where names = argNames params
          emptyValues = [emptyValue x | x <- params]

-- R6RS adds letrec*, with fairly obvious semantics

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

-- taken from Arc, (f . (1 2 3)) is the same as (apply f '(1 2 3))
eval env (DottedList [function] (List args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecongized special form" badForm


evalExprs :: Env -> [LispVal] -> IOThrowsError [LispVal]
evalExprs env exprs = mapM (eval env) exprs

argNames :: [LispVal] -> [Name]
argNames params = map (\(List [Atom x, y]) -> x) params

args :: [LispVal] -> [LispVal]
args params = map (\(List [x, y]) -> y) params

evalArgs :: Env -> [LispVal] -> IOThrowsError [LispVal]
evalArgs env params = evalExprs env (args params)


-- Call primitive functions directly.
-- Call user-defined functions after ensuring the correct number of arguments are present,
-- and then evaluating those args in the proper context.
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc _ func) args = liftThrows $ func args
apply (IOFunc _ func) args = func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
        then throwError $ NumArgs (num params) args
        else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ evalExprs env body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
              Nothing -> return env
apply unFunc args = throwError $ NotFunction "Not a function" (show unFunc)


-- special forms

lispAnd :: Env -> [LispVal] -> IOThrowsError LispVal
lispAnd env [pred] = eval env pred
lispAnd env (pred:rest) = do
    result <- eval env pred
    case result of
        Bool False -> return $ Bool False
        _ -> lispAnd env rest

lispOr :: Env -> [LispVal] -> IOThrowsError LispVal
lispOr env [pred] = eval env pred
lispOr env (pred:rest) = do
    result <- eval env pred
    case result of
        Bool False -> lispOr env rest
        val -> return val

lispIf :: Env -> [LispVal] -> IOThrowsError LispVal
lispIf env (pred:conseq:alt) = do
    result <- eval env pred
    case result of
        Bool False -> eval env $ head alt
        Bool True -> eval env conseq
        badCond -> throwError $ TypeMismatch "boolean" badCond

lispCond :: Env -> [LispVal] -> IOThrowsError LispVal
lispCond env (List (Atom "else" : exprs) : []) = liftM last $ evalExprs env exprs
lispCond env (List (pred:conseq) : rest) = do
    result <- eval env pred
    case result of
        Bool False -> case rest of
            [] -> return $ Null False
            _ -> lispCond env rest
        _ -> liftM last $ evalExprs env conseq


primitives :: [(Name, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", subtractOp),
              ("*", numericBinop (*)),
              ("/", floatBinop (/)),
              ("mod", integralBinop mod),
              ("quotient", integralBinop quot),
              ("remainder", integralBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("null?", nullBoolUnop),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("string=?", strBoolBinop (==)),
              ("string<", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", symbolBoolUnop),
              ("list?", listBoolUnop),
              ("dotted-list?", dottedListBoolUnop),
              ("number?", numBoolUnop),
              ("integer?", intBoolUnop),
              ("float?", floatBoolUnop),
              ("char?", charBoolUnop),
              ("string?", strBoolUnop),
              ("bool?", boolBoolUnop),
              ("symbol->string", convertSymbolToString),
              ("string->symbol", convertStringToSymbol),
              ("string->list", convertStringToList),
              ("list->string", convertListToString),
              ("char->integer", convertCharToInt),
              ("integer->char", convertIntToChar),
              ("char-upcase", convertCharUpcase),
              ("char-downcase", convertCharDowncase),
              ("char-at", charAt),
              ("ceiling", floatingUnop ceiling),
              ("floor", floatingUnop floor),
              ("string-concatenate", stringConcatenate),
              ("string-slice", stringSlice),
              ("string-reverse", stringReverse)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor var func)

numericBinop :: (LispNum -> LispNum -> LispNum) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

integralBinop :: (LispInt -> LispInt -> LispInt) -> [LispVal] -> ThrowsError LispVal
integralBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
integralBinop op params = mapM unpackInt params >>= return . Number . Integer . foldl1 op

floatBinop :: (LispFloat -> LispFloat -> LispFloat) -> [LispVal] -> ThrowsError LispVal
floatBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
floatBinop op params = mapM unpackFloat params >>= return . Number . Float . foldl1 op

subtractOp :: [LispVal] -> ThrowsError LispVal
subtractOp num@[_] = unpackNum (head num) >>= return . Number . negate
subtractOp params = numericBinop (-) params

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op singleVal@[_] = throwError $ NumArgs 2 singleVal
boolBinop unpacker op args = do left <- unpacker $ head args
                                right <- unpacker $ args !! 1
                                return $ Bool $ left `op` right

numBoolBinop :: (LispNum -> LispNum -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop op params = boolBinop unpackNum op params

strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError LispNum
unpackNum (Number (Integer n)) = return $ Integer n
unpackNum (Number (Float n)) = return $ Float n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Number (Integer n)) = return n
unpackInt (String n) = let parsed = reads n in
                          if null parsed
                              then throwError $ TypeMismatch "integer" $ String n
                              else return . fst . head $ parsed
unpackInt (List [n]) = unpackInt n
unpackInt notInt = throwError $ TypeMismatch "integer" notInt

unpackFloat :: LispVal -> ThrowsError Float
unpackFloat (Number (Float f)) = return f
unpackFloat (Number (Integer f)) = return $ fromInteger f
unpackFloat (String f) = let parsed = reads f in
                             if null parsed
                                 then throwError $ TypeMismatch "float" $ String f
                                 else return . fst . head $ parsed
unpackFloat (List [f]) = unpackFloat f
unpackFloat notFloat = throwError $ TypeMismatch "float" notFloat

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number (Integer s)) = return $ show s
unpackStr (Number (Float s)) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


-- list processing (and strings now too)
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [String (c:xs)] = return . Char $ c
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgs = throwError $ NumArgs 1 badArgs

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [String (_:rest)] = return $ String rest
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgs = throwError $ NumArgs 1 badArgs

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List ([x] ++ xs)
cons [x, DottedList xs xlast] = return $ DottedList ([x] ++ xs) xlast
cons [Char c, String s] = return $ String (c:s)
cons [String a, String b] = return $ String (a ++ b)
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgs = throwError $ NumArgs 2 badArgs


-- equivalence testing
eqv :: [LispVal] -> ThrowsError LispVal
eqv [arg1, arg2] = return . Bool $ lispValEq arg1 arg2
eqv badArgs = throwError $ NumArgs 2 badArgs

-- For any type that is an instance of Eq, you can define an Unpacker that takes a function
-- from LispVal to that type, and may throw an error.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [List arg1, List arg2] = return . Bool $ (length arg1 == length arg2) &&
                                                   (and $ map equalPair $ zip arg1 arg2)
    where equalPair (x1, x2) =let (Bool x) = extractValue $ equal [x1, x2] in x

equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNum, AnyUnpacker unpackFloat, AnyUnpacker unpackInt, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    return $ Bool $ (primitiveEquals || lispValEq arg1 arg2)
equal badArgs = throwError $ NumArgs 2 badArgs


boolUnop :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
boolUnop op [val] = return . op $ val
boolUnop op badArgs = throwError $ NumArgs 1 badArgs

symbolBoolUnop = boolUnop isLispSymbol
listBoolUnop = boolUnop isLispList
dottedListBoolUnop = boolUnop isLispDottedList
numBoolUnop = boolUnop isLispNumber
intBoolUnop = boolUnop isLispInteger
floatBoolUnop = boolUnop isLispFloat
charBoolUnop = boolUnop isLispChar
strBoolUnop = boolUnop isLispString
boolBoolUnop = boolUnop isLispBool
nullBoolUnop = boolUnop isNull

floatingUnop :: (LispFloat -> LispInt) -> [LispVal] -> ThrowsError LispVal
floatingUnop op [Number (Float val)] = return . Number . Integer . op $ val
floatingUnop op [badArg] = throwError $ TypeMismatch "float" badArg
floatingUnop op badArgs = throwError $ NumArgs 1 badArgs

-- type identification primitives
isLispSymbol :: LispVal -> LispVal
isLispSymbol (Atom _) = Bool True
isLispSymbol _ = Bool False

isLispList :: LispVal -> LispVal
isLispList (List _) = Bool True
isLispList _ = Bool False

isLispDottedList :: LispVal -> LispVal
isLispDottedList (DottedList _ _) = Bool True
isLispDottedList _ = Bool False

isLispNumber :: LispVal -> LispVal
isLispNumber (Number  _) = Bool True
isLispNumber _ = Bool False

isLispInteger :: LispVal -> LispVal
isLispInteger (Number (Integer _)) = Bool True
isLispInteger _ = Bool False

isLispFloat :: LispVal -> LispVal
isLispFloat (Number (Float _)) = Bool True
isLispFloat _ = Bool False

isLispChar :: LispVal -> LispVal
isLispChar (Char _) = Bool True
isLispChar _ = Bool False

isLispString :: LispVal -> LispVal
isLispString (String _) = Bool True
isLispString _ = Bool False

isLispBool :: LispVal -> LispVal
isLispBool (Bool _) = Bool True
isLispBool _ = Bool False

isNull :: LispVal -> LispVal
isNull (List []) = Bool True
isNull _ = Bool False

-- conversions

conversion :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
conversion func [arg]   = func arg
conversion _    badArgs = throwError $ NumArgs 1 badArgs

convertSymbolToString = conversion symbolToString
convertStringToSymbol = conversion stringToSymbol
convertStringToList = conversion stringToList
convertListToString = conversion listToString
convertCharUpcase = conversion charUpcase
convertCharDowncase = conversion charDowncase
convertCharToInt = conversion charToInt
convertIntToChar = conversion intToChar

symbolToString :: LispVal -> ThrowsError LispVal
symbolToString (Atom a) = return $ String a
symbolToString badArg = throwError $ TypeMismatch "atom" badArg

stringToSymbol :: LispVal -> ThrowsError LispVal
stringToSymbol (String s) = return $ Atom s
stringToSymbol badArg = throwError $ TypeMismatch "string" badArg

stringToList :: LispVal -> ThrowsError LispVal
stringToList (String s) = return $ str2lst (String s)
stringToList badArg = throwError $ TypeMismatch "atom" badArg

listToString :: LispVal -> ThrowsError LispVal
listToString (List l) = return $ lst2str (List l)
listToString badArg = throwError $ TypeMismatch "list" badArg

charUpcase :: LispVal -> ThrowsError LispVal
charUpcase (Char c) = return $ Char $ toUpper c
charUpcase badArg = throwError $ TypeMismatch "char" badArg

charDowncase :: LispVal -> ThrowsError LispVal
charDowncase (Char c) = return $ Char $ toLower c
charDowncase badArg = throwError $ TypeMismatch "char" badArg

charToInt :: LispVal -> ThrowsError LispVal
charToInt (Char c) = return $ Number . Integer . toInteger $ ord c
charToInt badArg = throwError $ TypeMismatch "char" badArg

intToChar :: LispVal -> ThrowsError LispVal
intToChar (Number (Integer n)) = return $ Char . chr $ fromInteger n
intToChar badArg = throwError $ TypeMismatch "integer" badArg


-- string functions

lst2str :: LispVal -> LispVal
lst2str (List l) = String [c | Char c <- l]

str2lst :: LispVal -> LispVal
str2lst (String s) = List [Char c | c <- s]

charAt :: [LispVal] -> ThrowsError LispVal
charAt [Number (Integer idx), String s] = return . Char $ s !! (fromInteger idx)
charAt [badArg, String _] = throwError $ TypeMismatch "integer" badArg
charAt [Number (Integer _), badArg] = throwError $ TypeMismatch "string" badArg
charAt badArgs = throwError $ NumArgs 2 badArgs

stringConcatenate :: [LispVal] -> ThrowsError LispVal
stringConcatenate [List xs] = return . String $ foldl (++) "" strings
    where strings = [s | String s <- xs]
stringConcatenate [badArg] = throwError $ TypeMismatch "list" badArg
stringConcatenate badArgs = throwError $ NumArgs 1 badArgs

stringSlice :: [LispVal] -> ThrowsError LispVal
stringSlice [String s, Number (Integer start), Number (Integer len)] =
    return . String $ take (fromInteger len) $ drop (fromInteger start) s
stringSlice badArgs@[_,_,_] = throwError $ TypeMismatch "string,integer,integer" (List badArgs)
stringSlice badArgs = throwError $ NumArgs 3 badArgs

stringReverse :: [LispVal] -> ThrowsError LispVal
stringReverse [String s] = return . String $ reverse s
stringReverse [badArg] = throwError $ TypeMismatch "string" badArg
stringReverse badArgs = throwError $ NumArgs 1 badArgs


-- error checking
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | FileNotFound String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args, found values (" ++ unwordsList found ++ ")"
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (FileNotFound filename) = "File not found: " ++ show filename

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

--XXX what type does trapError have?
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- REPL (read-eval-print loop)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = do
    result <- evalString env expr
    if result == ""
        then return ()
        else putStrLn result

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOneThenRepl :: [String] -> IO ()
runOneThenRepl args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr
    runReplWithEnv $ return env

runReplWithEnv :: IO Env -> IO ()
runReplWithEnv env = do env >>= until_ (== "quit") (readPrompt "> ") . evalAndPrint

runRepl :: IO ()
runRepl = runReplWithEnv primitiveBindings
--
--runOneThenRepl :: [String] -> IO ()
--runOneThenRepl args = do runOneWithEnv primitiveBindings args >>= runReplWithEnv


-- saving state, the environment (a list of strings paired to mutable values)
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []


type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

setVars :: Env -> [(String, LispVal)] -> IOThrowsError LispVal
setVars envRef values = liftM last $ mapM set values
    where set (var, value) = setVar envRef var value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

--- XXX types? same as makeFunc, no?
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-file", closePort),
                ("close-output-file", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("display", display),
                ("random", randomInt)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


-- display values, fallback on showVal since only strings and chars need to be displayed differently
displayVal :: LispVal -> String
displayVal (String contents) = contents
displayVal (Char c) = [c]
displayVal x = showVal x


display :: [LispVal] -> IOThrowsError LispVal
display [] = return $ Null False
display (x : xs) = do liftIO . putStr $ displayVal x; display xs

randomInt :: [LispVal] -> IOThrowsError LispVal
randomInt [Number (Integer high)] = liftM (Number . Integer) $ liftIO num
    where num = getStdRandom (randomR (0,high-1))
randomInt [badArg] = throwError $ TypeMismatch "integer" badArg
randomInt args = throwError $ NumArgs 1 args
