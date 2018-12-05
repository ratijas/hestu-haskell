{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Scheme.VM.Core where

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.IORef
import Data.Maybe
import System.Environment
import System.IO
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef, javaStyle, LanguageDef)
import Text.ParserCombinators.Parsec hiding (spaces, string)

-- *** Parser

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr


readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)


readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val



parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space



parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
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
parseNumber = liftM (Number . read) $ many1 digit


parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces


parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail


parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- *** D Parser


newtype DProgram = DProgram DBody
instance Show DProgram where
  show (DProgram body) = show body


data DStmt = String ::= DExpr            -- ^ Declaration
           | DExpr   := DExpr            -- ^ Assignment
           | DExpr DExpr                 -- ^ Wrapper for an expression
           | DIf DExpr DBody DBody       -- ^ If expr then body else body end
           | DWhile DExpr DBody          -- ^ While ... loop ... end
           | DFor String DIterable DBody -- ^ For ident in iter loop ... end
           | DLoop DBody                 -- ^ Loop forever
           deriving Show
           -- TODO


data DIterable = DIterableExpr DExpr        -- ^ Wrapper for an expression
               | DIterableRange DExpr DExpr -- ^ Range lower..upper
               deriving Show
               -- TODO


newtype DBody = DBody [DStmt]
instance Show DBody where
  show (DBody stmts) = intercalate ";\n" (map show stmts)


-- *** DExpr


data DExpr -- | *** Primitives
           = DAtom String         -- ^ Identifier
           | DBool Bool           -- ^ Boolean
           | DInt Integer         -- ^ Integer
           | DReal Double         -- ^ Floating point
           | DString String       -- ^ String (sequence of bytes)
           | DFunc { d_params :: [String]
                   , d_body :: DBody
                   -- , d_closure :: Env
                   }              -- ^ Function literal via "func" keyword
           -- | *** Container literals
           | DArray [DExpr]       -- ^ Array literal via BRACKETS
           | DTuple [(String, DExpr)]
                                  -- ^ Tuple literal via BRACES
           -- | *** Operations
           | DIndex DExpr DExpr   -- ^ Indexing via BRACKETS
           | DCall DExpr [DExpr]  -- ^ Function call via PARENS
           | DMember DExpr (Either String Int)
                                  -- ^ Member access via DOT operator
           | DOp DOp              -- ^ Operation via one of predefined operators


data DBinaryOp = DAnd
               | DOr
               | DXor
               | DAdd
               | DSub
               | DMul
               | DDiv
               | DLT
               | DGT
               | DLE
               | DGE
               | DEqual
               | DNotEqual
               deriving Enum



instance Show DBinaryOp where
  show DAnd      = "and"
  show DOr       = "or"
  show DXor      = "xor"
  show DAdd      = "+"
  show DSub      = "-"
  show DMul      = "*"
  show DDiv      = "/"
  show DLT       = "<"
  show DGT       = ">"
  show DLE       = "<="
  show DGE       = ">="
  show DEqual    = "="
  show DNotEqual = "/="


allSymbolicOps :: [DBinaryOp]
allSymbolicOps = [DAdd ..]


data DUnaryOp = DUnaryMinus
              | DUnaryPlus
              | DUnaryNot


instance Show DUnaryOp where
  show DUnaryMinus = "-"
  show DUnaryPlus  = "+"
  show DUnaryNot   = "not "


data DTypeIndicator = DTypeInt
                    | DTypeReal
                    | DTypeBool
                    | DTypeString
                    | DTypeEmpty
                    | DTypeArray
                    | DTypeTuple
                    | DTypeFunc

instance Show DTypeIndicator where
  show (DTypeInt) = "int"
  show (DTypeReal) = "real"
  show (DTypeBool) = "bool"
  show (DTypeString) = "string"
  show (DTypeEmpty) = "empty"
  show (DTypeArray) = "[]"
  show (DTypeTuple) = "{}"
  show (DTypeFunc) = "func"

data DOp = DUnaryOp DUnaryOp DExpr
         | DBinaryOp DExpr DBinaryOp DExpr
         | DExpr `IsInstance` DTypeIndicator


instance Show DOp where
  show (DUnaryOp op expr)      = "(" ++ (show op) ++ (show expr) ++ ")"
  show (DBinaryOp lhs op rhs)  = "(" ++ (show lhs) ++ " " ++ (show op) ++ " " ++ (show rhs) ++ ")"
  show (lhs `IsInstance` rhs) = "(" ++ (show lhs) ++ " is " ++ (show rhs) ++ ")"


instance Show DExpr where
  show (DAtom name) = name
  show (DBool True) = "true"
  show (DBool False) = "false"
  show (DInt i) = show i
  show (DReal i) = show i
  show (DString contents) = "\"" ++ contents ++ "\""
  show (DFunc args body) = "func(" ++ (intercalate ", " args) ++ ") is " ++ show body ++ " end"
  show (DArray items) = "[" ++ (intercalate ", " (map show items)) ++ "]"
  show (DTuple items) =  "{" ++ (intercalate ", " (map printItem items)) ++ "}"
    where
        printItem :: (String, DExpr) -> String
        printItem (k, v) = if null k then                     show v
                                     else k ++ " := " ++ show v
  show (DIndex lhs idx) = (show lhs) ++ "[" ++ (show idx) ++ "]"
  show (DCall fn args) = (show fn) ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
  show (DMember lhs member) = (show lhs) ++ "." ++ (either (show . DAtom) show member)
  show (DOp op) = show op


-- *** DExpr Parser


readDProgram :: String -> ThrowsError DProgram
readDProgram = readOrThrowD program

readDExpr :: String -> ThrowsError DExpr
readDExpr = readOrThrowD (expr <* eof)


-- readDExprList :: String -> ThrowsError [DExpr]
-- readDExprList = readOrThrowD (endBy expr spaces)


readOrThrowD :: Parser a -> String -> ThrowsError a
readOrThrowD parser input = case parse parser "d" input of
    Left err  -> throwError $ Parser err
    Right val -> return val


-- The lexer

language = javaStyle
            { P.caseSensitive  = True
            , P.reservedNames = [ "true", "false"
                                , "not", "and", "or", "xor"
                                , "is", "end", "func"
                                ]
            , P.reservedOpNames = ["..", ".", "=>", ":="] ++
                                  (map show allSymbolicOps)
            }

lexer          = P.makeTokenParser language

parens         = P.parens         lexer
braces         = P.braces         lexer
brackets       = P.brackets       lexer
identifier     = P.identifier     lexer
decimal        = P.decimal        lexer
reserved       = P.reserved       lexer
naturalOrFloat = P.naturalOrFloat lexer
commaSep       = P.commaSep       lexer
dot            = P.dot            lexer
semi           = P.semi           lexer
reservedOp     = P.reservedOp     lexer
whiteSpace     = P.whiteSpace     lexer


program :: Parser DProgram
program = liftM DProgram (whiteSpace >> body <* eof)


string :: Parser DExpr
string = do
                char '"'
                x <- many $ noneOf "\""
                char '"'
                return $ DString x


atom :: Parser DExpr
atom = identifier >>= return . DAtom


bool :: Parser DExpr
bool = (reserved "true" >> return (DBool True))
   <|> (reserved "false" >> return (DBool False))


number :: Parser DExpr
number = naturalOrFloat >>= return . either DInt DReal


primitive :: Parser DExpr
primitive = bool
        <|> number
        <|> string
        <|> atom


-- mainparser :: Parser DExpr
-- mainparser = m_whiteSpace >> stmtparser <* eof
--     where
--       stmtparser :: Parser Stmt
--       stmtparser = fmap Seq (m_semiSep1 stmt1)
--       stmt1 = (m_reserved "nop" >> return Nop)
--               <|> do { v <- m_identifier
--                      ; m_reservedOp ":="
--                      ; e <- exprparser
--                      ; return (v := e)
--                      }
--               <|> do { m_reserved "if"
--                      ; b <- exprparser
--                      ; m_reserved "then"
--                      ; p <- stmtparser
--                      ; m_reserved "else"
--                      ; q <- stmtparser
--                      ; m_reserved "fi"
--                      ; return (If b p q)
--                      }
--               <|> do { m_reserved "while"
--                      ; b <- exprparser
--                      ; m_reserved "do"
--                      ; p <- stmtparser
--                      ; m_reserved "od"
--                      ; return (While b p)
--                      }


indexing :: Parser (DExpr -> DExpr)
indexing = try $ do
  idx <- brackets expr
  return $ flip DIndex idx


calling :: Parser (DExpr -> DExpr)
calling = try $ do
  args <- parens $ commaSep expr
  return $ flip DCall args


membering :: Parser (DExpr -> DExpr)
membering = try $ do
  dot
  choice [ identifier >>= return . Left
         , decimal >>= return . Right . fromIntegral
         ]
    >>= return . flip DMember


typeChecking :: Parser (DExpr -> DExpr)
typeChecking = try $ do
  reserved "is"
  typ <- typeIndicator
  return $ DOp . (`IsInstance` typ)
    where typeIndicator = (reserved "int"      >> return DTypeInt)
                      <|> (reserved "real"     >> return DTypeReal)
                      <|> (reserved "bool"     >> return DTypeBool)
                      <|> (reserved "string"   >> return DTypeString)
                      <|> (reserved "empty"    >> return DTypeEmpty)
                      <|> (brackets whiteSpace >> return DTypeArray)
                      <|> (braces   whiteSpace >> return DTypeTuple)
                      <|> (reserved "func"     >> return DTypeFunc)



literal :: Parser DExpr
literal = array
      <|> tuple
      <|> func


array :: Parser DExpr
array = brackets (commaSep expr) >>= return . DArray


tuple :: Parser DExpr
tuple = braces (commaSep item) >>= return . DTuple
    where item :: Parser (String, DExpr)
          item = do
            k <- key
            v <- expr
            return (k, v)
          key :: Parser String
          key = option "" $ try $ do
            k <- identifier
            reservedOp ":="
            return k


func :: Parser DExpr
func = do
  reserved "func"
  params <- option [] $ parens $ commaSep identifier
  b <- funcBody
  return $ DFunc { d_params = params, d_body = b }
    where funcBody :: Parser DBody
          funcBody = full <|> short

          full :: Parser DBody
          full = do
            reserved "is"
            b <- body
            reserved "end"
            return b

          short :: Parser DBody
          short = do
            reservedOp "=>"
            e <- expr
            return $ DBody [DExpr e]


body :: Parser DBody
body = statements >>= return . DBody


statements :: Parser [DStmt]
statements = statement `endBy` semi


statement :: Parser DStmt
statement = expr >>= return . DExpr
     -- <|> decl
     -- <|> ...
-- TODO:
-- Statement ::= Decl
--             | Assignment
--             | Expr
--             | If
--             | While
--             | For
--             | Loop

expr :: Parser DExpr
expr = buildExpressionParser table term
   <?> "expression"


term :: Parser DExpr
term = parser
   <?> "term"
   where
    parser = do
      p  <- primary
      tails <- many termTail
      return $ foldl (flip id) p tails


primary :: Parser DExpr
primary = parens expr
      <|> primitive
      <|> literal
      <?> "term primary"


termTail :: Parser (DExpr -> DExpr)
termTail = calling
       <|> indexing
       <|> membering
       <|> typeChecking
       <?> "term tail"


table = [[ prefix (reservedOp "-") DUnaryMinus
         , prefix (reservedOp "+") DUnaryPlus
         , prefix (reserved "not") DUnaryNot
         ]
        ,[ binaryOp "*" DMul, binaryOp "/" DDiv ]
        ,[ binaryOp "+" DAdd, binaryOp "-" DSub ]
        -- < | <= | > | >= | = | /=
        ,[ binaryOp (show DLT) DLT
         , binaryOp (show DLE) DLE
         , binaryOp (show DGT) DGT
         , binaryOp (show DGE) DGE
         , binaryOp (show DEqual) DEqual
         , binaryOp (show DNotEqual) DNotEqual
         ]
        ,[ binaryKeyword "and" DAnd
         , binaryKeyword "or"  DOr
         , binaryKeyword "xor" DXor
         ]
        ]

prefix match name = Prefix parser
  where parser = do match
                    return $ DOp . DUnaryOp name

binary match name = Infix parser AssocLeft
  where parser = do match
                    return (\lhs rhs -> DOp $ DBinaryOp lhs name rhs)
binaryOp      = binary . reservedOp
binaryKeyword = binary . reserved


-- *** LispVal


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      l_body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, l_body = body, closure = env}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


instance Show LispVal where show = showVal


-- *** LispError


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String


showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr


instance Show LispError where show = showError


type ThrowsError = Either LispError


trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val


type IOThrowsError = ExceptT LispError IO


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


-- *** Variables


type Env = IORef [(String, IORef LispVal)]


nullEnv :: IO Env
nullEnv = newIORef []


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)


setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value


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


-- *** Core


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env badForm@(List (Atom "quote":_)) = throwError $ BadSpecialForm "Quote takes only one argument" badForm
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
     makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
     makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
     makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
     makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
     makeVarArgs varargs env [] body
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env
apply (IOFunc func) args = func args
apply notFunc _ = throwError $ NotFunction "Not a function" (showVal notFunc)


makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal


-- *** Primitives


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]


-- primitiveBindings :: IO Env
-- primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
--      where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right


numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString


unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool (Number 0) = return False
unpackBool (Number _) = return True
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool


car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList


cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList


cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) &&
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)


equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
      primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                         [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
      eqvEquals <- eqv [arg1, arg2]
      return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


-- *** IO Primitives


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars (map (makeFunc IOFunc) ioPrimitives
                                             ++ map (makeFunc PrimitiveFunc) primitives))
     where makeFunc constructor (var, func) = (var, constructor func)


applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args


makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode


closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False


readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port ) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)


readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename


load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList


readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
