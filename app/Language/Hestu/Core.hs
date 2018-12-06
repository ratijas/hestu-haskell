{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Hestu.Core where

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


-- *** D AST


newtype DProgram = DProgram DBody
instance Show DProgram where
  show (DProgram body) = show body


data DStmt = String ::= DExpr            -- ^ Declaration
           | DExpr   := DExpr            -- ^ Assignment
           | DExpr DExpr                 -- ^ Wrapper for an expression
           | DIf DExpr DBody DBody       -- ^ If expr then body else body end
           | DWhile DExpr DBody          -- ^ While ... loop ... end
           | DFor String DIterable DBody -- ^ For ident in iter loop ... end


instance Show DStmt where
  show (string ::= expr) = "var " ++ string ++ " := " ++ show expr
  show (expr1 := exp2) = show expr1 ++ " := " ++ show exp2
  show (DExpr exp) = show exp
  show (DIf expr body1 body2) = "if " ++ show expr ++" then " ++ show body1 ++ " else "++ show body2
  show (DWhile expr body) = "while " ++ show expr ++ " loop " ++ show body ++ " end"
  show (DFor string iterable body) = "for " ++ string ++ " in "++ show iterable ++ " loop " ++ show body ++ " end"

data DIterable = DIterableExpr DExpr        -- ^ Wrapper for an expression
               | DIterableRange DExpr DExpr -- ^ Range lower..upper


instance Show DIterable where
  show (DIterableExpr iexp) = show iexp
  show (DIterableRange expr1 expr2) = show expr1 ++ ".." ++ show expr2

newtype DBody = DBody [DStmt]
instance Show DBody where
  show (DBody stmts) = intercalate ";\n" (map show stmts) ++ ";"


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
           | DEmpty


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
  show DEmpty = "<empty>"


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
               deriving (Enum, Eq)


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


-- *** D Parser


language = javaStyle
            { P.caseSensitive  = True
            , P.reservedNames = [ "true", "false"
                                , "not", "and", "or", "xor"
                                , "is", "end", "func"
                                , "if", "then", "else"
                                , "while", "for", "loop", "var", "in"
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
  whiteSpace
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
         , (decimal <* whiteSpace) >>= return . Right . fromIntegral
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


iterable :: Parser DIterable
iterable = do
  exp1 <- expr
  option (DIterableExpr exp1) $ try $ do
    reservedOp ".."
    exp2 <- expr
    return $ DIterableRange exp1 exp2


statement :: Parser DStmt
statement = d_if
        <|> d_for
        <|> d_while
        <|> d_decl
        <|> d_loop
        <|> d_assignment
        <|> d_expr
  where
    d_assignment :: Parser DStmt
    d_assignment = try $ do
      lvalue <- expr
      reservedOp ":="
      rvalue <- expr
      return $ lvalue := rvalue

    d_expr :: Parser DStmt
    d_expr = liftM DExpr expr

    d_if :: Parser DStmt
    d_if = do
      reserved "if"
      cond <- expr
      reserved "then"
      body1 <- body
      reserved "else"
      body2 <- body
      reserved "end"
      return $ DIf cond body1 body2

    d_while :: Parser DStmt
    d_while = do
      reserved "while"
      cond <- expr
      reserved "loop"
      b <- body
      reserved "end"
      return $ DWhile cond b

    d_decl :: Parser DStmt
    d_decl = do
       reserved "var"
       var <- identifier
       val <- option DEmpty $ do
        reservedOp ":="
        expr
       return $ var ::= val

    d_loop :: Parser DStmt
    d_loop = do
      reserved "loop"
      b <- body
      reserved "end"
      return $ DWhile (DBool True) b

    d_for :: Parser DStmt
    d_for = do
      reserved "for"
      (var :: String) <- identifier
      reserved "in"
      it <- iterable
      reserved "loop"
      b <- body
      reserved "end"
      return $ DFor var it b


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


-- *** D Evaluation


-- Various readers for debugging


readAny :: Parser a -> String -> ThrowsError a
readAny parser input = case parse parser "Hestu" input of
  Left err -> throwError $ Parser err
  Right val -> return val


readExpr :: String -> ThrowsError DExpr
readExpr = readAny expr


readStmt :: String -> ThrowsError DStmt
readStmt = readAny statement


readBody :: String -> ThrowsError DBody
readBody = readAny body


-- eval & apply


execBody :: DBody -> ThrowsError DExpr
execBody (DBody body) = do
  xs <- mapM exec body
  return $ last (DEmpty : xs)


exec :: DStmt -> ThrowsError DExpr
exec (DExpr expr) = eval expr


eval :: DExpr -> ThrowsError DExpr
eval DEmpty = throwError Yahaha -- TODO: EmptyRockException
eval val@(DBool _)   = return val
eval val@(DInt _)    = return val
eval val@(DReal _)   = return val
eval val@(DString _) = return val
eval val@(DArray xs) = mapM eval xs >>= return . DArray
eval val@(DTuple xs) = do
  vals <- mapM eval values
  return $ DTuple $ zip keys vals
  where keys = (map fst xs)
        values = (map snd xs)

eval (DIndex arrayExpr indexExpr) = do
  array <- eval arrayExpr
  list <- case array of
    DArray arr -> return arr
    DString str -> return $ map (DString . return) str
    _ -> throwError $ TypeMismatch "index can only be applied to arrays" DTypeEmpty -- TODO: DExpr to DTypeIndicator
  index <- eval indexExpr
  case index of
    DInt idx | 0 <= i && i < length list -> return $ list !! i
             | otherwise                 -> throwError $ Default "index out of range"
      where i = fromIntegral idx
    _  -> throwError $ TypeMismatch "index must be int" DTypeEmpty -- TODO

eval (DMember tupleExpr index) = do
  tuple <- eval tupleExpr
  case tuple of
    DTuple tup -> case index of
      Left name -> case lookup name tup of
        Just x -> return x
        _      -> throwError $ Default $ "attribute error: no such field: " ++ show name
      Right idx | 0 <= idx && idx < length tup -> return $ snd $ tup !! idx
                | otherwise -> throwError $ Default $ "attribute error: index out of range"
    _ -> throwError $ TypeMismatch "member access can only be applied to tuples" DTypeEmpty -- TODO

eval (DOp (DUnaryOp operator expr)) = do
  operand <- eval expr
  unaryOperation operator operand

eval (DOp (DBinaryOp lhsExpr op rhsExpr)) = do
  lhs <- eval lhsExpr
  rhs <- eval rhsExpr
  binaryOperation lhs op rhs

eval (DOp (expr `IsInstance` typ)) = do
  val <- eval expr
  return $ DBool $ val `isInstance` typ


unaryOperation :: DUnaryOp -> DExpr -> ThrowsError DExpr
unaryOperation DUnaryMinus (DInt operand) = return $ DInt $ operand * (-1)
unaryOperation DUnaryMinus (DReal operand) = return $ DReal $ operand * (-1)
unaryOperation DUnaryPlus val@(DInt operand) = return $ val
unaryOperation DUnaryPlus val@(DReal operand) = return $ val
unaryOperation DUnaryNot (DBool operand) = return $ DBool $ not operand
unaryOperation _ _ = throwError $ TypeMismatch "wrong type for unary operation" DTypeEmpty -- TODO


binaryOperation :: DExpr -> DBinaryOp -> DExpr -> ThrowsError DExpr
binaryOperation lhs op rhs =
  case lookup op boolOpFunc of
    Just boolOp -> do
      l <- unpackBool lhs
      r <- unpackBool rhs
      return $ DBool $ boolOp l r
    _ -> case lookup op equalityOpFunc of
      Just eqOp -> do
        l <- unpackReal lhs
        r <- unpackReal rhs
        return $ DBool $ eqOp l r
      _ -> case lookup op mathOpFunc of
        Just mathOp -> mathOp lhs rhs
        _ -> throwError $ Default "not implemented"


boolOpFunc :: [(DBinaryOp, Bool -> Bool -> Bool)]
boolOpFunc = [(DAnd, (&&)),
              (DOr,  (||)),
              (DXor, (/=))]


equalityOpFunc :: [(DBinaryOp, Double -> Double -> Bool)]
equalityOpFunc = [(DLT, (<)),
                  (DGT, (>)),
                  (DLE, (<=)),
                  (DGE, (>=)),
                  (DEqual, (==)),
                  (DNotEqual, (/=))]


mathOpFunc :: [(DBinaryOp, DExpr -> DExpr -> ThrowsError DExpr)]
mathOpFunc = [(DAdd, d_add),
              (DSub, d_sub),
              (DMul, d_mul),
              (DDiv, d_div)]


unpackBool :: DExpr -> ThrowsError Bool
unpackBool (DBool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" DTypeEmpty -- TODO


unpackReal :: DExpr -> ThrowsError Double
unpackReal (DInt int) = return $ fromIntegral int
unpackReal (DReal real) = return real
unpackReal notReal = throwError $ TypeMismatch "real" DTypeEmpty -- TODO


d_add :: DExpr -> DExpr -> ThrowsError DExpr
d_add (DInt exp1)     (DInt exp2)     = return $ DInt(exp1 + exp2)
d_add (DReal exp1)    (DReal exp2)    = return $ DReal(exp1 + exp2)
d_add (DInt exp1)     (DReal exp2)    = return $ DReal((fromIntegral exp1) + exp2)
d_add (DReal exp1)    (DInt exp2)     = return $ DReal(exp1 + (fromIntegral exp2))
d_add (DString str1)  (DString str2)  = return $ DString(str1 ++ str2)
d_add (DTuple tuple1) (DTuple tuple2) = return $ DTuple(tuple1 ++ tuple2)
d_add (DArray arr1)   (DArray arr2)   = return $ DArray(arr1 ++ arr2)
d_add _ _ = throwError $ TypeMismatch "add" DTypeEmpty -- TODO

d_sub :: DExpr -> DExpr -> ThrowsError DExpr
d_sub (DInt exp1)  (DInt exp2)  = return $ DInt(exp1 - exp2)
d_sub (DReal exp1) (DReal exp2) = return $ DReal(exp1 - exp2)
d_sub (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) - exp2)
d_sub (DReal exp1) (DInt exp2)  = return $ DReal(exp1 - (fromIntegral exp2))
d_sub _ _ = throwError $ TypeMismatch "sub" DTypeEmpty -- TODO


d_mul :: DExpr -> DExpr -> ThrowsError DExpr
d_mul (DInt exp1)  (DInt exp2)  = return $ DInt(exp1 * exp2)
d_mul (DReal exp1) (DReal exp2) = return $ DReal(exp1 * exp2)
d_mul (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) * exp2)
d_mul (DReal exp1) (DInt exp2)  = return $ DReal(exp1 * (fromIntegral exp2))
d_mul _ _ = throwError $ TypeMismatch "mul" DTypeEmpty -- TODO


d_div :: DExpr -> DExpr -> ThrowsError DExpr
d_div (DInt exp1)  (DInt exp2)  = return $ DInt(quot exp1 exp2)
d_div (DReal exp1) (DReal exp2) = return $ DReal(exp1 / exp2)
d_div (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) / exp2)
d_div (DReal exp1) (DInt exp2)  = return $ DReal(exp1 / (fromIntegral exp2))
d_div _ _ = throwError $ TypeMismatch "div" DTypeEmpty -- TODO


isInstance :: DExpr -> DTypeIndicator -> Bool
(DInt _)   `isInstance` DTypeInt = True
(DReal _)  `isInstance` DTypeReal = True
(DBool _)  `isInstance` DTypeBool = True
(DString _) `isInstance` DTypeString = True
DEmpty     `isInstance` DTypeEmpty = True
(DArray _) `isInstance` DTypeArray = True
(DTuple _) `isInstance` DTypeTuple = True
(DFunc {}) `isInstance` DTypeFunc = True
_ `isInstance` _ = False


-- *** Error Handling


toTypeIndicator :: DExpr -> DTypeIndicator

toTypeIndicator (DInt _)  = DTypeInt
toTypeIndicator (DReal _) = DTypeReal
toTypeIndicator (DBool _) = DTypeBool
toTypeIndicator (DString _) = DTypeString
toTypeIndicator DEmpty = DTypeEmpty
toTypeIndicator (DArray _) = DTypeArray
toTypeIndicator (DTuple _) = DTypeTuple
toTypeIndicator (DFunc {}) = DTypeFunc
toTypeIndicator _ = DTypeEmpty -- error todo

data HestuError = NumArgs Integer [DExpr]
                | TypeMismatch String DTypeIndicator
                | Parser ParseError
                | NotFunction String String
                | UnboundVar String String
                | Default String
                | Yahaha               -- ^ Null pointer exception, when trying to evaluate DEmpty

instance Show HestuError where
  show (NumArgs expected found) = "Expected " ++ show expected
                                       ++ "  args; found values " ++ show found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar  message varname)  = message ++ ": " ++ varname
  show (Default str) = "unknown error. " ++ str
  show (Yahaha) = "Yahaha"

type ThrowsError = Either HestuError


trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val

