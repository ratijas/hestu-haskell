{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE OverloadedStrings         #-}

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
import           Text.Parsec.Expr
import           Text.Parsec.Language (javaStyle)
import           Text.ParserCombinators.Parsec hiding (spaces, string)

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM


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
           | DReturn DExpr               -- ^ Early return statement


instance Show DStmt where
  show (string ::= expr) = "var " ++ string ++ " := " ++ show expr
  show (expr1 := exp2) = show expr1 ++ " := " ++ show exp2
  show (DExpr exp) = show exp
  show (DIf expr body1 body2) = "if " ++ show expr ++" then " ++ show body1 ++ " else "++ show body2
  show (DWhile expr body) = "while " ++ show expr ++ " loop " ++ show body ++ " end"
  show (DFor string iterable body) = "for " ++ string ++ " in "++ show iterable ++ " loop " ++ show body ++ " end"
  show (DReturn arg) = "return" ++ val
    where val = case arg of DEmpty -> ""
                            v -> " " ++ show v

data DIterable = DIterableExpr DExpr        -- ^ Wrapper for an expression
               | DIterableRange DExpr DExpr -- ^ Range lower..upper


instance Show DIterable where
  show (DIterableExpr iexp) = show iexp
  show (DIterableRange expr1 expr2) = show expr1 ++ ".." ++ show expr2


newtype DIterator = DIterator [DExpr]


iteratorGet :: DExpr -> IOThrowsError DIterator
iteratorGet iterable =
  case iterable of
    -- loop over individual characters of a string
    DString str -> return $ DIterator $ map (DString . return) str
    -- loop over individual array elements
    DArray array -> return . DIterator . V.toList =<< V.freeze array
    -- loop over tuple named keys
    DTuple iterator -> return $ DIterator $ map (DString . fst) $ V.toList iterator
    x -> liftThrows $ typeMismatch'or'yahaha "string, array or tuple" x


iteratorNext :: DIterator -> Maybe (DIterator, DExpr)
iteratorNext (DIterator (x:xs)) = Just ((DIterator xs), x)
iteratorNext (DIterator []) = Nothing


newtype DBody = DBody [DStmt]
instance Show DBody where
  show (DBody stmts) = intercalate ";\n" (map show stmts) ++ ";"


data DExpr -- | *** Primitives
           = DEmpty                             -- ^ Represents an absence of any value
           | DAtom String                       -- ^ Identifier
           | DBool Bool                         -- ^ Boolean
           | DInt Integer                       -- ^ Integer
           | DReal Double                       -- ^ Floating point
           | DString String                     -- ^ String (sequence of bytes)
           -- | *** Container literals
           | DArrayLit (V.Vector DExpr)         -- ^ Array literal via BRACKETS
           | DTupleLit (V.Vector (String, DExpr))
                                                -- ^ Tuple literal via BRACES
           -- | *** Container instances
           | DArray (VM.IOVector DExpr)         -- ^ Array instances
           | DTuple (V.Vector (String, IORef DExpr))
                                                -- ^ Tuple instances.
                                                --   Tuple keys and their order are immutable,
                                                --   but individual fields do support assignment.
           -- | *** Operations
           | DIndex DExpr DExpr                 -- ^ Indexing via BRACKETS
           | DMember DExpr (Either String Int)  -- ^ Member access via DOT operator
           | DCall DExpr [DExpr]                -- ^ Function call via PARENS
           | DOp DOp                            -- ^ Operation via one of predefined operators
           -- *** Functions
           | DFuncLit { lit_params :: [String]
                      , lit_body :: DBody
                      }                         -- ^ Function literal via "func" keyword
           | DFunc { d_params :: [String]
                   , d_body :: DBody
                   , d_closure :: Env
                   }                            -- ^ Function instance with closure
           | DPrimitiveFunc ([DExpr] -> ThrowsError DExpr)
           | DPrimitiveIOFunc ([DExpr] -> IOThrowsError DExpr)


instance Show DExpr where
  show DEmpty = "<empty>"
  show (DAtom name) = name
  show (DBool True) = "true"
  show (DBool False) = "false"
  show (DInt i) = show i
  show (DReal i) = show i
  show (DString contents) = "\"" ++ contents ++ "\""

  show (DArrayLit items) = "[" ++ (intercalate ", " (V.toList $ V.map show items)) ++ "]"
  show (DTupleLit items) = "{" ++ (intercalate ", " (V.toList $ V.map showTupleItem items)) ++ "}"
    where
      showTupleItem :: (String, DExpr) -> String
      showTupleItem (k, v) = if null k
        then                show v
        else k ++ " := " ++ show v

  show (DArray _) = "[...]"
  show (DTuple _) =  "{...}"

  show (DIndex lhs idx) = (show lhs) ++ "[" ++ (show idx) ++ "]"
  show (DMember lhs member) = (show lhs) ++ "." ++ (either (show . DAtom) show member)
  show (DCall fn args) = (show fn) ++ "(" ++ (intercalate ", " (map show args)) ++ ")"
  show (DOp op) = show op

  show (DFuncLit args body) = "func(" ++ (intercalate ", " args) ++ ") is " ++ show body ++ " end"
  show (DFunc args body closure) = "closure(" ++ (intercalate ", " args) ++ ") is " ++ show body ++ " end"

  show (DPrimitiveFunc _) = "<primitive>"
  show (DPrimitiveIOFunc _) = "<io>"

showTupleItem :: (String, DExpr) -> String
showTupleItem (k, v) = if null k
  then                show v
  else k ++ " := " ++ show v


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
                                , "return"
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
stringLiteral  = P.stringLiteral  lexer


program :: Parser DProgram
program = liftM DProgram (whiteSpace >> body <* eof)


string :: Parser DExpr
string = stringLiteral >>= return . DString


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
array = brackets (commaSep expr) >>= return . DArrayLit . V.fromList


tuple :: Parser DExpr
tuple = braces (commaSep item) >>= return . DTupleLit . V.fromList
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
  return $ DFuncLit { lit_params = params, lit_body = b }
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
        <|> d_return
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

    d_return :: Parser DStmt
    d_return = do
      reserved "return"
      arg <- option DEmpty expr
      return $ DReturn arg


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


execBody :: Env -> DBody -> IOThrowsError DExpr
execBody env (DBody body) = do
  xs <- mapM (execStmt env) body
  return $ last (DEmpty : xs)


execStmt :: Env -> DStmt -> IOThrowsError DExpr

execStmt env (DExpr expr) = eval env expr

execStmt env (name ::= expr) = do
  val <- eval env expr
  defineVar env name val

execStmt env ((DAtom var) := expr) = do
  val <- eval env expr
  setVar env var val

execStmt env (_ := expr) = throwError Yahaha  -- TODO

execStmt env (DIf condition thenBody elseBody) = do
  cond <- eval env condition
  execBody env $ case cond of
    DBool True -> thenBody
    __________ -> elseBody

execStmt env loop@(DWhile condition loopBody) = do
  cond <- eval env condition
  case cond of
    DBool True -> execBody env loopBody >> execStmt env loop
    __________ -> return DEmpty

execStmt env loop@(DFor name iterable body) =
  case iterable of
    DIterableExpr expr -> do
      iterable' <- eval env expr
      iterator <- iteratorGet iterable'
      defineVar env name DEmpty
      forLoopIter env name iterator body
      where
        forLoopIter env name iterator body = do
          case iteratorNext iterator of
            Nothing            -> return DEmpty
            Just (iterator, x) -> do
              setVar env name x
              execBody env body
              forLoopIter env name iterator body

    DIterableRange lower upper -> do
      (l, u) <- range (lower, upper)
      defineVar env name (DInt l)
      forLoop name (l, u) body

  where range :: (DExpr, DExpr) -> IOThrowsError (Integer, Integer)
        range (lower, upper) = do
          low <- eval env lower
          up <- eval env upper
          case (low, up) of
            ((DInt l), (DInt u)) -> return (l, u)
            ____________________ -> throwError $ TypeMismatch "int" (toTypeIndicator low)

        forLoop :: String -> (Integer, Integer) -> DBody -> IOThrowsError DExpr
        forLoop name (l, u) body = do
          let condition = DOp $ DBinaryOp (DInt l) DLT (DInt u)
          cond <- eval env condition
          case cond of
            DBool True -> do
              setVar env name (DInt l)
              execBody env body
              forLoop name (l + 1, u) body
            __________ -> return DEmpty

execStmt env (DReturn arg) = throwError $ RaiseReturn arg


eval :: Env -> DExpr -> IOThrowsError DExpr
eval env DEmpty          = return DEmpty
eval env (DAtom atom)    = getVar env atom
eval env val@(DBool _)   = return val
eval env val@(DInt _)    = return val
eval env val@(DReal _)   = return val
eval env val@(DString _) = return val

eval env (DArrayLit xs) = do
  ys <- V.mapM (eval env) xs
  zs <- liftIO $ V.thaw ys
  return $ DArray zs

eval env (DTupleLit xs) = do
  let (keys, values) = V.unzip xs
  ys <- V.mapM (eval env) values
  zs <- liftIO $ V.mapM newIORef ys
  return $ DTuple $ V.zip keys zs

eval env val@(DArray _) = return val
eval env val@(DTuple _) = return val


eval env (DIndex containerExpr indexExpr) = do
  -- get reasonable index
  index <- eval env indexExpr
  i <- case index of
    DInt idx -> return $ fromIntegral idx
    other -> liftThrows $ typeMismatch'or'yahaha "int" other

  -- get reasonable container
  container <- eval env containerExpr

  case container of
    DArray xs -> do
      item <- liftIO $ VM.read xs i
      return item
      -- throwError $ AttributeError container $ show i -- TODO: informative error message
    DString str -> if 0 <= i && i < length str
      then return $ DString $ return $ str !! i
      else throwError $ AttributeError container $ show i
    other -> liftThrows $ typeMismatch'or'yahaha "array or string" other

eval env (DMember tupleExpr index) = do
  tuple <- eval env tupleExpr
  kv <- case tuple of
    DTuple t -> return t
    other    -> liftThrows $ typeMismatch'or'yahaha "tuple" other

  ref <- case index of
    Left name -> case V.find ((== name) . fst) kv of
      Just (_, value) -> return value
      _______________ -> throwError $ AttributeError tuple name
    Right idx
      | 0 <= idx && idx < V.length kv
        -> return $ snd $ (V.!) kv idx
      | otherwise
        -> throwError $ AttributeError tuple $ show idx
  liftIO $ readIORef ref

eval env (DCall function args) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

eval env (DOp (DUnaryOp operator expr)) = do
  operand <- eval env expr
  liftThrows $ unaryOperation operator operand

eval env (DOp (DBinaryOp lhsExpr op rhsExpr)) = do
  lhs <- eval env lhsExpr
  let rhs = eval env rhsExpr -- do not force evaluation just yet
  binaryOperation lhs op rhs

eval env (DOp (expr `IsInstance` typ)) = do
  val <- eval env expr
  return $ DBool $ val `isInstance` typ

eval env (DFuncLit params body) = return $ DFunc params body env


apply :: DExpr -> [DExpr] -> IOThrowsError DExpr
apply (DFunc params body closure) args =
  if num params /= num args
    then throwError $ NumArgs (num params) args
    else (liftIO $ bindVars closure $ zip params args) >>= exec
  where num = toInteger . length
        exec env = (execBody env body) `catchError` recoverOnReturn
        recoverOnReturn :: HestuError -> IOThrowsError DExpr
        recoverOnReturn (RaiseReturn arg) = return arg
        recoverOnReturn e = throwError e
apply (DPrimitiveFunc func) args = liftThrows $ func args
apply (DPrimitiveIOFunc func) args = func args
apply DEmpty _ = throwError Yahaha
apply notFunc _ = throwError $ NotFunction "Not a function" (show notFunc)


unaryOperation :: DUnaryOp -> DExpr -> ThrowsError DExpr
unaryOperation DUnaryMinus (DInt operand) = return $ DInt $ operand * (-1)
unaryOperation DUnaryMinus (DReal operand) = return $ DReal $ operand * (-1)
unaryOperation DUnaryPlus val@(DInt operand) = return $ val
unaryOperation DUnaryPlus val@(DReal operand) = return $ val
unaryOperation DUnaryNot (DBool operand) = return $ DBool $ not operand
unaryOperation _ val = typeMismatch'or'yahaha "int, real or boolean" val


-- | Takes unevaluated `rhs`, so that it can be lazily evaluated in case of
-- logical short-circuiting operators.
binaryOperation :: DExpr -> DBinaryOp -> IOThrowsError DExpr -> IOThrowsError DExpr
binaryOperation lhs op rhsExpr =
  let boolFuncM = lookup op boolOpFunctions
      eqFuncM   = lookup op equalityOpFunctions
      mathFuncM = lookup op mathOpFunctions
  in case (boolFuncM, eqFuncM, mathFuncM) of
    (Just boolFunc, _, _) -> applyBoolOp boolFunc
    (_, Just eqFunc,   _) -> applyEqOp eqFunc
    (_, _, Just mathFunc) -> rhsExpr >>= mathFunc lhs
    _____________________ -> liftThrows $ throwError Yahaha
  where
    applyBoolOp f = do
      l <- liftThrows $ unpackBool lhs
      f l rhsExpr >>= return . DBool

    applyEqOp f = do
        l <- unpack lhs
        r <- rhsExpr >>= unpack
        return $ DBool $ f l r
        where unpack = liftThrows . unpackReal


-- | `rhs` is wrapped in monad for lazy evaluation.
boolOpFunctions :: [(DBinaryOp, Bool -> IOThrowsError DExpr -> IOThrowsError Bool)]
boolOpFunctions = [ (DAnd, andF)
                  , (DOr ,  orF)
                  , (DXor, xorF)]
  where
    andF l rhs = if not l
      then return False
      else unpack rhs

    orF l rhs = if l
      then return True
      else unpack rhs

    xorF l rhs = do
      r <- unpack rhs
      return $ l /= r

    unpack = (>>= liftThrows . unpackBool)


equalityOpFunctions :: [(DBinaryOp, Double -> Double -> Bool)]
equalityOpFunctions = [ (DLT, (<))
                      , (DGT, (>))
                      , (DLE, (<=))
                      , (DGE, (>=))
                      , (DEqual, (==))
                      , (DNotEqual, (/=))]


mathOpFunctions :: [(DBinaryOp, DExpr -> DExpr -> IOThrowsError DExpr)]
mathOpFunctions = [ (DAdd, d_add)
                  , (DSub, d_sub)
                  , (DMul, d_mul)
                  , (DDiv, d_div)]


unpackBool :: DExpr -> ThrowsError Bool
unpackBool (DBool b) = return b
unpackBool val = typeMismatch'or'yahaha "boolean" val


unpackReal :: DExpr -> ThrowsError Double
unpackReal (DInt int) = return $ fromIntegral int
unpackReal (DReal real) = return real
unpackReal val = typeMismatch'or'yahaha "int or real" val


d_add :: DExpr -> DExpr -> IOThrowsError DExpr
d_add (DInt exp1)     (DInt exp2)     = return $ DInt(exp1 + exp2)
d_add (DReal exp1)    (DReal exp2)    = return $ DReal(exp1 + exp2)
d_add (DInt exp1)     (DReal exp2)    = return $ DReal((fromIntegral exp1) + exp2)
d_add (DReal exp1)    (DInt exp2)     = return $ DReal(exp1 + (fromIntegral exp2))
d_add (DString str1)  (DString str2)  = return $ DString(str1 ++ str2)
d_add (DTuple tuple1) (DTuple tuple2) = return $ DTuple((V.++) tuple1 tuple2)
d_add (DArray arr1)   (DArray arr2)   = do
  a1 <- V.freeze arr1
  a2 <- V.freeze arr2
  xs <- V.thaw $ (V.++) a1 a2
  return $ DArray xs
d_add x y = throwError $ TypeMismatch "int/real + int/real, strings, tuples or arrays" (toTypeIndicator x)

d_sub :: DExpr -> DExpr -> IOThrowsError DExpr
d_sub (DInt exp1)  (DInt exp2)  = return $ DInt(exp1 - exp2)
d_sub (DReal exp1) (DReal exp2) = return $ DReal(exp1 - exp2)
d_sub (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) - exp2)
d_sub (DReal exp1) (DInt exp2)  = return $ DReal(exp1 - (fromIntegral exp2))
d_sub x y = throwError $ TypeMismatch "int/real - int/real" (toTypeIndicator x)


d_mul :: DExpr -> DExpr -> IOThrowsError DExpr
d_mul (DInt exp1)  (DInt exp2)  = return $ DInt(exp1 * exp2)
d_mul (DReal exp1) (DReal exp2) = return $ DReal(exp1 * exp2)
d_mul (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) * exp2)
d_mul (DReal exp1) (DInt exp2)  = return $ DReal(exp1 * (fromIntegral exp2))
d_mul x y = throwError $ TypeMismatch "int/real * int/real" (toTypeIndicator x)


d_div :: DExpr -> DExpr -> IOThrowsError DExpr
d_div (DInt exp1)  (DInt exp2)  = return $ DInt(quot exp1 exp2)
d_div (DReal exp1) (DReal exp2) = return $ DReal(exp1 / exp2)
d_div (DInt exp1)  (DReal exp2) = return $ DReal((fromIntegral exp1) / exp2)
d_div (DReal exp1) (DInt exp2)  = return $ DReal(exp1 / (fromIntegral exp2))
d_div x y = throwError $ TypeMismatch "int/real / int/real" (toTypeIndicator x)


isInstance :: DExpr -> DTypeIndicator -> Bool
(DInt _)   `isInstance` DTypeInt = True
(DReal _)  `isInstance` DTypeReal = True
(DBool _)  `isInstance` DTypeBool = True
(DString _) `isInstance` DTypeString = True
DEmpty     `isInstance` DTypeEmpty = True
(DArray _) `isInstance` DTypeArray = True
(DTuple _) `isInstance` DTypeTuple = True
(DFuncLit {}) `isInstance` DTypeFunc = True
_ `isInstance` _ = False


-- *** Error Handling


toTypeIndicator :: DExpr -> DTypeIndicator
toTypeIndicator (DInt _)    = DTypeInt
toTypeIndicator (DReal _)   = DTypeReal
toTypeIndicator (DBool _)   = DTypeBool
toTypeIndicator (DString _) = DTypeString
toTypeIndicator (DArray _)  = DTypeArray
toTypeIndicator (DTuple _)  = DTypeTuple
toTypeIndicator (DFuncLit {})  = DTypeFunc
toTypeIndicator _ = DTypeEmpty


data HestuError = NumArgs Integer [DExpr]
                | TypeMismatch String DTypeIndicator
                | Parser ParseError
                | NotFunction String String
                | UnboundVar String String
                | AttributeError DExpr String
                | Default String
                | RaiseReturn DExpr    -- ^ Flow control operator
                | Yahaha               -- ^ Null pointer exception, when trying to evaluate DEmpty

instance Show HestuError where
  show (NumArgs expected found) = "Expected " ++ show expected
                                       ++ " args; found values " ++ show found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (NotFunction message func) = message ++ ": " ++ show func
  show (UnboundVar  message varname)  = message ++ ": " ++ varname
  show (AttributeError object member) = "Attribute error: object " ++ (show object)
                                     ++ " has no attribute " ++ member
  show (Default str) = "unknown error. " ++ str
  show (RaiseReturn arg) = "Return"
  show (Yahaha) = "Ya-ha-ha!"

type ThrowsError = Either HestuError


trapError action = catchError action (return . show)


extractValue :: ThrowsError a -> a
extractValue (Right val) = val


typeMismatch'or'yahaha :: String -> DExpr -> ThrowsError a
typeMismatch'or'yahaha expected found =
  case toTypeIndicator found of
    DTypeEmpty -> throwError Yahaha
    typ -> throwError $ TypeMismatch expected typ


-- *** IO Error Handling


type IOThrowsError = ExceptT HestuError IO


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


-- *** REPL


evalAndPrint :: Env -> String -> IO ()
evalAndPrint env script =  evalString env script >>= putStrLn


evalString :: Env -> String -> IO String
evalString env script = runIOThrows $ (liftThrows $ readBody script)
                                  >>= execBody env
                                  >>= (liftIO . showPlus)


runOne :: String -> IO ()
runOne script = primitiveBindings >>= flip evalAndPrint script


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Hestu>>> ") . evalAndPrint


readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout


-- *** Environment / Variables


type Env = IORef [(String, IORef DExpr)]


nullEnv :: IO Env
nullEnv = newIORef []


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . isJust . lookup var


getVar :: Env -> String -> IOThrowsError DExpr
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)


setVar :: Env -> String -> DExpr -> IOThrowsError DExpr
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> DExpr -> IOThrowsError DExpr
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value


bindVars :: Env -> [(String, DExpr)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


-- *** Primitives


primitives :: [(String, [DExpr] -> ThrowsError DExpr)]
primitives = [("length", builtinLength)]


builtinLength :: [DExpr] -> ThrowsError DExpr
builtinLength [(DArray items)] = return $ DInt $ fromIntegral $ VM.length items
builtinLength [(DTuple items)] = return $ DInt $ fromIntegral $ V.length items
builtinLength [notArray] = throwError $ TypeMismatch "array" (toTypeIndicator notArray)
builtinLength args = throwError $ NumArgs 1 args


ioPrimitives :: [(String, [DExpr] -> IOThrowsError DExpr)]
ioPrimitives = [ ("print", builtinPrint False)
               , ("println", builtinPrint True)
               , ("format", builtinFormat)]


builtinPrint :: Bool -> [DExpr] -> IOThrowsError DExpr
builtinPrint False [(DString s)] = (liftIO $ putStr s) >> return DEmpty
builtinPrint False [x]           = (liftIO $ putStr $ show x) >> return DEmpty
builtinPrint False xs            = mapM (builtinPrint False . return) xs >> return DEmpty
builtinPrint True [(DString s)] = (liftIO $ putStrLn s) >> return DEmpty
builtinPrint True xs = builtinPrint False xs >> (liftIO $ putStrLn "") >> return DEmpty


-- | Largely based on `show` implementation with exception on IO-based types.
builtinFormat :: [DExpr] -> IOThrowsError DExpr
builtinFormat [] = return $ DString ""
builtinFormat xs = liftIO $ liftM (DString . foldr (++) "") $ mapM showPlus xs


showPlus :: DExpr -> IO String
showPlus (DArray xs) =
  V.freeze xs
  >>= V.mapM showPlus
  >>= return . bracketed . intercalate ", " . V.toList
  where bracketed = ("[" ++) . (++ "]")

showPlus (DTuple xs) =
  V.mapM showTupleItem xs
  >>= return . braced . intercalate ", " . V.toList
  where
    braced = ("{" ++) . (++ "}")

    showTupleItem :: (String, IORef DExpr) -> IO String
    showTupleItem (k, rv) =
      readIORef rv
      >>= showPlus
      >>= return . (tag ++)
      where tag = if null k then "" else (k ++ " := ")

showPlus other = return $ show other


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars (map (makeFunc DPrimitiveIOFunc) ioPrimitives
                                             ++ map (makeFunc DPrimitiveFunc) primitives))
     where makeFunc constructor (var, func) = (var, constructor func)
