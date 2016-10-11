import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id

evalExpr env (IntLit int) = return $ Int int

evalExpr env (PrefixExpr PrefixMinus expr) = do
    e <- evalExpr env expr
    case e of
        (Int i) -> return $ Int (-i)

evalExpr env (BoolLit bool) = return $ Bool bool

evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    let v11 = case v1 of
            (Return val) -> val
            _ -> v1
        v22 = case v2 of
            (Return val) -> val
            _ -> v2
        in infixOp env op v11 v22

evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e

evalExpr env (UnaryAssignExpr unaryAssignOp (LVar var)) = do
    stateLookup env var
    case unaryAssignOp of
        (PostfixInc) -> do
            e <- evalExpr env (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1))
            setVar var e
        (PostfixDec) -> do
            e <- evalExpr env (InfixExpr OpSub (VarRef (Id var)) (IntLit 1))
            setVar var e

evalExpr env (NullLit) = return Nil

evalExpr env (CallExpr funcName values) = do
    (Function id args body) <- evalExpr env funcName
    evalStmt env $ VarDeclStmt $ varDeclList args values
    val <- evalStmt env $ BlockStmt body
    case val of
        (Return ret) -> return $ Return ret
        (Break) -> return Nil

evalExpr env (ArrayLit []) = return $ List []    
evalExpr env (ArrayLit (expr:exprs)) = do
    x  <- evalExpr env expr
    xs <- evalExpr env (ArrayLit exprs)
    case xs of
        (List []) -> return $ List [x]
        _ -> return $ List $ [x] ++ [xs]

-- evalExpr env (DotRef list func) = do    -- i.concat


evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil

evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)

evalStmt env (ExprStmt expr) = evalExpr env expr

evalStmt env (ReturnStmt maybeExpr) = do
    case maybeExpr of 
        (Just val) -> do
            ret <- evalExpr env val
            return $ Return ret

evalStmt env (BreakStmt Nothing) = return Break

evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt (stmt:stmts)) = do
    val <- evalStmt env stmt
    case val of
        (Return ret) -> return $ Return ret
        (Break) -> return Break
        _ -> evalStmt env (BlockStmt stmts)

evalStmt env (IfSingleStmt expr stmt) = do
    e <- evalExpr env expr
    case e of
        (Bool b) -> 
            if b then evalStmt env stmt
            else return Nil
        _ -> error $ "Not a valid expression."

evalStmt env (IfStmt expr stmt1 stmt2) = do
    e <- evalExpr env expr
    case e of
        (Bool b) -> 
            if b then evalStmt env stmt1
            else evalStmt env stmt2
        _ -> error $ "Not a valid expression."

evalStmt env (WhileStmt expr stmt) = do
    e <- evalExpr env expr
    case e of
        (Bool b) ->
            if b then do
                val <- evalStmt env stmt
                let ret = case val of
                        (Break) -> True
                        _ -> False
                    in do
                    if ret then return Nil
                    else evalStmt env $ WhileStmt expr stmt
            else return Nil

evalStmt env (DoWhileStmt stmt expr) = do
    val <- evalStmt env stmt
    let ret = case val of
            (Break) -> True
            _ -> False
        in do
        if ret then return Nil
        else do
            e <- evalExpr env expr
            case e of
                (Bool b) -> 
                    if b then evalStmt env $ DoWhileStmt stmt expr
                    else return Nil

evalStmt env (ForStmt forInit condition ipp stmt) = do
    evalExpr env val
    cond <- evalExpr env e1
    case cond of
        (Bool b) -> 
            if b then do
                val <- evalStmt env stmt
                let ret = case val of
                        (Break) -> True
                        _ -> False
                    in do
                    if ret then return Nil
                    else do
                        evalExpr env e2
                        evalStmt env $ ForStmt NoInit condition ipp stmt
            else
                return Nil
    where
        e1 = case condition of
            (Just val) -> val
        e2 = case ipp of
            (Just val) -> val
        val = case forInit of
            (ExprInit expr) -> expr
            (NoInit) -> NullLit

evalStmt env (FunctionStmt id args body) = 
    funcDecl env (id, args, body)


-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env OpEq (List []) (List []) = return $ Bool $ True
infixOp env OpEq (List (x:xs)) (List (y:ys)) = do
    (Bool b1) <- infixOp env OpEq x y
    (Bool b2) <- infixOp env OpEq (List xs) (List ys)
    return $ Bool $ (b1 && b2)

-- 
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

funcDecl :: StateT -> (Id,[Id],[Statement]) -> StateTransformer Value
funcDecl env ((Id id), args, body) = 
    setVar id $ Function (Id id) args body

varDeclList :: [Id] -> [Expression] -> [VarDecl]
varDeclList [] [] = []
varDeclList (arg:args) (value:values) =
    (VarDecl arg (Just value)):varDeclList args values

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
