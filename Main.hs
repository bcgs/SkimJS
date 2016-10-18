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
        _ -> return $ Error $ "Not Integer"

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

evalExpr env (AssignExpr OpAssign (LBracket (VarRef (Id id)) expr2) expr3) = do
    List list <- evalExpr env (VarRef (Id id))
    Int index <- evalExpr env expr2
    newValue <- evalExpr env expr3
    setVar id $ List $ setValue index list newValue

evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    val <- stateLookup env var -- crashes if the variable doesn't exist
    case val of
        (Error _) -> return $ Error $ "Variable " ++ show var ++ " not defined!"
        _ -> do
            e <- evalExpr env expr
            setVar var e

evalExpr env (UnaryAssignExpr unaryAssignOp (LVar var)) = do
    val <- stateLookup env var
    case val of
        (Error _) -> return $ Error $ "Variable " ++ show var ++ " not defined!"
        _ -> do
            case unaryAssignOp of
                (PostfixInc) -> do
                    e <- evalExpr env (InfixExpr OpAdd (VarRef (Id var)) (IntLit 1))
                    setVar var e
                (PostfixDec) -> do
                    e <- evalExpr env (InfixExpr OpSub (VarRef (Id var)) (IntLit 1))
                    setVar var e

evalExpr env (NullLit) = return Nil

evalExpr env (CallExpr funcName values) = do
    case funcName of
        (DotRef list1 (Id func)) -> do
            List l1 <- evalExpr env list1
            case func of
                ("head") -> do
                    case l1 of
                        (x:xs) -> return x
                        [] -> return $ Error $ "OutOfBoundsException."
                ("tail") -> do
                    case l1 of
                        (x:xs) -> return $ List xs
                        [] -> return $ List []
                ("concat") -> do
                    List l2 <- evalExpr env (head values)
                    return $ List $ l1 ++ l2
        _ -> do
            (Function id args body) <- evalExpr env funcName
            evalStmt env $ VarDeclStmt $ varDeclList args values
            val <- evalStmt env $ BlockStmt body
            case val of
                (Return ret) -> return $ Return ret
                (Break) -> return Nil
                (Int i) -> return $ Int i
                (Error str) -> return $ Error str
                (Bool b) -> return $ Bool b

evalExpr env (ArrayLit []) = return $ List []    
evalExpr env (ArrayLit (expr:exprs)) = do
    x  <- evalExpr env expr
    (List xs) <- evalExpr env (ArrayLit exprs)
    case xs of
        [] -> return $ List (x:[])
        _ -> return $ List $ (x:xs)

evalExpr env (BracketRef list_ index_) = do
    Int index <- evalExpr env index_
    List list <- evalExpr env list_
    return $ getValue index list


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
        (Nothing) -> return $ Return Nil

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
    case forInit of
        (VarInit decls) -> evalStmt env (VarDeclStmt decls)
        _ -> evalExpr env val
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

infixOp env OpLAnd (List []) (List []) = return $ Bool $ True
infixOp env OpLAnd (List (x:xs)) (List (y:ys)) = do
    (Bool b1) <- infixOp env OpLAnd x y
    (Bool b2) <- infixOp env OpLAnd (List xs) (List ys)
    return $ Bool $ (b1 && b2)
infixOp env OpLAnd _ _ = return $ Bool $ False

infixOp env OpEq (List []) (List []) = return $ Bool $ True
infixOp env OpEq (List (x:xs)) (List (y:ys)) = do
    (Bool b1) <- infixOp env OpEq x y
    (Bool b2) <- infixOp env OpEq (List xs) (List ys)
    return $ Bool $ (b1 && b2)
infixOp env OpEq _ _ = return $ Bool $ False

infixOp env OpNEq (List []) (List []) = return $ Bool $ False
infixOp env OpNEq (List (x:xs)) (List (y:ys)) = do
    (Bool b1) <- infixOp env OpNEq x y
    (Bool b2) <- infixOp env OpNEq (List xs) (List ys)
    return $ Bool $ (b1 && b2)
infixOp env OpNEq _ _ = return $ Bool $ True

-- 
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> (Error $ "Variable " ++ show var ++ " not defined.",s)
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
varDeclList _ _ = error $ "List of params doesn't match!"

getValue :: Int -> [Value] -> Value
getValue index [] = error $ "Index out of bounds exception!"
getValue 0 (x:xs) = x
getValue index (x:xs) = getValue (index-1) xs 

setValue :: Int -> [Value] -> Value -> [Value]
setValue 0 [] newValue = [newValue]
setValue 0 (x:xs) newValue = (newValue:xs)
setValue i (x:xs) newValue = x:(setValue (i-1) xs newValue)
setValue i [] newValue = 
    error $ "Index out of bounds exception! (index: " ++ show i ++")"

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
