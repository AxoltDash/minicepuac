module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- COnfiguraciones del sistema de transicion. (revisar)

tc :: ConfigT -> Type
tc (_, (Num n)) = Number
tc (_, (Boolean b)) = Bool
tc (g, Id s) = lookup g s
tc (g, (Add i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  (t1, t2) -> error $ "Bad operand types for operator + : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Sub i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  (t1, t2) -> error $ "Bad operand types for operator - : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Mul i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  (t1, t2) -> error $ "Bad operand types for operator * : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Div i d)) = case (tc (g, i), tc (g, d)) of
  (Number, Number) -> Number
  (t1, t2) -> error $ "Bad operand types for operator / : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (And i d)) = case (tc (g, i), tc (g, d)) of
  (Bool, Bool) -> Bool
  (t1, t2) -> error $ "Bad operand types for operator && : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Or i d)) = case (tc (g, i), tc (g, d)) of
  (Bool, Bool) -> Bool
  (t1, t2) -> error $ "Bad operand types for operator || : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Not b)) = case tc (g, b) of
  Bool -> Bool
  t -> error $ "Bad operand types for operator not: expected Bool, got (" ++ show t ++ ")"
tc (g, (Let (i, t) a c))
  | t == tc (g, a) = tc ((i, t):g, c)
  | otherwise = error $ "Incompatible types in variable " ++ "\"" ++ i ++"\"" ++ ": expected " ++ show t ++ ", got " ++ show (tc (g, a))
tc (g, (Lambda (Arrow dom codom) i b))
  | codom == tc ((i, dom):g, b) = (Arrow dom codom)
  | otherwise = error $ "Incompatible types in function return: expected " ++ show codom ++ ", got " ++ show (tc ((i, dom):g, b))
tc (g, (App f a)) = 
  let tf = tc (g, f)
      ta = tc (g, a)
  in case tf of
    Arrow dom codom -> 
      if ta == dom 
        then codom 
        else error $ "Type mismatch: expected argument of type " ++ show dom ++ ", got " ++ show ta
    t -> error $ "Cannot apply to: " ++ show t
tc (g, (Let (i, t) a c))
  | t == tc (g, a) = tc ((i, t):g, c)
  | otherwise = error $ "Incompatible types in variable " ++ "\"" ++ i ++"\"" ++ ": expected " ++ show t ++ ", got " ++ show (tc (g, a))
lookup :: Gamma -> String -> Type
lookup [] s = error "Free variable"
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s
