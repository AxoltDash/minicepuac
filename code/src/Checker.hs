module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- COnfiguraciones del sistema de transicion. (revisar)

tc :: ConfigT -> Type
tc (_, (ANum n)) 
              | n == 0 = Refinement Number Zero
              | otherwise = Refinement Number NonZero
tc (_, (ABool b)) = Refinement Boolean NonZero
tc (g, Id s) = lookup g s
tc (g, (Add i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number Zero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number NonZero, Refinement Number Zero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator + : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Sub i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number Zero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number NonZero, Refinement Number Zero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator - : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Mul i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number NonZero, Refinement Number NonZero) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number _) -> Refinement Number Zero
  (Refinement Number _, Refinement Number Zero) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator * : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Div i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Number NonZero, Refinement Number _) -> Refinement Number NonZero
  (Refinement Number Zero, Refinement Number _) -> Refinement Number Zero
  (Refinement Number _, Refinement Number _) -> Refinement Number MaybeZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator / : expected (Number, Number), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (And i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Boolean _, Refinement Boolean _) -> Refinement Boolean NonZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator && : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Or i d)) = case (tc (g, i), tc (g, d)) of
  (Refinement Boolean _, Refinement Boolean _) -> Refinement Boolean NonZero
  (Refinement t1 _, Refinement t2 _) -> error $ "Bad operand types for operator || : expected (Bool, Bool), got (" ++ show t1 ++ ", " ++ show t2 ++ ")"
tc (g, (Not b)) = case tc (g, b) of
  Refinement Boolean _ -> Refinement Boolean NonZero
  Refinement t _ -> error $ "Bad operand types for operator not: expected Bool, got (" ++ show t ++ ")"
tc (g, (Let (i, r) v b)) = let r' = evalRef $ getRef r
                               rv = tc (g, v) 
                               e = evalr r' rv 
                               in tc ((i, r') : g, b)
tc (g, (Lambda (Arrow dom codom) i b)) = let dom' = evalRef $ getRef dom 
                                             codom' = evalRef $ getRef codom
                                             in Arrow dom' codom'
tc (g, (App f a)) = case tc (g, f) of 
                    (Arrow dom codom) -> let t = tc (g, a) 
                                             e = evalr dom t
                                            in codom  
                    _ -> error "eesaf"
                        

getRef :: Type -> Type 
getRef Number = Refinement Number MaybeZero
getRef Boolean = Refinement Boolean NonZero
getRef r = r

evalRef :: Type -> Type
evalRef (Refinement Number p) = Refinement Number p 
evalRef (Refinement Boolean p) = Refinement Boolean p 
evalRef (Arrow dom codom) = let dom' = getRef $ evalRef dom 
                                codom' = getRef $ evalRef codom 
                            in Arrow dom' codom'
evalRef (Refinement r p) = let Refinement t p' = evalRef r in if (evalPred p' p) then (Refinement t p') else error "cepuac"

evalr :: Type -> Type -> Type
evalr (Refinement t1 p1) (Refinement t2 p2) = if ((t1 == t2) && (evalPred p1 p2)) then (Refinement t1 p1) else error "je je"
evalr (Arrow dom1 codom1) (Arrow dom2 codom2) = let dom = evalr dom1 dom2 
                                                    codom = evalr codom1 codom2 
                                                  in Arrow dom codom
evalr _ _ = error "ejejejejj"


evalPred :: Predicate -> Predicate -> Bool
evalPred NonZero MaybeZero = True
evalPred Zero MaybeZero = True
evalPred p1 p2 = p1 == p2

lookup :: Gamma -> String -> Type
lookup [] s = error "Free variable"
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s