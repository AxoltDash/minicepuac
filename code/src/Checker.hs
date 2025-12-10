{-# LANGUAGE BangPatterns #-}
module Checker where
import Grammar hiding (lookup)
import Prelude hiding (lookup)
import Lexer
import Data.Maybe (Maybe)

type Gamma = [(String, Type)] -- Contexto de tipificado.

type ConfigT = (Gamma, ASA) -- Configuraciones del sistema de tipos 

tc :: ConfigT -> Type
-- Tipos primitivos encapsulados en refinamientos 
tc (_, (ANum n)) 
              | n == 0 = Refinement Number Zero
              | otherwise = Refinement Number NonZero
tc (_, (ABool b)) = Refinement Boolean NonZero
tc (g, Id s) = lookup g s
-- El type checker resuelve el tipo refinado de la expresion
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
  (Refinement Number _, Refinement Number Zero) -> error $ "This expression contain a division by zero."
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
-- Se fuerzan las llamadas a todas las funciones usadas
-- aunque no se usen sus valores de retorno, permite detectar errores de tipos

-- Pasa a r la etiqueta de tipo, a su tipo mas reducido r'(usa solve).
-- Verifica que el valor v sea consistente con el tipo r' (usa evalr).
-- Regresa el tipo del cuerpo, con el contexto de tipos extendido.
tc (g, (Let (i, r) v b)) = let !r' = solve r
                               !rv = tc (g, v) 
                               !e = evalr rv r'
                                in tc ((i, rv) : g, b)
-- Pasa las etiquetas de tipos refinados dom y codom, a sus tipos mas reducidos dom' y codom' (usa solve).
-- Se hace explicita la resolucion de dom y codom
tc (g, (Lambda (Arrow dom codom) i b)) = let !dom' = solve dom 
                                             !codom' = solve codom
                                              in Arrow dom' codom'
-- Obtiene el tipo de f que se pide que sea funcion, al usar tc se tiene que ya es el mas parituclar.
-- Obtiene el tipo particular del parametro real a,
-- Verifica que el tipo del parametro real a sea consistente con el dominio de la respectiva funcion.                                           
tc (g, (App f a)) = case tc (g, f) of 
                    (Arrow dom codom) -> let !t = tc (g, a) 
                                             !e = evalr t dom
                                              in codom  
                    _ -> error "Cannot apply to a non-function"

-- Permite resolver refinamientos anidados, el resultado despues de pasar por esta funcion
-- es un tipo sin refinamientos anidados
solve :: Type -> Type
solve (Refinement Number p) = Refinement Number p 
solve (Refinement Boolean p) = Refinement Boolean p 
solve (Arrow dom codom) = let !dom' = solve dom 
                              !codom' = solve codom 
                            in Arrow dom' codom'
solve (Refinement r p) = let Refinement t p' = solve r in 
     if (evalPred p' p) 
     then (Refinement t p') 
     else error $ "Predicate mismatch: expected " ++ show p ++ ", got " ++ show p' ++ " in type " ++ show t

-- Permite saber si un tipo refinado es de otro tipo
-- es decir tienen el mismo tipo primitivo y predicados compatibles
-- conceptualmente evalr a b es:
-- si a es del tipo b entonces regresa el tipo refinado
-- con el mismo tipo primitivo de ambos y el predicado mas particular
-- por lo reducido de los refinamientos es Zero o NonZero
evalr :: Type -> Type -> Type
evalr (Refinement t1 p1) (Refinement t2 p2) = 
  if ((t1 == t2) && (evalPred p1 p2)) 
  then (Refinement t1 p1) 
  else error $ "Type or predicate mismatch: expected type " ++ show t2 ++ " with predicate " ++ show p2 ++ ", got type " ++ show t1 ++ " with predicate " ++ show p1
evalr (Arrow dom1 codom1) (Arrow dom2 codom2) = let !dom = evalr dom1 dom2 
                                                    !codom = evalr codom1 codom2 
                                                  in Arrow dom codom
-- Las funciones no son un tipo refinado y viceversa
evalr (Refinement _ _) (Arrow dom codom) = error $ "Type mismatch: cannot match refinement type with function type (Arrow " ++ show dom ++ " " ++ show codom ++ ")"
evalr (Arrow dom codom) (Refinement _ _) = error $ "Type mismatch: cannot match function type (Arrow " ++ show dom ++ " " ++ show codom ++ ") with refinement type"

-- Permite saber si dos predicados son consistentes, para evalPred a b:
-- si a cumple tambien b es consistente y se regresa True
-- si a no cumple b no se es consistente y se regresa False
evalPred :: Predicate -> Predicate -> Bool
evalPred NonZero MaybeZero = True
evalPred Zero MaybeZero = True
evalPred p1 p2 = p1 == p2

-- Busca el tipo de una variable
lookup :: Gamma -> String -> Type
lookup [] s = error ("Variable " ++ s ++ " not in scope")
lookup ((id, t):xs) s 
  | id == s = t 
  | otherwise = lookup xs s
