{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-
EXAMEN
"Abstracciones en la Práctica de la Programación Funcional"
RIO 2020
por Mauro Jaskelioff

Resolver los ejercicios en este archivo. y
enviar la solución por correo electrónico a:

mauro @ fceia.unr.edu.ar

antes del 7/3/2020.
-}

module Examen where

import Prelude hiding ((<>),mempty,Monoid)

import Monoid
import Functors
import Generics
import Applicative
import Alternative
import Control.Applicative

-------------------------------------
-- Monoides
-------------------------------------

{-  #1
 Definir una instancia válida de Monoid para el siguiente tipo de datos tal que
 su operación binaria calcule el mínimo entre sus argumentos.
-}

data Min a = Infinito | N a
       deriving Show


instance (Ord a) => Monoid (Min a) where
       mempty = Infinito
       (N a) <> Infinito = N a
       Infinito <> (N b) = N b
       (N a) <> (N b) = if a < b then N a else N b

{- #2
 Definir un tipo de datos Max y definir una instancia de Monoid para el mismo
 tal que su operación binaria calcule el máximo entres sus argumentos.

-}

data Max a = MenosInfinito | N a
       deriving Show

instance (Ord a) => Monoid (Max a) where
       mempty = MenosInfinito
       (N a) <> MenosInfinito = N a
       MenosInfinito <> (N b) = N b
       (N a) <> (N b) = if a > b then N a else N b

-------------------------------------
-- Functores
-------------------------------------

{- #3
 De ser posible, definir una instancia de Functor válida
 para el siguiente tipo de datos
-}

data CSList a = Vacia | Singleton a | ConsSnoc a (CSList a) a
       deriving Show

instance Functor CSList where
       fmap f Vacia = Vacia
       fmap f (Singleton a) = Singleton (f a)
       fmap f (ConsSnoc a xs b) = ConsSnoce (f a) (fmap f xs) (f b)

-----------------------------------------
-- Generics
-----------------------------------------

{- #4
 Dar una instancia de Generic para CSList
-}
data CSListG = K () 
           :+: Id 
           :+: Id :*: Rec CSListG :*: Id

toG :: CSList -> CSListG
toG Vacia = Inl (K ())
toG (Singleton a) = Inr $ Inl $ Id a
toG (ConsSnoc a xs b) = Inr $ Inr $ FunProd (Id a) $ FunProd (Rec xs) (Id b)

fromG :: CSListG -> CSList
fromG Inl (K ()) = Vacia
fromG (Inr (Inr (Id a))) = Singleton a
fromG (Inr (Inr (FunProd (Id a) (FunProd (Rec xs) (Id b))))) = ConsSnoc a xs b

instance Generic CSList where
           type Rep CSList = CSListG
           fromRep = fromG
           toRep = toG

{- #5
Usando crush (del módulo Generics) definir una función que calcule en
forma genérica el máximo y el mínimo almacenados en una estructura
(ver ejercicios #1 y #2).

Ejemplos:
> minmax [2,6,3]
(2,6)

> minmax (Node (Leaf 5) (Node (Leaf 3) (Leaf 4)))
(3,5)

> minmax (ConsSnoc 4 (Singleton 9) 7)
(4,9)

(Para que funcionen los ejemplos de más arriba es necesario
 que estén definidas instancias de Crush para [], Bin, y CSList)

 Por ejemplo:
-}
data MinMax a = FunProd (Min a) (Max a)
 
instance Crush [] where
  gcrush = gcrush . toRep

{-
No se especifica que hace minmax para estructuras vacías:
> minmax []
LAUNCHING MISSILE! EVACUATE EARTH.
-}

minmax :: (Functor f, Generic f, Crush (Rep f), Ord a) => f a -> (a,a)
minmax = undefined

-- (Puntos extra si minmax calcula el resultado en una sola pasada)

-----------------------------------------
-- Applicative / Alternative
-----------------------------------------

{- #6
  Dado un aplicativo f y un aplicativo g, es
  (f :+: g) un aplicativo?
   Dar la instancia o argumentar por qué no lo es.
-}

{- #7
Chequear que una cadena de paréntesis está bien balanceada con un parser

parentesis :: Parser ()

Por ejemplo
> parse parentesis  "(()())()"
Just ((),"")

> parse parentesis  "(()()()"
Nothing
-}

{- #8
Evaluador de expresiones aritméticas

Dada la gramática BNF de expresiones aritméticas

  expr ::= term (’+’ expr | ε )
  term ::= factor (’*’ term | ε )
  factor ::= nat | ’(’ expr ’)’

(donde ε denota la cadena vacía y nat es un número natural)

Dar una función

  evalExpr :: String -> Maybe Int

que parsee expresiones y las evalúe.

Por ejemplo:

  > evalExpr "2*3+4*(1+4)"
Just 26

Recomendamos definir un parser para cada no terminal.

-- Puntos extra: Extender el parser para manejar resta y división.
-}
