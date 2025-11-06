module Practica03 where

-- Tipo de dato Prop
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]

-- Ejercicio 1
variables :: Prop -> [String]
variables prop = varsAux prop []
  where
    varsAux :: Prop -> [String] -> [String]
    varsAux (Var x) collector
        | include x collector = colletor
        | otherwise = collecor ++ [x]
    varsAux (Cons _) collector = collector
    varsAux (Not f) collector  = varsAux f collector
    varsAux (And f g) collector = varsAux g (varsAux f collector )
    varsAux (Or  f g) collector = varsAux g (varsAux f collector)
    varsAux (Impl f g) collector = varsAux g (varsAux f collector)
    varsAux (Syss f g) collector = varsAux g (varsAux f collector) 

-- Ejercicio 2
interpretacion :: Prop -> Estado -> Bool
interpretacion (const a) _ = a
interpretacion (var x) list = include x list
interpretacion (Not prop) list = not (interpretacion prop list)
interpretacion (And x y) list = (intrepretacion y list) && (interpretacion x list)
interpretacion (Or x y) list = (interpretacion x list) || (interpretacion y list) 
interpretacion (Impl x y) list = (interpretacion (Not x) list) || (interpretacion y list)
interpretacion (Syss x y) list = (interpretacion x list) == (interpretacion y list)


-- Ejercicio 3
estadosPosibles :: Prop -> [Estado]
estadosPosibles prop = conjuntoPotencia (variables prop)

-- Ejercicio 4
modelos :: Prop -> [Estado]
modelos prop = modelosRecur prop (estadosPosibles prop) []
    where
        modelosRecur :: Prop -> [Estado] -> [Estado] -> [Estado]
        modelosRecur _ [] _ = collector
        modelosRecur prop (x:xs) collector 
            | interpretacion prop x = modelosRecur xs prop (collector ++ [x])
            | otherwise             = modelosRecur xs prop collector

-- Ejercicio 5
sonEquivalentes :: Prop -> Prop -> Bool
sonEquivalentes (Cons True) (Cons True) = True
sonEquivalentes (Cons True) q = sonEquivalentesRecu (Cons True) q (estadosPosibles q)
sonEquivalentes p (Cons True) = sonEquivalentesRecu p (Cons True) (estadosPosibles p)
sonEquivalentes p q = (sonEquivalentesRecu p q (estadosPosibles p)) && (sonEquivalentesRecu p q (estadosPosibles q))

sonEquivalentesRecu :: Prop -> Prop -> [Estado] -> Bool
sonEquivalentesRecu _ _ [] = True
sonEquivalentesRecu p q (x:xs) 
    | interpretacion (Syss p q) x = sonEquivalentesRecu p q xs
    | otherwise                   = False

-- sameVars :: [String] -> [String] -> Bool
-- sameVars [] list = True
-- sameVars list [] = False
-- sameVars (x:xs) list    
--     | include x list = sameVars xs (remove x list) 
--     | otherwise      = False 

-- remove :: (Eq a) => a -> [a] -> [a]
-- remove _ [] = []
-- remove a (x:xs) 
--     | a == x    = xs
--     | otherwise = x : (remove a xs)

-- Ejercicio 6
tautologia :: Prop -> Bool
tautologia p = sonEquivalentes p (Cons True)

-- Ejercicio 7
consecuenciaLogica :: [Prop] -> Prop -> Bool
consecuenciaLogica = undefined

--Funcion auxiliar
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia (x:xs) = [(x:ys) | ys <- conjuntoPotencia xs] ++ conjuntoPotencia xs

include :: (Eq a) => a -> [a] -> Bool
include _ [] = False
include elem (head : sublist)
    | elem == head = True
    | elem != head = include elem ys
