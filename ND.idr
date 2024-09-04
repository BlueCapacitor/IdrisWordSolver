module ND

import Rational


data ProbVal : (p : Type) -> Type -> Type where
	MkProbVal : List (p, e) -> ProbVal p e

(Num p) => Functor (ProbVal p) where
	map f (MkProbVal xs) = MkProbVal (map (map f) xs)

map_prob : (p_a -> p_b) -> ProbVal p_a e -> ProbVal p_b e
map_prob f (MkProbVal xs) = MkProbVal (map (map_first f) xs) where
	map_first : (a -> a') -> (a, b) -> (a', b)
	map_first g (x, y) = (g x, y)

(Num p) => Applicative (ProbVal p) where
	pure x = MkProbVal [(1, x)]
	(MkProbVal fs) <*> (MkProbVal xs) = MkProbVal (map nd_f_to_f fs <*> xs) where
		nd_f_to_f : (p, a -> b) -> (p, a) -> (p, b)
		nd_f_to_f (p_f, f) (p_x, x) = (p_f * p_x, f x)



-- test : ProbVal Rational Nat
-- test = MkProbVal [(2 // 3, 1), (1 // 3, 2)]

-- test2 : ProbVal Rational Nat
-- test2 = map (+1) test

-- testf : ProbVal Rational (Nat -> Nat)
-- testf = MkProbVal [(1 // 2, (* 10)), (1 // 2, (* 100))]
