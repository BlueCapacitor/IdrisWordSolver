module Rational

public export
data Rational = Quotient Integer Integer

public export
infixl 9 //
(//) = Quotient

public export
getDividend : Rational -> Integer
getDividend (Quotient dividend _) = dividend

public export
getDivisor : Rational -> Integer
getDivisor (Quotient _ divisor) = divisor

public export
gcd : Integer -> Integer -> Integer
gcd 0 = id
gcd x = flip gcd x . (`mod` x)

public export
lcm : Integer -> Integer -> Integer
lcm x y = (x * y) `div` gcd x y

public export
simplify : Rational -> Rational
simplify (Quotient dividend divisor) =
	Quotient (div dividend factor) (div divisor factor) where
		factor : Integer
		factor = abs (gcd dividend divisor) * div divisor (abs divisor)

public export
Num Rational where
	(Quotient dividendA divisorA) + (Quotient dividendB divisorB) =
		Quotient (dividendA * div divisorB commonDivisor + dividendB * div divisorA commonDivisor) $
			div (divisorA * divisorB) commonDivisor
	where
		commonDivisor : Integer
		commonDivisor = gcd divisorA divisorB

	(Quotient dividendA divisorA) * (Quotient dividendB divisorB) =
		Quotient (dividendA * dividendB) (divisorA * divisorB)

	fromInteger x = Quotient x 1

public export
Neg Rational where
	negate (Quotient dividend divisor) = Quotient (negate dividend) divisor
	(-) = flip $ (+) . negate

public export
Fractional Rational where
	(Quotient dividendA divisorA) / (Quotient dividendB divisorB) =
		Quotient (dividendA * divisorB) (divisorA * dividendB)
	recip (Quotient dividend divisor) = Quotient divisor dividend

public export
Show Rational where
	show (Quotient dividend divisor) = show dividend ++ " // " ++ show divisor
