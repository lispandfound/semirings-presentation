import Control.Applicative
primes :: [Integer]
primes = sieve [2..]
 where sieve (p:ns) =
         p : sieve (filter (not . (`divisible` p)) ns)
       m `divisible` n = m `mod` n == 0

class Semiring r where
  one :: r
  zero :: r
  closure :: r -> r
  (+.) :: r -> r -> r
  infixl 6 +.
  (*.) :: r -> r -> r
  infixl 7 *.


data ExReal = Infinity | Real Double deriving (Show, Eq)

instance Semiring ExReal where
  zero = Infinity
  one = Real 0
  closure x = Real 0
  (+.) (Real x) (Real y) = Real (min x y)
  (+.) (Real x) Infinity = Real x
  (+.) Infinity Infinity = Infinity
  (+.) x y = y +. x

  (*.) (Real x) (Real y) = Real (x + y)
  (*.) _ Infinity = Infinity
  (*.) x y = y *. x


instance Semiring (Maybe [Double]) where
  zero = Nothing
  one = Just []
  closure = const one

  (*.) = liftA2 (++)
  
  (+.) Nothing x = x
  (+.) x Nothing = x
  (+.) x y = minLength <$> x <*> y
    where minLength x y = if sum x < sum y then x else y

  
  
  
