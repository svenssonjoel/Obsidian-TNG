{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-} 

module Obsidian.Shape where 

infixl 3 :.
data Z
data tail :. head

type DIM0 = Z
type DIM1 = DIM0 :. ()
type DIM2 = DIM1 :. ()
type DIM3 = DIM2 :. ()

-- Swap this out for the "Succ sh" approach 

data Shape sh where
  Z    :: Shape Z
  (:.) :: Shape tail -> Int -> Shape (tail :. ())

-- | The dimensionality of @sh@
dim :: Shape sh -> Int
dim Z = 0
dim (sh :. _) = 1 + dim sh

-- | The total number of elements in @sh@
size :: Shape sh -> Int
size Z = 1
size (sh :. i) = size sh * i
