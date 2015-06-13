{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-} 

module Obsidian.Shape where 

data Z
data Succ a 
-- data tail :. head

type DIM0 = Z
type DIM1 = Succ DIM0
type DIM2 = Succ DIM1 
type DIM3 = Succ DIM2 

-- Swap this out for the "Succ sh" approach 

data Shape sh where
  Z    :: Shape Z
  Succ :: Shape tail -> Int -> Shape (Succ tail)

-- | The dimensionality of @sh@
dim :: Shape sh -> Int
dim Z = 0
dim (Succ sh _) = 1 + dim sh

-- | The total number of elements in @sh@
size :: Shape sh -> Int
size Z = 1
size (Succ sh i) = size sh * i
