{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGAUGE FlexibleContexts #-} 
{- LANGUAGE OverlappingInstances #-} 

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- This one is often good 
{-# LANGUAGE ScopedTypeVariables #-}

module Obsidian.Exp where

-- For Applicative instances on Monads.
import Control.Applicative

-- Shapes
import Obsidian.Shape 
-----------------------------------------------------------------
-- HACKING
-----------------------------------------------------------------
data Index sh where
  -- An index into zerodim
  ZI :: Index Z
  -- index into succdim
  SuccI :: Index tail -> Value Int -> Index (Succ tail) 

ixVal :: Index (Succ sh) -> Value Int
ixVal (SuccI _ v) = v 

-- Add up dimensionalities 
type family SAdd a b where
  SAdd a Z = a
  SAdd a (Succ b) = Succ (SAdd a b)


-----------------------------------------------------------------
--
-----------------------------------------------------------------

type Id = String 

-- | A Manifest array has a shape and a name. 
data Manifest sh a = Manifest Id (Shape sh) 


-- | A Type level tag, indicating a Push array (defunctionalised)
data Push sh a = Push -- I do not know what goes here yet.


-----------------------------------------------------------------
--
-----------------------------------------------------------------

-- | A Class of value types admissible
class EltVal a

instance EltVal Int
instance EltVal Float


-- | Handling of tuples

tup = Tup2
                          
-- | Value level expressions.
data Value a where
  Literal  :: Show a => a -> Value a
  Variable :: String -> Value a
  BinOp    :: BinOp -> Value a -> Value a -> Value a

  Tup2     :: Value a -> Value b -> Value (a,b) 

  -- | An Expression computing a scalar at level p can be
  --  converted into a Value
  --  TODO: Look for restrictions that should apply here 
  UnLift   :: EltVal a => Exp p a -> Value a
  -- Lift/unlift seems to enable arbitraty hierarchy jumps!
  --  Needs more thought! 

  -- | Manifest Arrays can be indexed into
  Index :: Exp p (Manifest sh a) -> Index sh -> Value a 

-- | Binary operations 
data BinOp = Add | Sub | Mul | Div
           deriving Show



-- | Expressions parameterised on GPU Hierarchy level that computes
--   Them. 
data Exp p a where

  -- | Lift a value
  Lift :: Value a -> Exp p a 
  
  -- | The Sublanguages for different p's are defined separately
  ParmOp :: p a -> Exp p a 
  
  -- | Run a computation in a monad called M
  RunM :: M p a -> Exp p a 

-- | The M Monad is implemented Björn & Benny style. 
--   Thus far very sketchy.
--   Not sure p parameter is useful at all. 
data M p a where

  -- | Allocate some data. 
  --   Should spit out a manifest array.
  Malloc :: Shape sh -> M p (Manifest sh a)  
  
  -- | Apply a push array (push it to memory)
  PApply :: Exp p (Push sh a) -> (Index sh -> a -> M p ()) -> M p ()

  -- | Assign into a Manifest arrays.
  --   Given a manifest array this slots directly into PApply 
  Assign :: Manifest sh a -> Index sh -> a -> M p ()

  -- | Bjorn & Benny Style 
  Bind   :: M p a -> (a -> M p b) -> M p b
  Return :: a -> M p a 

-- | Björn & Benny monad instance for M
instance Monad (M a) where
  (>>=) = Bind
  return = Return

instance Functor (M a) where
  fmap g fa = do {a <- fa; return $ g a} 

instance Applicative (M a) where
  pure = return
  ff <*> fa =
    do
      f <- ff
      fmap f fa

-----------------------------------------------------------------
-- Hierarchy Programming 
-----------------------------------------------------------------

-- | Sequential computations 
data Seq a where
  -- LiftS  :: Value a -> Seq a

  -- | Generate or sequential pull to push conversion
  Seq :: Shape sh
      -> (Index sh -> Value a)
      -> Seq (Push sh a) 
          
data Warp a where
  -- LiftW :: Value a -> Warp a 

  -- | Generate or warp-parallel pull to push conversion
  Warp :: Shape sh
       -> (Index sh -> Value a)
       -> Warp (Push sh a)

-- Don't know what operations go here yet. 
data Grid :: * -> *


data Block a where
  -- LiftB     :: Value a -> Block a
  
  -- | Generate/Conversion to push
  Block :: Shape sh
        -> (Index sh -> Value a)
        -> Block (Push sh a)
 
-----------------------------------------------------------------
-- Experimental zone
-----------------------------------------------------------------

  -- With some kind of concat operations
  DistrWarps :: Shape sh -- For each in this shape 
             -> (Index sh -> Exp Warp (Push sh' a))
             -- need to concat all sh'
             -- I think this means sh' needs to
             -- appended as an innermost dimension onto sh
             -- -> Block (Push (sh :. sh') a)
             -- this is wrong (adds it as outermost dim).
             -- It is also wrong because it requires sh' to be ()
             -- (If I am right). 

             -- Attempt
             -> Block (Push (SAdd sh sh') a) 

  Distr :: (p <= Block)
        => Shape sh
        -> (Index sh -> Exp p (Push sh' a))
        -> Block (Push (SAdd sh sh') a) 

-----------------------------------------------------------------
-- 
-----------------------------------------------------------------

  --  some defunctionalised push things go here
  --  Should be "Value a" "Value b" everywhere (I think) 
  MapB    :: (a -> b) -> Block (Push sh a) -> Block (Push sh b)
  
  IMapB   :: (Index sh -> a -> b)
          -> Block (Push sh a)
          -> Block (Push sh b)

  -- This needs work to support multidim shapes
  AppendB :: Int 
          -> Block (Push DIM1 a)
          -> Block (Push DIM1 a)
          -> Block (Push DIM1 a)
          
  InterleaveB :: Block (Push DIM1 a)
              -> Block (Push DIM1 a)
              -> Block (Push DIM1 a)

  -- Need some way of flattening nested things.
  -- Don't know if this is it though. 
  FlattenB :: Block (Push (Succ d) a)
           -> Block (Push d a) 

  -- defunced permutations
  -- along outermost dim ?
  -- This is a bit confusing 
  Reverse :: Block (Push sh a) -> Block (Push sh a)
  Rotate  :: Int -> Block (Push sh a) -> Block (Push sh a) 

  
  Permute :: (Index sh -> Index sh)
          -> (Maybe (Index sh -> Index sh)) -- programmer supplied inverse
          -> Block (Push sh a)
          -> Block (Push sh a)

  -- Is this operation even possible ?
  -- If this is OK it provides Flatten! 
  Reshape :: Shape sh' -- new shape ?
          -> (Index sh -> Index sh') -- what to do with the
                                     -- indices
          -> Block (Push sh a)
          -> Block (Push sh' a) 

  -- Maybe a programming style where most permuting, reorg etc
  -- is done on push arrays is to be preferred ? 




-----------------------------------------------------------------
-- Hierarchy constraints 
-----------------------------------------------------------------
type family LessThanOrEqual a b where
   LessThanOrEqual Seq    Seq   = True
   LessThanOrEqual Seq    Warp  = True
   LessThanOrEqual Seq    Block = True
   LessThanOrEqual Seq    Grid  = True
   
   LessThanOrEqual Warp    Warp  = True
   LessThanOrEqual Warp    Block = True
   LessThanOrEqual Warp    Grid  = True
   
   LessThanOrEqual Block   Block = True
   LessThanOrEqual Block   Grid  = True

   LessThanOrEqual Grid    Grid  = True
   
   LessThanOrEqual x y           = False
   

-- | This constraint is a more succinct way of requiring that @a@ be less than or equal to @b@.
type a <= b = (LessThanOrEqual a b ~ True)



-----------------------------------------------------------------
-- Pull arrays
-----------------------------------------------------------------

data Pull sh a = Pull (Index sh -> a) (Shape sh)





-----------------------------------------------------------------
-- Experimentation 
-----------------------------------------------------------------

-- A little bit awkward that a shape is needed when
-- the manifest array computed has an associated shape.
-- That associated shape is, however, locked away inside of the Exp Block.
-- Lengths are static,   however, so could inspect the Exp and find the length.
pull :: Shape sh -> Exp Block (Manifest sh a) -> Pull sh (Value a)
pull sh arr = Pull (\i -> Index arr i) sh


pushBlock :: Pull sh (Value a)  -> Exp Block (Push sh a)
pushBlock (Pull f sh) = ParmOp $ Block sh f 


freezePull :: Pull sh (Value a) -> Exp Block (Manifest sh a)
freezePull arr@(Pull f n) =
  RunM $ do
    manifest <- Malloc n
    PApply (pushBlock arr) (Assign manifest)
    return manifest



-- testing 
tuplePull :: Pull (Succ Z) (Value (Int, Int))
tuplePull = Pull (\i -> tup (ixVal i) (ixVal i)) (Succ Z 10) 

tupleTest :: Exp Block (Manifest (Succ Z) (Int, Int))
tupleTest = freezePull tuplePull
