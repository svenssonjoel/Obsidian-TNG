{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{- LANGUAGE FlexibleInstances #-}
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
--
-----------------------------------------------------------------
data Index sh where
  ZI :: Index Z
  (::.) :: Index tail -> Value Int -> Index (tail :. ()) 



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
instance (EltVal a, EltVal b) => EltVal (a,b) 


-- | Value level expressions.
data Value a where
  Literal  :: Show a => a -> Value a
  Variable :: String -> Value a
  BinOp    :: BinOp -> Value a -> Value a -> Value a

  -- | A Expression computing a scalar at level p can be
  --  converted into a Value
  --  TODO: Look for restrictions that should apply here 
  UnLift   :: EltVal a => Exp p a -> Value a

  -- | Manifest Arrays can be indexed into
  Index :: Exp p (Manifest sh a) -> Index sh -> Value a 

-- | Binary operations 
data BinOp = Add | Sub | Mul | Div
           deriving Show



-- | Expressions parameterised on GPU Hierarchy level that computes
--   Them. 
data Exp p a where

  -- | The Sublanguages for different p's are defined separately
  ParmOp :: p a -> Exp p a 
  
  -- | Run a computation in a monad called M
  RunM :: M p a -> Exp p a 

-- | The M Monad is implemented Björn & Benny style. 
--   Thus far very sketchy. 
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
  LiftS  :: Value a -> Seq a

  -- | Generate or sequential pull to push conversion
  Seq :: (p <= Seq)
      => Shape sh
      -> (Index sh -> Exp p a)
      -> Seq (Push sh a) 
          
data Warp a where
  LiftW :: Value a -> Warp a 

  -- | Generate or warp-parallel pull to push conversion
  Warp :: (p <= Warp)
       => Shape sh
       -> (Index sh -> Exp p a)
       -> Warp (Push sh a)

-- Don't know what operations go here yet. 
data Grid :: * -> *


data Block a where
  LiftB     :: Value a -> Block a
  
  -- | Generate/Convertion to push 
  Block :: (p <= Block)
        => Shape sh
        -> (Index sh -> Exp p a)
        -> Block (Push sh a)

  --  some defunctionalised push things go here
  MapB    :: (a -> b) -> Block (Push sh a) -> Block (Push sh b)
  
  IMapB   :: (Index sh -> a -> b)
          -> Block (Push sh a)
          -> Block (Push sh b)

  -- This needs work to support shapes
  AppendB :: Int 
          -> Block (Push (Z:.()) a)
          -> Block (Push (Z:.()) a)
          -> Block (Push (Z:.()) a)
          
  Interleave :: Block (Push (Z:.()) a)
             -> Block (Push (Z:.()) a)
             -> Block (Push (Z:.()) a)

  -- defunced permutations
  -- along outermost dim ? 
  Reverse :: Block (Push sh a) -> Block (Push sh a)
  Rotate  :: Int -> Block (Push sh a) -> Block (Push sh a) 





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


pull :: Shape sh -> Exp Block (Manifest sh a) -> Pull sh (Value a)
pull sh arr = Pull (\i -> Index arr i) sh


pushBlock :: Pull sh (Value a)  -> Exp Block (Push sh a)
pushBlock (Pull f sh) = ParmOp $ Block sh ( ParmOp . LiftB . f ) 


freezePull :: Pull sh (Value a) -> Exp Block (Manifest sh a)
freezePull arr@(Pull f n) =
  RunM $ do
    manifest <- Malloc n
    PApply (pushBlock arr) (Assign manifest)
    return manifest


