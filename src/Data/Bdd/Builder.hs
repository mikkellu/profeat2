{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RankNTypes      #-}

-- | Builders for constructing 'Bdd's.
--
-- Internally, a Builder uses a Shared Ordered Binary Decision Diagram to
-- ensure all 'Bdd's are reduced and nodes are shared when possible.
module Data.Bdd.Builder
  ( -- * Builder Type
    BuilderT, Builder
  , runBuilderT, runBuilder
  , Ref
  , readRef
  , Sobdd
  , getSobdd
    -- * BDD Builders
  , Connective
  , fromBdd
  , true, false
  , proj
  , not
  , and
  , nand
  , or
  , nor
  , implies
  , xor
  , xnor
  , ite
    -- * Combinators
  , ConnectiveBuilder
  , trueB, falseB
  , notB
  , andB
  , nandB
  , orB
  , norB
  , impliesB
  , xorB
  , xnorB
  , iteB
  ) where

import Prelude hiding ( and, or, not )

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.HashMap.Strict                  ( HashMap )
import qualified Data.HashMap.Strict as Map

import Data.Bdd
import Data.Bdd.Internal

type UniqueTable = HashMap (Variable, Bdd, Bdd) Bdd

data BuilderState = BuilderState
  { uniqueTable :: !UniqueTable
  , nextNodeId  :: !NodeId
  }

initialState :: BuilderState
initialState = BuilderState
  { uniqueTable = Map.empty
  , nextNodeId  = 2 -- 0 and 1 reserved for terminal nodes
  }

-- | A @BuilderT@ is used to construct 'Bdd's.
newtype BuilderT s m a = B { unB :: StateT BuilderState m a } deriving (Functor)

instance (Functor m, Monad m) => Applicative (BuilderT s m) where
    pure    = B . pure
    f <*> x = B $ unB f <*> unB x

instance Monad m => Monad (BuilderT s m) where
    return = B . return
    m >>= f = B $ unB m >>= (unB . f)

instance MonadTrans (BuilderT s) where
    lift = B . lift

-- | 'BuilderT' over 'Identity'.
type Builder s a = BuilderT s Identity a

-- | Run a 'BuilderT' computation.
runBuilderT :: Monad m => (forall s. BuilderT s m a) -> m a
runBuilderT m = evalStateT (unB m) initialState

-- | Run a 'Builder' computation.
runBuilder :: (forall s. Builder s a) -> a
runBuilder m = runIdentity (runBuilderT m)

-- | A reference to some value of type @a@. The reference is indexed by @s@
-- representing the state thread of a 'Builder s'. This type parameter
-- ensures that references cannot escape from a 'Builder' context.
newtype Ref s a = Ref a

-- | Get the referenced value.
readRef :: Ref s a -> a
readRef (Ref x) = x

-- | Returns the internal SOBDD.
getSobdd :: Monad m => BuilderT s m Sobdd
getSobdd = B $ do
    ns <- gets (Map.elems . uniqueTable)
    return . Sobdd $ false':true':ns

-- | Add a 'Bdd' node to the internal SOBDD. If an isomorphic node already
-- exists it is returned, instead of creating a new (redundant) node.
addNode :: Monad m => Variable -> Bdd -> Bdd -> BuilderT s m Bdd
addNode var t e = B $ do
    let info = (var, t, e)
    ut <- gets uniqueTable

    case Map.lookup info ut of
        Just n  -> return n
        Nothing -> do
            nid <- freshNodeId
            let n = BddNode nid var t e
            modify $ \s -> s { uniqueTable = Map.insert info n ut }
            return n
  where
    freshNodeId = do
        nid <- gets nextNodeId
        modify $ \s -> s { nextNodeId = nid + 1 }
        return nid


type Connective s m = Monad m => Ref s Bdd -> Ref s Bdd -> BuilderT s m (Ref s Bdd)

-- | Import a 'Bdd' into a 'Builder'.
fromBdd :: Monad m => Bdd -> BuilderT s m (Ref s Bdd)
fromBdd = liftM Ref . go where
    go node = case viewNode node of
        Terminal True    -> return true'
        Terminal False   -> return false'
        Decision var t e -> do
            t' <- go t
            e' <- go e
            addNode var t' e'

-- | The terminal node labeled with 'True'.
true :: Ref s Bdd
true = Ref true'

true' :: Bdd
true' = BddTerm True

-- | The terminal node labeled with 'False'.
false :: Ref s Bdd
false = Ref (BddTerm False)

false' :: Bdd
false' = BddTerm False

-- | Projection function of a 'Variable'.
proj :: Monad m => Variable -> BuilderT s m (Ref s Bdd)
proj var = liftM Ref (addNode var true' false')

not :: Monad m => Ref s Bdd -> BuilderT s m (Ref s Bdd)
not x = ite x false true

and, nand, or, nor, implies, xor, xnor :: Connective s m
x `and`     y =                  ite x y       false
x `nand`    y = not y >>= \ny -> ite x ny      true
x `or`      y =                  ite x true    y
x `nor`     y = not x >>= \nx -> ite y false   nx
x `implies` y =                  ite x y       true
x `xor`     y = not y >>= \ny -> ite x ny      y
x `xnor`    y = not y >>= \ny -> ite x y       ny

-- | @ite cond t e@ creates a 'Bdd' representing @if cond then t else e@.
ite :: Monad m
    => Ref s Bdd -> Ref s Bdd -> Ref s Bdd -> BuilderT s m (Ref s Bdd)
ite (Ref c) (Ref t) (Ref e) = liftM Ref (ite' c t e)

ite' :: Monad m => Bdd -> Bdd -> Bdd -> BuilderT s m Bdd
ite' c t e = case c of
    BddTerm True       -> return t
    BddTerm False      -> return e
    BddNode _ cVar _ _ -> do
        let var = min (min cVar (variable t)) (variable e)

        t' <- ite' (child c var True)  (child t var True)  (child e var True)
        e' <- ite' (child c var False) (child t var False) (child e var False)

        if t' == e'
            then return t'
            else addNode var t' e'
  where
    child n var b = case n of
        BddTerm _         -> n
        BddNode _ nodeVar t' e'
          | var < nodeVar -> n
          | otherwise     -> if b then t' else e'


type ConnectiveBuilder s m
    =  Monad m
    => BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)

bindAp2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindAp2 f mx my = do
    x <- mx; y <- my
    f x y

trueB :: Monad m => BuilderT s m (Ref s Bdd)
trueB = return true

falseB :: Monad m => BuilderT s m (Ref s Bdd)
falseB = return false

notB :: Monad m => BuilderT s m (Ref s Bdd) -> BuilderT s m (Ref s Bdd)
notB = (>>= not)

andB, nandB, orB, norB, impliesB, xorB, xnorB :: ConnectiveBuilder s m
andB     = bindAp2 and
nandB    = bindAp2 nand
orB      = bindAp2 or
norB     = bindAp2 nor
impliesB = bindAp2 implies
xorB     = bindAp2 xor
xnorB    = bindAp2 xnor

iteB :: Monad m
     => BuilderT s m (Ref s Bdd)
     -> BuilderT s m (Ref s Bdd)
     -> BuilderT s m (Ref s Bdd)
     -> BuilderT s m (Ref s Bdd)
iteB c t e = do
    c' <- c; t' <- t; e' <- e
    ite c' t' e'

