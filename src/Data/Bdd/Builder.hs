{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , newVariable
    -- * BDD Builders
  , fromBdd
  , true, false
  , proj
  , not
  , and
  , or
  , implies
  , ite
    -- * Combinators
  , trueB, falseB
  , notB
  , andB
  , orB
  , impliesB
  ) where

import Prelude hiding ( and, or, not )

import Control.Applicative
import Control.Lens
import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Map                   ( Map )
import qualified Data.Map as Map

import Data.Bdd
import Data.Bdd.Internal

type UniqueTable = Map (Variable, Bdd, Bdd) Bdd

data BuilderState = BuilderState
  { _uniqueTable :: !UniqueTable
  , _lastNodeId  :: !NodeId
  , _varCount    :: !Int
  }

makeLenses ''BuilderState

initialState :: BuilderState
initialState = BuilderState
  { _uniqueTable = Map.empty
  , _lastNodeId  = 1 -- 0 and 1 reserved for terminal nodes
  , _varCount    = 0
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
    ns <- use $ uniqueTable.to Map.elems
    return . Sobdd $ false':true':ns

-- | Add a 'Bdd' node to the internal SOBDD. If an isomorphic node already
-- exists it is returned, instead of creating a new (redundant) node.
addNode :: Monad m => Variable -> Bdd -> Bdd -> BuilderT s m Bdd
addNode var t e = B $ do
    let info = (var, t, e)
    use (uniqueTable.at info) >>= \case
        Just n  -> return n
        Nothing -> do
            nid <- freshNodeId
            let n = BddNode nid var t e
            uniqueTable %= Map.insert info n
            return n
  where
    freshNodeId = do
        lastNodeId += 1
        use lastNodeId

addVariable :: Monad m => Variable -> BuilderT s m ()
addVariable (Variable v) = B $ do
    count <- use varCount
    when (v >= count) $ varCount .= v + 1

-- | Create a fresh 'Variable'. The new variable is the maximal element in
-- the variable order.
newVariable :: Monad m => BuilderT s m Variable
newVariable = B $ do
    v <- use varCount
    varCount += 1
    return (Variable v)

-- | Import a 'Bdd' into a 'Builder'.
fromBdd :: Monad m => Bdd -> BuilderT s m (Ref s Bdd)
fromBdd = liftM Ref . go where
    go node = case viewNode node of
        Terminal True    -> return true'
        Terminal False   -> return false'
        Decision var t e -> do
            addVariable var
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
proj var = do
    addVariable var
    liftM Ref (addNode var true' false')

not :: Monad m => Ref s Bdd -> BuilderT s m (Ref s Bdd)
not x = ite x false true

and :: Monad m => Ref s Bdd -> Ref s Bdd -> BuilderT s m (Ref s Bdd)
x `and` y = ite x y false

or :: Monad m => Ref s Bdd -> Ref s Bdd -> BuilderT s m (Ref s Bdd)
x `or` y = ite x true y

implies :: Monad m => Ref s Bdd -> Ref s Bdd -> BuilderT s m (Ref s Bdd)
x `implies` y = ite x y true

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


ap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
ap2 f mx my = do
    x <- mx; y <- my
    f x y

trueB :: Monad m => BuilderT s m (Ref s Bdd)
trueB = return true

falseB :: Monad m => BuilderT s m (Ref s Bdd)
falseB = return false

notB :: Monad m => BuilderT s m (Ref s Bdd) -> BuilderT s m (Ref s Bdd)
notB = (>>= not)

andB
    :: Monad m
    => BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
andB = ap2 and

orB
    :: Monad m
    => BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
orB = ap2 or

impliesB
    :: Monad m
    => BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
    -> BuilderT s m (Ref s Bdd)
impliesB = ap2 implies

