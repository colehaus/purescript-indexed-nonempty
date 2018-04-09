module Data.NonEmpty.Indexed where

import Prelude

import Data.Eq (class Eq1, eq1)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex
  (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe)
import Data.NonEmpty as UnIndexed
import Data.Ord (class Ord1, compare1)
import Data.Semigroup.Foldable (class Foldable1, fold1Default)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.TraversableWithIndex
  (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..), uncurry)


data NonEmpty f k v = NonEmpty (Tuple k v) (f k v)

infixr 5 NonEmpty as :|

foldl1 ::
     forall f k v. Foldable (f k)
  => (v -> v -> v)
  -> NonEmpty f k v
  -> v
foldl1 f ((Tuple _ v) :| fv) = foldl f v fv

fromNonEmpty :: forall f k v r. (k -> v -> f k v -> r) -> NonEmpty f k v -> r
fromNonEmpty f (Tuple k v :| fa) = f k v fa

nonEmpty ::
     forall f k v.
     (f k v -> Maybe (Tuple (Tuple k v) (f k v)))
  -> f k v
  -> Maybe (NonEmpty f k v)
nonEmpty split = map (uncurry NonEmpty) <<< split

head :: forall f k v. NonEmpty f k v -> Tuple k v
head (h :| _) = h

tail :: forall f k v. NonEmpty f k v -> f k v
tail (_ :| t) = t

hmap ::
     forall f g k.
     (f k ~> g k)
  -> NonEmpty f k
  ~> NonEmpty g k
hmap f (h :| t) = h :| f t

deindex ::
     forall f g k v u.
     (Tuple k v -> u)
  -> (f k v -> g u)
  -> NonEmpty f k v
  -> UnIndexed.NonEmpty g u
deindex f g (h :| t) = f h UnIndexed.:| g t

reindex ::
     forall j k f v.
     (k -> j)
  -> (f k v -> f j v)
  -> NonEmpty f k v
  -> NonEmpty f j v
reindex f g (Tuple k v :| t) = Tuple (f k) v :| g t

index ::
     forall f g u k v.
     (u -> Tuple k v)
  -> (g u -> f k v)
  -> UnIndexed.NonEmpty g u
  -> NonEmpty f k v
index f g (h UnIndexed.:| t) = f h :| g t

foldMapWithIndex1 ::
     forall f k v s.
     FoldableWithIndex k (f k)
  => Semigroup s
  => (k -> v -> s)
  -> NonEmpty f k v
  -> s
foldMapWithIndex1 f ((Tuple k v) :| fkv) =
  foldlWithIndex (\i s a -> s <> f i a) (f k v) fkv

derive instance genericNonEmpty ::
  (Generic (f k v), Generic k, Generic v) =>
  Generic (NonEmpty f k v)

instance showNonEmpty ::
  (Generic (f k v), Generic k, Generic v) =>
  Show (NonEmpty f k v) where
  show = gShow

derive instance eqNonEmpty :: (Eq k, Eq v, Eq (f k v)) => Eq (NonEmpty f k v)
instance eq1NonEmpty :: (Eq k, Eq1 (f k)) => Eq1 (NonEmpty f k) where
  eq1 ((Tuple k1 v1) :| fkv1) ((Tuple k2 v2) :| fkv2) =
    k1 == k2 && v1 == v2 && fkv1 `eq1` fkv2

derive instance ordNonEmpty :: 
     (Ord k, Ord v, Ord (f k v)) 
  => Ord (NonEmpty f k v)
instance ord1NonEmpty :: (Ord k, Ord1 (f k)) => Ord1 (NonEmpty f k) where
  compare1 ((Tuple k1 v1) :| fkv1) ((Tuple k2 v2) :| fkv2) =
    k1 `compare` k2 <> v1 `compare` v2 <> fkv1 `compare1` fkv2

derive instance functorNonEmpty :: (Functor (f k)) => Functor (NonEmpty f k)
instance functorWithIndexNonEmpty ::
     FunctorWithIndex k (f k)
  => FunctorWithIndex k (NonEmpty f k) where
  mapWithIndex f (Tuple k v :| fkv) = Tuple k (f k v) :| mapWithIndex f fkv

instance foldableNonEmpty :: (Foldable (f k)) => Foldable (NonEmpty f k) where
  foldMap f (Tuple k v :| fkv) = f v <> foldMap f fkv
  foldl f acc (Tuple _ v :| fkv) = foldl f (f acc v) fkv
  foldr f acc (Tuple _ v :| fkv) = f v (foldr f acc fkv)
instance foldableWithIndexNonEmpty ::
     (FoldableWithIndex k (f k))
  => FoldableWithIndex k (NonEmpty f k) where
  foldMapWithIndex f (Tuple k v :| fkv) = f k v <> foldMapWithIndex f fkv
  foldlWithIndex f acc (Tuple k v :| fkv) = foldlWithIndex f (f k acc v) fkv
  foldrWithIndex f acc (Tuple k v :| fkv) = f k v (foldrWithIndex f acc fkv)
instance foldable1NonEmpty :: Foldable (f k) => Foldable1 (NonEmpty f k) where
  fold1 = fold1Default
  foldMap1 f (Tuple _ v0 :| fkv) = foldl (\acc v -> acc <> f v) (f v0) fkv

instance traversableNonEmpty ::
     Traversable (f k)
  => Traversable (NonEmpty f k) where
  sequence (Tuple k v :| fkv) =
    (NonEmpty <<< Tuple k) <$> v <*> sequence fkv
  traverse f (Tuple k v :| fkv) =
    (NonEmpty <<< Tuple k) <$> f v <*> traverse f fkv
instance traversableWithIndexNonEmpty ::
     (TraversableWithIndex k (f k))
  => TraversableWithIndex k (NonEmpty f k) where
  traverseWithIndex f (Tuple k v :| fkv) =
    (NonEmpty <<< Tuple k) <$> f k v <*> traverseWithIndex f fkv
