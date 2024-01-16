module Notula.Assoc where

import Notula.Prelude

import Data.Set as Set
import Data.Array as Array
import Data.Semigroup.Last (Last (..))
import Data.Newtype (un)
import Data.Foldable (foldlDefault, foldrDefault)


fst :: forall a b. a /\ b -> a
fst (a /\ _) = a

snd :: forall a b. a /\ b -> b
snd (_ /\ b) = b


-- | Association list. Right-biased.
newtype Assoc k v = Assoc (Array (k /\ v))

instance Functor (Assoc k) where
  map f (Assoc kvs) = Assoc (kvs # map \(k /\ v) -> k /\ f v)

instance Foldable (Assoc k) where
  foldMap f (Assoc kvs) = foldMap f (snd <$> kvs)
  foldl x = foldlDefault x
  foldr x = foldrDefault x

instance Traversable (Assoc k) where
  traverse f (Assoc kvs) = Assoc <$> (Array.zip (map fst kvs)) <$> (traverse f (map snd kvs))
  sequence = traverse identity

instance Semigroup (Assoc k v) where
  append (Assoc xs) (Assoc ys) = Assoc (xs <> ys)

instance Monoid (Assoc k v) where
  mempty = Assoc []

derive instance Generic (Assoc k v) _

derive instance (Eq k, Eq v) => Eq (Assoc k v)

instance (Show k, Show v) => Show (Assoc k v) where
  show x = genericShow x


empty :: forall k v. Assoc k v
empty = Assoc []

singleton :: forall k v. k -> v -> Assoc k v
singleton k v = Assoc [k /\ v]

fromFoldable :: forall f k v. Foldable f => f (k /\ v) -> Assoc k v
fromFoldable = Assoc <<< Array.fromFoldable

append :: forall k v. k -> v -> Assoc k v -> Assoc k v
append k v (Assoc kvs) = Assoc (kvs <> [k /\ v])

prepend :: forall k v. k -> v -> Assoc k v -> Assoc k v
prepend k v (Assoc kvs) = Assoc ([k /\ v] <> kvs)

has :: forall k v. Eq k => k -> Assoc k v -> Boolean
has key (Assoc kvs) = kvs # any (fst >>> (_ == key))

-- Remove from the first list any keys present in the second
minus :: forall k v. Ord k => Assoc k v -> Assoc k v -> Assoc k v
minus (Assoc xs) (Assoc ys) = Assoc (xs # Array.filter keep)
  where
  keep (k /\ _) = not (Set.member k ysKeys)
  ysKeys = Set.fromFoldable $ fst <$> ys

remove :: forall k v. Eq k => k -> Assoc k v -> Assoc k v
remove key (Assoc kvs) = Assoc $ kvs # Array.filter (fst >>> (_ /= key))

lookup :: forall k v. Eq k => k -> Assoc k v -> Maybe v
lookup key (Assoc kvs) = kvs # foldMap (\(k /\ v) -> if k == key then Just (Last v) else Nothing) # map (un Last)

toArray :: forall k v. Assoc k v -> Array (k /\ v)
toArray (Assoc kvs) = kvs

filterKeys :: forall k v. (k -> Boolean) -> Assoc k v -> Assoc k v
filterKeys pred (Assoc kvs) = Assoc $ kvs # Array.filter (fst >>> pred)

filterWithKey :: forall k v. (k -> v -> Boolean) -> Assoc k v -> Assoc k v
filterWithKey pred (Assoc kvs) = Assoc $ kvs # Array.filter (uncurry pred)

findMapWithKey :: forall k v r. (k -> v -> Maybe r) -> Assoc k v -> Maybe r
findMapWithKey f (Assoc kvs) = kvs # Array.findMap (uncurry f)
