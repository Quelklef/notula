-- For internal use only

module Notula.Prelude (module Notula.Prelude, module X) where
  
import Prelude as X

import Effect (Effect) as X
import Effect.Class (class MonadEffect, liftEffect) as X
import Effect.Unlift (class MonadUnliftEffect, withRunInEffect) as X
import Control.Monad.Trans.Control (class MonadBaseControl, liftBaseWith, restoreM) as X
import Control.Alt ((<|>)) as X
import Foreign (Foreign) as X
import Type.Proxy (Proxy (..)) as X
import Prim.Coerce (class Coercible) as X
import Safe.Coerce (coerce) as X
import Data.Functor.Contravariant (class Contravariant, cmap, (>$<)) as X
import Data.Generic.Rep (class Generic) as X
import Data.Show.Generic (genericShow) as X
import Data.Ord.Generic (genericCompare) as X
import Data.Foldable (class Foldable, fold, foldl, foldr, foldMap, intercalate, elem, minimum, maximum, null, sum, any) as X
import Data.FoldableWithIndex (foldMapWithIndex) as X
import Data.Traversable (class Traversable, traverse, for, for_) as X
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as X
import Data.Tuple.Nested ((/\), type (/\)) as X
import Data.Either (Either (..)) as X
import Data.Maybe (Maybe (..), fromMaybe) as X
import Data.Map (Map) as X
import Data.Monoid (guard, power) as X
import Data.Monoid.Endo (Endo (..)) as X
import Data.Lens.Types (Getter') as X
import Data.Lens.Lens (Lens', ALens', cloneLens) as X
import Data.Lens.Setter (Setter', (.~), (%~)) as X
import Data.Lens.Getter ((^.)) as X
import Data.Lens.Record (prop) as X
import Data.Lens.Lens.Tuple (_1, _2) as X
import Data.Newtype (class Newtype) as X
import Data.Identity (Identity (..)) as X

foreign import dTrace :: forall a r. a -> r -> r
foreign import dTraceWith :: forall a. String -> a -> a

dTraceThis :: forall a. a -> a
dTraceThis a = dTrace a a

dTraceShowThis :: forall a. X.Show a => a -> a
dTraceShowThis a = dTrace (X.show a) a

as :: forall @a. a -> a
as x = x

runIdentity :: forall a. X.Identity a -> a
runIdentity (X.Identity a) = a
