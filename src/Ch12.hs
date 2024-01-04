{-
An instance declaration has the form:
  instance (assertion1, ..., assertionn) => class type1 ... typem where ...
The part before the "=>" is the context, while the part after the "=>"
is the head of the instance declaration.
-}
{-
The 'FlexibleContexts' extension lifts the Haskell 98 restriction that
the type-class constraints in a type signature must have the form
(class type-variable) or (class (type-variable type1 type2 ... typen)).
It makes possible to declare 'MonadError e ((->) r)'.
-}
{-# LANGUAGE FlexibleContexts #-}
{-
In Haskell 98 the head of an instance declaration must be of the form
'C (T a1 ... an)', where 'C' is the class, 'T' is a data type constructor,
and the 'a1 ... an' are distinct type variables.
The 'FlexibleInstances' extension allows the head of the instance
declaration to mention arbitrary nested types, like
'MonadError e (ReaderT r m)'.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-
For each functional dependency, '⟨tvs⟩left -> ⟨tvs⟩right', of the class,
every type variable in 'S(⟨tvs⟩right)' must appear in 'S(⟨tvs⟩left)', where
'S' is the substitution mapping each type variable in the class declaration
to the corresponding type in the instance head.
The 'UndecidableInstances' extension makes the following declaration legal.
'MonadWriter w m => MonadWriter w (ReaderT r m)'
even though lhs type 'ReaderT r m' does not determine rhs type 'w'.
-}
{-# LANGUAGE UndecidableInstances #-}

module Ch12 where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Monad.IO.Unlift as U
import Control.Monad.Reader (MonadTrans (..), ReaderT (..))
import qualified Control.Monad.Reader as R
import Control.Monad.ST (ST)
import System.IO (Handle, IOMode (..))
import qualified System.IO as I

{-
Exercise 12.1. Convince yourself of the previous claim
(that the lifting operation can be generalized) about 'ReaderT'
by writing and comparing several MonadMn instances for 'ReaderT'.
-}
liftThroughReaderT :: m a -> ReaderT r m a
liftThroughReaderT x = ReaderT $ const x

{-
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
The type variables defined are as follows:

r = resource from the environment
m = the resulting Monad
a = value returned in the Monad

https://blog.ssanj.net/posts/2018-01-12-stacking-the-readert-writert-monad-transformer-stack-in-haskell.html
-}

class (Monad m) => MonadReader r m | m -> r where
  ask :: m r
  reader :: (r -> a) -> m a
  {- HLINT ignore -}
  reader f = f <$> ask

class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell :: w -> m ()

{-
Need the assertion 'MonadReader r m' because we call 'ask' on it.
-}
instance (MonadReader r m) => MonadReader r (ReaderT r m) where
  ask :: ReaderT r m r
  ask = liftThroughReaderT ask

{-
'((->) r) :: Type -> Type' takes some type 'a' and returns the type
that represents all functions 'r -> a'. In this example, instance
'MonadError e ((->) r)' is the instance of 'MonadError' where the Monad
is '(->) r', which is sometimes called the "reader monad", since it
effectively lets you "read" a value of type 'r', but not change it.

For this specific instance of 'MonadError', the underlying monad is
'ReaderT r m'. This gives us:

catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a

Calling 'catchError' on variable terms 'm' and 'h' gives us:

m :: ReaderT r m a and h :: (e -> ReaderT r m a)

Now we can start evaluating the RHS and plug in the types:
catchError m h =
  ReaderT $
    catchError
      (runReaderT m :: r -> m a)
      (runReaderT . h :: e -> (r -> m a))

Simplifying a bit gives us:

ReaderT $ catchError (_a :: r -> m a) (_b :: e -> (r -> m a)

which is the definition of 'catchError' where the underlying monad is '(->) r'.
Since 'catchError' requires the monad to be a 'MonadError', we need '(->) r'
to be a 'MonadError'.

At the last part, it might seem the underlying monad should be 'r -> m a',
and we should declare 'MonadError e (r -> m a)'. But a monad has kind '* -> *',
whereas 'r -> m a' has kind '*', which is why the definition left out 'm a'.
It's the same reason we write 'Monad Maybe' and not 'Monad Maybe a'.
-}
instance
  (MonadError e m, MonadError e ((->) r)) =>
  MonadError e (ReaderT r m)
  where
  throwError :: e -> ReaderT r m a
  throwError = liftThroughReaderT . throwError

  catchError :: ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
  catchError m h = ReaderT $ catchError (runReaderT m) (runReaderT . h)

{-
Need the assertion 'MonadWriter w m' because we call 'tell' on it.
-}
instance (MonadWriter w m) => MonadWriter w (ReaderT r m) where
  tell :: w -> ReaderT r m ()
  tell = liftThroughReaderT . tell

{-
Exercise 12.2. Write the code for the following instances:

instance MonadBase IO IO
instance MonadBase (ST s) (ST s)

Hint: notice that the types in both arguments coincide.

Then write the code for the instance that lifts an operation
from a monadic stack mone additional layer t:

instance (MonadBase b m, MonadTrans t) => MonadBase b (t m)
-}
class (Monad b, Monad m) => MonadBase b m | m -> b where
  liftBase :: b a -> m a

instance MonadBase IO IO where
  liftBase = R.liftIO

instance MonadBase (ST s) (ST s) where
  liftBase = liftBase

instance
  (Monad (t m), MonadBase b m, MonadTrans t) =>
  MonadBase b (t m)
  where
  liftBase = lift . liftBase

{-
Exercise 12.8. Write the lifted version of the following function,
using MonadUnliftIO:

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r

Note that you have to define both the new signature and its implementation.
-}
withFile ::
  (MonadUnliftIO m) =>
  FilePath ->
  IOMode ->
  (Handle -> m r) ->
  m r
withFile name mode f = U.withRunInIO $ \run -> I.withFile name mode (run . f)
