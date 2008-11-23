<h3>Appendix: Using ghci with HAppS</h3>

<p><font color=orange>When working with HAppS, I seem to spend a lot of time in ghci looking at type signatures,
   and it's not always obvious what's going on. So I thought I'd share a little mini ghci session 
   in case this helps anyone.</font>

<p>*Main> :t query AskDatastore
<br>query AskDatastore :: (Control.Monad.Trans.MonadIO m) => m (Data.Set.Set User)
<br>*Main> :t query AskDatastore :: WebT IO (Data.Set.Set User) <font color=orange>-- concretize the type</font>
<br>query AskDatastore :: WebT IO (Data.Set.Set User) :: WebT IO (Data.Set.Set User) <font color=orange>-- hoorah, it typechecks</font>
<br>
<br>*Main> :t update InitializeDummyData
<br>update InitializeDummyData :: (Control.Monad.Trans.MonadIO m) => m ()
<br>*Main> :t update InitializeDummyData :: WebT IO () <font color=orange>-- concretize the type</font>
<br>update InitializeDummyData :: WebT IO () :: WebT IO ()
<p><font color=orange>What you see above are nice looking types. And they can be made concrete, with WebT -- no
   class class context in the type signature. This is Right.</font>	 

<p><font color=orange>What happens if we try an update on something that should really be a query?
   Does it get rejected?</font>

<p>*Main> :t update AskDatastore
<br>update AskDatastore :: (UpdateEvent AskDatastore res, Control.Monad.Trans.MonadIO m) => m res
<br>*Main> :t query InitializeDummyData
<br>query InitializeDummyData :: (QueryEvent InitializeDummyData res, Control.Monad.Trans.MonadIO m) => m res
<p><font color=orange>Typechecks with a weird class context, but will complain if run,
   and impossible to make concrete in ghci using :: to narrow the type. Ugly looking types. This is Wrong. </font>

<hr>

<p><font color=orange>What exactly is askDatastore?</font>

<p>*Main> :i askDatastore
<br>askDatastore :: Query AppState (Data.Set.Set User)
<br>  	-- Defined at src/AppStateSetBased.hs:43:0-11

<p><font color=orange>what's a Query?</font>

<p>Prelude Control.Monad.Reader HAppS.State GHC.Conc> :i Query
<br>type Query state = Ev (ReaderT state STM)
<br>  	-- Defined in HAppS-State-0.9.2.1:HAppS.State.Types
<br>
<br>data StateStuff.Method st where
<br>  ...
<br>  Query :: forall st ev res.
<br>           (QueryEvent ev res) =>
<br>           (ev -> Query st res) -> StateStuff.Method st
<br>  	-- Defined in HAppS.State.ComponentSystem

<p><font color=orange>that's weird. in the type synonym Query takes a single arg,
    but in the definition for askDatastore, it took two.  I guess the type system uses currying.
    Let's just substitute the concrete type AppState for the type variables state and see if 
    this mess typechecks.
    </font>

<p>*AppStateSetBased Control.Monad.Reader GHC.Conc Data.Set> :t askDatastore :: Ev (ReaderT AppState STM) (Set User)
<br>askDatastore :: Ev (ReaderT AppState STM) (Set User) :: Ev (ReaderT AppState STM) (Set User)
<br>*Main> 
<br>

<P><font color=orange>No type errors -- it works.
<br>Think of it like this: askDatastore is a macid event that reads a value.

<p>:i ReaderT
<br>newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
<br>  	-- Defined in Control.Monad.Reader

<p>The type constructor ReaderT takes three arguments to yield a concrete type. 
   In the definition for askDatastore, it is partially applied,
   with two arguments. ReaderT AppState STM is an instance of Monad,
   and also an instance of MonadReader AppState, with AppState as the
   reader environment and STM (software transactional memory) as the wrapped monad.

<p>*Main Control.Monad.Reader HAppS.State GHC.Conc> :i ReaderT
<br>instance (Monad m) => Monad (ReaderT r m)
<br>&nbs;&nbsp;*Main Control.Monad.Reader HAppS.State GHC.Conc> :i ReaderT
  -- Defined in Control.Monad.Reader
<br>instance (Monad m) => MonadReader r (ReaderT r m)
<br>&nbsp;&nbsp;  -- Defined in Control.Monad.Reader

<p>What about Ev?... nah.

<p>That is as far down the HAppS type rabbit hole as I am going to go.
</font>

