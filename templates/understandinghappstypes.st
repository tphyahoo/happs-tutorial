
<p>I find it helpful to have ghci tell me about the types in the code I am reading. Here is a snip of ghci output.</p>

<p>
<br>*Main>:i simpleHTTP
<br>simpleHTTP :: (ToMessage a) => Conf -> [ServerPartT IO a] -> IO ()
<br>  	-- Defined in HAppS.Server.SimpleHTTP
<br>*Main>:i Conf
<br>data Conf = Conf {port :: Int}
<br>        -- Defined in HAppS.Server.HTTP.Types
<br>*Main> :i ServerPartT
<br>newtype ServerPartT m a
<br>  = ServerPartT {unServerPartT :: Request -> WebT m a}
<br>  	-- Defined in HAppS.Server.SimpleHTTP
<br>instance [overlap ok] (Monad m) => Monad (ServerPartT m)
<br>  -- Defined in HAppS.Server.SimpleHTTP
<br>*Main> :i WebT
<br>newtype WebT m a = WebT {unWebT :: m (Result a)}
<br>  	-- Defined in HAppS.Server.SimpleHTTP
<br>instance [overlap ok] (Monad m) => Monad (WebT m)
<br>  -- Defined in HAppS.Server.SimpleHTTP
</p>

<p>So basically what this tells you is that the meat of a HAppS application is a list of ServerParts, which themselves are a wrapper over a function that takes an HTTP request to a response.</p>

<p>I use ghci info a lot, and you should too as you are learning HAppS. This kind of comfort with the HAppS type system is particularly important at the present time, when the HAppS documentation is relatively sparse</p>

<p><a href="http://www.haskell.org/haskellwiki/HAppS_tutorial">HAppsTutorial2</a> at the haskell wiki has a more in-depth
look at thinking behind HAppS and the HAppS type system. </p>
