<h3>Where it all begins: the function main</h3>

<p>Have a look at <a href="/src/Main.hs">Main.hs</a> module at the core of this web application.</p>

<p>Two bits of code that should jump out at you as being important are 

<ol>
  <li>startSystemState (Proxy :: Proxy AppState) -- start the HAppS state system
  <li>simpleHTTP (Conf {port=p}) controller -- start serving web pages
</ol>

<p>We'll pospone learning about the HAppS state system (the first line) for later.</p>

<p>To learn more about what the controller function is doing,
   , open this file in ghci: cd src; ghci Main.hs and have a look at these functions using ghci :info .</p>

<p>*Main> :i controller
<br>controller :: [ServerPartT IO Response]
<br>  	-- Defined at <a href="/src/Controller.hs">Controller.hs</a>...
<br>*Main> :i ServerPartT
<br>newtype ServerPartT m a
<br>  = ServerPartT {unServerPartT :: Request -> WebT m a}
<br>        -- Defined in HAppS.Server.SimpleHTTP
<br>instance [overlap ok] (Monad m) => Monad (ServerPartT m)
<br>  -- Defined in HAppS.Server.SimpleHTTP
<br>*Main> :i WebT
<br>newtype WebT m a = WebT {unWebT :: m (Result a)}
<br>  	-- Defined in HAppS.Server.SimpleHTTP
<br>instance [overlap ok] (Monad m) => Monad (WebT m)
<br>  -- Defined in HAppS.Server.SimpleHTTP
<br>*Main> :i Result
<br>data Result a
<br>  = NoHandle | Ok (Response -> Response) a | Escape Response
<br>  	-- Defined in HAppS.Server.SimpleHTTP
<br>instance [overlap ok] (Show a) => Show (Result a)
<br>  -- Defined in HAppS.Server.SimpleHTTP
<br>
</p>

<p>The controller function is a list of ServerPartTs, which are basically handlers that
accept an HTTP request and return a response. 
Well, ok... this is a bit obfuscated by the many types involved in the construction, 
and if you want to be pedantic it's probably a bit more complicated than that, but you don't need to understand all
the details at this point. So, for the moment, just think about a ServerPartT as a wrapper
over a function that takes an HTTP request and returns a response.

We look at what is going on in the controller code in
<a href="/tutorial/basic-url-handling">basic url handling</a>, next.
</p>

$! 
<p>Now you have a choice about what to read next.</p>

<p>If you are in a hurry to write a HAppS application without delving too much into what's going on behind the scenes,
read , which looks at what's happening in the controller code.</p>

<p>If you want to understand the HAppS type system in more detail, read <a href="understanding-happs-types">understanding HAppS types</a>.
</p>
!$

