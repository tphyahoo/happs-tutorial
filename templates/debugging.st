<h3>Debugging</h3>

<p>A few words on debugging.

<p>One thing you can do is put debugFilter before the serverPartT that seems to be causing trouble.
   <br> This will spit out the request and response objects, which can be helpful.

<p>*Main> :t debugFilter :: Show a => [ServerPartT IO a] -> [ServerPartT IO a]
<br>debugFilter :: Show a => [ServerPartT IO a] -> [ServerPartT IO a] :: (Show a) =>
<br>                                                                     [ServerPartT IO a] -> [ServerPartT IO a]
<br>*Main> :i debugFilter
<br>debugFilter ::
<br>  (Show a, Control.Monad.Trans.MonadIO m) =>
<br>  [ServerPartT m a] -> [ServerPartT m a]
<br>  	-- Defined in HAppS.Server.SimpleHTTP

<p>I personally don't use debugFilter much, as it gives almost too much information.

<p>Instead, I depend on Debug.Trace.trace (a standard library function which sneaks IO in
   anywhere you have a showable value), which I augmented with some helper functions (in Misc.hs).

<p>traceTrue x = trace (show x ++ "\n\n") True
<br>traceIt x = trace (show x ++ "\n\n") x
<br>traceMsg msg x = trace ( "\n\n" ++ msg ++ (show x) ++ "\n\n") x

<p>Typical (basically only) use of traceTrue: view arguments to a function in stdout,
   by putting it on the right side of the
   "execute this branch if true" bar in a function definition.
   The function executes as it normally would, because traceTrue always returns true.
   But you get debugging info as a side effect.

<p>tutlayout (RenderGlobals ts mbU) attrs tmpl0 | traceTrue ((RenderGlobals ts mbU), attrs, tmpl0) = .....

<p>Typical use of traceIt: quickie print to stdout of some var or expression:

<p> mainUserMenu = if (isJust mbU) 
<br>&nbsp;                   then traceIt \$ paintHMenu .
<br>&nbsp;&nbsp;                            map (menuLink ts ("/tutorial/" ++ tmpl0) )
<br>&nbsp;&nbsp;&nbsp;                                    . readtut \$ "mainusermenu"
<br>&nbsp;                           else "" 

<p>traceMsg does pretty much the same thing as traceIt, except you preface the shown expression with
   your own message like "show the menu: ". This can be useful if you are doing more than one trace
   and need to disginguish them.

<p>There are probably smarter ways of debugging a happs app, the above is just what works for me.
   (Actually, I debug using these trace helpers all the time, not just in HApps.)
   If I get useful feedback on how other happs users approach debugging I will update this page.

<p>Incidentally, ghci has a debugger since 6.8.3. Overall it's great, but I didn't find it too useful for happs,
   because it would die out on me in ways I didn't understand, typically around hard to understand exceptions.
   Basically, HAppS would behave differently when I was stepping through it with the ghci debugger than 
   it would when running without the debugger.
   I suspect it's because the ghci debugger lacks support for multithreaded programs, or something along those lines.
   At any rate, I wound up not using the interactive debugger much, though I would have liked to.
