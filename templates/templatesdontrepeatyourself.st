<h3>Templates -- So You Don't Repeat Yourself</h3>

<p>Every page in this tutorial has certain things in common -- the header and menu bar for example.
You wouldn't want to have have to change the menu bar on every single page if there was a new menu item.

<p>This is why you need a templating system.</p>

<p>HAppS doesn't care much what templating system you use.  I use the
<a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HStringTemplate">HStringTemplate</a>
package to get the job done, so that's the syntax you'll be seeing in what follows. </p>

<p>
A templating system also helps you individualize output.
The way this works is by inserting variable text into placeholder templates.
For instance, the menu bar in this tutorial displays "logout your_username" if you are logged in,
rather than the login/register options. 
The line below does this too, just for teaching purposes. If you are logged in, it will display your username.
</p>

<p> Logged In? Let's see: $ loggedInUser $ </p>

<p>Have a look at the <a href="/templates/templatesdontrepeatyourself.st">template responsible for the content pane of this page</a>. It's pretty boring, except the line above reads as

<p> &quot; Logged In? Let's see: \$ loggedInUser \$ &quot;

<p>Now, if you are running this tutorial locally, load up ghci by running ./hackInGhci</p>

<p>You can see the effect of
rendering the current content pane by calling

<p> *Main> :m +Misc View Text.StringTemplate

<br>*Main Misc View Text.StringTemplate> do templates <- directoryGroupSafer "templates" ; writeFile "output.html" \$ renderTemplateGroup templates [] "templates-dont-repeat-yourself" 

<p>and then opening the file output.html in firefox. (On ubuntu, in ghci, I just do ":! firefox output.html &" and the file opens in a new tab in firefox.)

<p>To see how this page would look if you were logged in: 

<p>*Main Misc View Text.StringTemplate> do templates <- directoryGroupSafer "templates" ; writeFile "output.html" \$ renderTemplateGroup templates [("loggedInUser","DarthVader")] "templates-dont-repeat-yourself" 

<p>and reopen output.html in your browser.

<p>As you may have noticed, the html written by the above command is only the current content pane, not the header or table of content links. To render a full page from ghci, as it would appear for a logged in user, try

<p>*Main Misc View Text.StringTemplate> do templates <- directoryGroupSafer "templates"; writeFile "output.html" \$ tutlayout (RenderGlobals templates (Just \$ User "Darth" "" undefined undefined)) [] "templates-dont-repeat-yourself"

<p>Note that tutlayout is a pure function. The only IO is in fetching the templates with directoryGroupSafer.
Currently the tutorial does a directory read every time the main webserver handler loops, but it could just
as easily only do the directory read once at app startup time. This would mean a lot less disk reads, but
of course you would have to stop/start the server whenever a template changed to see your changes. Which setup
you want depends on whether you are willing to sacrifice development convenience for a snappier server.
I opted for convenience, but it would be easy to change -- just move the directoryGroupSafer command out of the
tutorial handler and into Main.hs before the server starts. Try it as an exercise if you like.

<p>As a side note, you can get away with passing undefined values to the tutlayout function because
it never attempts to evaluate the profile and jobs bits of the User data object. That's lazy evaluation at work.
<p>If you reload output.htlm, you'll see you are missing the header image and css because you're opening a plain html file
rather than a page being served by happs (which knows where to find the images and css),
but other than that the layout is complete.

<p>You can get a sense for how this all works by looking at the tutlayout function in <a href="/src/View.hs">src/View.hs</a>.

<p>It's not too much fun to develop a web page by outputting a string to a static file and then opening 
it in a browser every time something changes, so the next thing you might want to try is actually
modifying the current template (in ./templates/templates-dont-repeat-yourself.st) with some random text,
reloading this actual page, and watching your changes appear.

$!
<p>You might have also noticed that the table of contents-style navigation links in the left pane
change colors depending on what page is selected. You could have a look at the the
<a href="/templates/tableofcontents.st">/templates/tableofcontents.st</a> to get another taste of how templating works.
Here, HAppS looks at each request to determine what the page was called. 
If the page matches anything in a certain list, the link class gets set to an "active" value, otherwise it
gets a default value. 
!$

<p> We'll learn some <a href="/tutorial/stringtemplate-basics">StringTemplate basics</a> next.</p>


