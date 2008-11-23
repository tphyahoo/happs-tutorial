<h3>Run This Tutorial Locally</h3>

<p> Before going further, you may want to inform yourself about the <a href=/tutorial/prerequisites>basic prerequisites</a>, both knowledge and equipment, you need to make the best use of this tutorial </p>

<p>This tutorial is cabalized. You can install it, and chase down all the HAppS dependencies it needs, simply by doing 
<p>cabal install happs-tutorial

<p>The cabal installation may take up to an hour, mainly because the
HAppS-Server installation is slow, but it should succeed in one
shot. This is a symptom of the <a href="/tutorial/happs-slow-linking-bug">HAppS slow linking bug</a>.

<p>If you've never used cabal install or need more detailed info....</p>

<ul>
    <li>Haskell: You need at least ghc 6.8.2 to install HAppS. I installed this with with apt-get install haskell (works for ubuntu hardy heron), and then <a href="http://www.haskell.org/ghc/download.html">upgraded to ghc 6.8.3</a> as this is supposed to have fixed some bugs.</li> 
    <li>Dependency chasing haskell package installers: you should have the latest versions of cabal and cabal install from <a href="http://hackage.haskell.org/packages/archive/pkg-list.html">hackage</a>. These are already included in the latest version of ghc, or they will be soon. Another reason to upgrade to ghc 6.8.3.</li>
    <li>If you want to check out the latest version of this tutorial, install <a href="http://www.darcs.net">Darcs</a> and check out the repo with darcs get http://code.haskell.org/happs-tutorial</li>
</ul>

<p>I cabalized happs-tutorial was for the dependency chasing you get with cabal install,
not for actually running it.


<p>Cabal installs an executable somewhere that you can run, but the tutorial pages won't display because
the executable needs template files to display pages correctly. To actually run the tutorial locally, copy the 
happs-tutorial.tar.gz distribution file that cabal downloaded -- probably somewhere under  ~/.cabal if you're on linux: 

<p><i>find ~/.cabal | grep -i happs-tutorial</i>

<p>should show you a tar file. Or you can just download the tar file from
<a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/happs-tutorial">hackage</a>.
Once you have the tar file, untar this somewhere, cd into that, build and run here as described below.
Or, you could darcs get happs-tutorial and run there. 

<p>To run the app, either do ./hackInGhci.sh and then execute runInGhci inside Main.hs, or recompile
   the executable using ./runServerWithCompileNLink.sh.
   Really you only need to be inside ghci if you are experiencing
   the <a href="/tutorial/happs-slow-linking-bug">slow link time issue</a>.
   This isn't a problem at time of writing but seems to crop up from time to time.
<p>
Shutdown with ctrl-c.
<p>
You should now be able to browse this tutorial offline by running the executable, and opening http://localhost:5001 in your browser.

<p>Every so often, when starting via runInGhci, you may get an error message like:

<p>*Main> runInGhci
happs tutorial running in ghci.
exit :q ghci completely and reenter ghci, before restarting.
*** Exception: _local/happs-tutorial_state/events-0000000006: openFile: resource busy (file is locked)

<p>Don't worry about it. Every time I get this error I simply run runInGhci again, and the second time it always works.

<p>
You may also want to <a href="start-happs-on-boot">start HAppS on boot</a>.

<p>Next up is the <a href="/tutorial/main-function">HAppS server main function</a>.</p>




