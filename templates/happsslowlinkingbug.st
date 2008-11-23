<h3>HAppS Slow Link Time Workarounds</h3>
<p>
The fact that cabal-installing HAppS takes an hour is
officially a <a
href="http://code.google.com/p/happs/issues/detail?id=29">bug</a>. Hopefully
this situation will be remedied as HAppS matures and, eventually, has
an official release.  
<p>
I found this bug pretty problematic when I was experiencing it and I know I'm not the only one. 
So, I will share some experiences and observations that will hopefully help others,
and maybe even help diagnose and eventually squash this bug.
<p>
First of all, you don't actually need to compile an executable to run a HAppS server, 
and when you run from inside ghci the link time bug has no effect. So for a while I 
was doing this, by loading ghci using ./hackInGhci.sh and then running runInGhci inside Main.hs.
<p>
Secondly, at some point this problem went away, and my link time dropped from 5-10 minutes to under 10 seconds.
<p>
This is definitely due to a change in my own code base, not HAppS repo code, since I am only 
running against what I cabal installed and not the volatile HAppS library code in darcs.
At some point I intend to attempt a more precise diagnosis
by doing binary cuts on my repo and identifying the changes that seem
to have the biggest impact. I do have some suspicions. 
<ul>
  <li>Problems are related to Template Haskell and/or Data.Deriving</li>
  <li>Splitting up big modules into smaller modules
      <br>When I saw link times over 5 minutes I tried to isolate the 
        "slow" methods in a file called "Slow.hs" so that the linking is only slow when that file changes.
        Lately I removed the Slow module since it didn't seem necessary anymore.</li>
  <li>Supplying type signatures helps, and the more concrete the type signature the better.
  <br>So, (askUsers :: Query TutorialState (M.Map String User) ) rather than (askUsers :: MonadReader State m => m (M.Map String User) )
  <br>tentative idea: ghc -fwarn-missing-signatures and give maximally precise signatures everywhere.</li>
</ul>

<p>

The shell command ./runServer.sh, which creates an executable and starts the server, also times the compile & link, and rings a bell when it's done. I figure this will be helpful if slow link times creep back in.
