<h3>Internationalization</h3>
<p>I had some problems displaying non-ascii characters such as ö, ä, and ü, although now everything that I want 
works (except for utf8 data in dummy data). I can create users with foreign characters in usernames or profile fields,
jobs with foreign characters in the description, and so on.

<p>But I wasted enough time on this that I thought I'd say a few words in hopes of saving others trouble.

<p>First of all, if you are running this tutorial locally and are seeing escape characters in the paragraph 
above, set your locale by doing something like (on linux)
<p>
export LANG=en_US.UTF-8
<br> sudo update-locale

<p>and restart the tutorial app. Hopefully at least then this page displays properly, as it does for me.

<p>Another tip: don't use emacs, or emacs22, to edit files with utf8 data. Supposedly emacs22 has good utf8 
support, but I can't even figure out a way for emacs to tell me whether the file I'm in is in utf8 or latin1, 
which led to some annoying confusions. Instead, use kate (a free apt-gettable editor), use tools-> encoding
to see what encoding you are seeing, and always set the kate encoding to utf8. If a tutorial template file
displays properly in kate with utf8, it should display properly in the live tutorial as well.

<p>Another annoyance: when I attempted to create dummy data with utf8 data (edited in kate)
and save this to HAppS State, this did NOT display correctly. (See AppStateSetBased.hs) I got \123 type
character escape sequences. I think this is because Serialization is based on show instances of data,
and ... well...
<br>
<br>*Main> show "ö"
<br>"\"\\246\""
<br>*Main> putStrLn "ö"
<br>ö

<p>This annoyance may be alleviated when ghc support for unicode is added sometime after ghc 6.8.10, as alluded
to in the <a href="tk">release notes</a>

<p>I also had a problem where data in form textbox fields was mysteriously showing up \ed. It turned out 
this is because I sloppily used show to escape quotes, rather than sticking a quote at the beginning and 
end of the string as was more proper. I then got bitten by show as above, causing much misery.

<p>But all that having been said, as the live demo shows, in general utf8 data does work as it should.
