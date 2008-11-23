<h3>Real World HAppS</h3>

<p>Haskell is a great way to program.</p>

<p>And <a href="http://www.happs.org">HAppS</a> is a great way to build
web applications.  

<p>Especially if you believe, like I do, that as
modern software systems tend toward ever increasing complexity,
database usage is an unnecessary source of complication that
<a href=http://gilesbowkett.blogspot.com/2007/05/sql-unnecessary-in-haskells-happs.html>should be factored out</a> where possible.

<p>Ruby's <a href="http://rubyonrails.com/">rails</a> and python's <a href="http://www.djangoproject.com">django</a> have become popular largely because of their <a href="">object relational mapping</a> systems,
which hide the complexity of database engines by converting application data manipulation logic into sql.
When I first used an ORM, it felt like a huge improvement over writing sql statements every time I wanted to manipulate
an application's state. But pretty soon ORMs started seeming hackish to me too. At some point,
the metaphors I wanted to use just <a href="http://en.wikipedia.org/wiki/Object-Relational_impedance_mismatch">broke down</a>.


$!Or to put it another way, that <a
href=http://gilesbowkett.blogspot.com/2007/05/sql-unnecessary-in-haskells-happs.html>sql
is an ugly hack</a>. !$
<p>
HAppS is haskell's answer to rails and django (and perl's <a href="http://www.catalystframework.org/">catalyst</a>, and <a href="http://www.php.net">php</a>).
$! , and every ORM ever written in the history of software) !$ 
With HAppS, there is no wrangling data
structures into and out of the database, because there is no database.  You use whatever data
structures are natural to your application, and serialize them
transparently using
<a href="http://www.google.com/search?q=scrap+your+boilerplate">powerful</a>
machinery</a> that's running <a href="http://www.haskell.org/th/">behind the
scenes</a>. And if there are existing databases that you need to connect to, you can do that too -- you're not locked in to using macid for everything.

<p>The above description is a bit idealized. Keeping everything in macid limits you to how much RAM you can afford,
   and even if you can afford a lot (16GB in the amazon cloud costs \$576/month) there's no guarantee that you won't
   max that out if your application has a lot of data.
   (See the <a href=/tutorial/macid-stress-test>stress test</a> chapter for more caveats.)
   The HAppS developers have promised a version of HAppS that will make it easy to share ram across computers
   with a technique called sharding, but this hasn't been released in a way that inspires confidence in me
   (on hackage, sufficient documentation),
   and to be honest I don't really understand how it is supposed to work even in theory.
   But what is realistic is to write an alpha version of an application without a database access layer, 
   and then add persistent hard drive storage (probably database, but could also be flat files or name your poison)
   outside of macid when it becomes necessary. Most web projects do not get to a size where this is necessary,
   so arguably coding in a database from a start is a form of insidious premature optimization, if you buy
   the argument that databases have overhead.

<p><a href="http://hackage.haskell.org/packages/archive/HAppS-State/0.9.2.1/doc/html/HAppS-State.html#1">MACID</a>,
the HAppS storage mechanism, is no vanilla serialization layer that will
start acting in weird ways when an application has many concurrent users doing possibly conflicting things. 
By <a href="http://research.microsoft.com/~simonpj/papers/stm/">leveraging haskell's type system</a> 
(see composable memory transactions paper),
you get the same <a href="http://en.wikipedia.org/wiki/ACID">ACID</a> guarantees that
normally only come with a database.  </p>

<p>You also get all the <a href="http://www.google.com/search?hl=en&q=why+haskell">goodness</a>
that comes from programming in <a href="http://www.haskell.org">haskell</a>, my favorite language.</p>

<p>In short, HAppS is awesome, and webmonkeys everywhere should use it. Except...

<p>There is this one <a href="/tutorial/missing-happs-documentation">minor detail</a>.



