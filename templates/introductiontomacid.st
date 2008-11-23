<h3>Introduction To Macid</h3>

<p>Macid is the HAppS storage mechanism that allows you to use whatever data structure you want
   to hold your permanent data, without worrying about getting it into and out of tabular form
   fit for storage in a traditional <a href="wikipedia tk">rdbms</a>.

<p>Before delving into how it works let's learn a bit about it from an operational perspective. 
   Assuming you have the tutorial installed and running locally: 

<ul>
  <li><a href=/tutorial/register>register</a> a new user with username "testuser" and password "testpassword"
  <li>Optionally, fill out profile information for your new test user, and/or make some job posts.
</ul>

<p>The data you entered is stored on your filesystem in a directory under your running executable, called <i>_local</i>.

<p>~/happs-tutorial>ls _local/happs-tutorial_state/
<br>current-0000000000  events-0000000000  events-0000000001  events-0000000002

<p>You can grep for it too.

<p>
thartman@thartman-laptop:~/happs-tutorial>grep -ra testuser _local
<br>_local/happs-tutorial_state/events-0000000000:ß\$6¡·¢:1525374391 696985193?AppStateSetBased.AddUsertestuser e1
       ... (and lots more lines of binary data)

<p>Hm... let's see, can we be sneaky and grep for the password?

<p>Try it, you can't. Because the password is stored as an md5 hash, out of respect for the privacy of your users.
   See the newUserPage function in <a href=/src/ControllerPostActions.hs>ControllerPostActions</a> if you're curious. 

$!<p>Keeping application data in static files
   may seem like a weird thing to do if you have gotten used to keeping application data in a database.
   But it's not really that weird. !$ 


<p><a href="/tutorial/maciddatasafety">Keeping your macid data safe</a>


