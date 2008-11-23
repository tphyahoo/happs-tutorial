<h3>GET and POST data</h3>

<p>GET data is data attached to an http request via the url string. In the job board app, an example of this is
   <a href=/tutorial/viewprofile?user=tphyahoo>tphyahoo's profile</a>, where the user is specified on the url string.
   Another example is <a href=/tutorial/viewjob?user=tphyahoo&job=darcshub>fetching a particular job</a>.

<p>POST data is data attached to a request after submitting a form.
   One place this is used is in the <a href=/tutorial/register>registration</a> process.
   Once you have a user created, you can also see POST in action by editing your profile or creating jobs.

<p>HAppS deals with GET and POST data similarly.

<p>Let's look at how
   <a href=/tutorial/viewprofile?user=tphyahoo>tphyahoo's profile</a> gets displayed.

<p><a href=/src/Controller.hs>Controller.hs</a>: ... , dir "viewprofile" [ methodSP GET \$ userProfile rglobs ] ...

<p>using dir and methodSP as above is a common pattern. dir pops the head element of the path array
   ["viewprofile"], resulting in an empty array. methodSP checks that the path array is empty and that 
   the method is GET. If so, control is passed to the next sp: userProfile rglobs, which is in another module.

<p><a href=/src/ControllerGetActions.hs>ControllerGetActions.hs</a>: 

<p>data UserNameUrlString = UserNameUrlString {profilename :: String}
<br>instance FromData UserNameUrlString where
<br>&nbsp;    fromData = liftM UserNameUrlString (look "user" `mplus` return "")
<br>userProfile rglobs = 
<br>&nbsp;  withData \$ \(UserNameUrlString user) ->
<br>&nbsp;&nbsp;    [ ServerPartT \$ \rq -> do .....
<br>

<p>*Main> :t withData :: FromData a => (a->[ServerPartT IO Response]) -> ServerPartT IO Response

<p>The above is the main pattern for processing GET or POST data in happs.

<p>First, you decide what argument type withData should accept. This might be a datatype you have already 
   defined, like User or Job, which already plays a major role in the app.
   Or it could be an ad-hoc datatype which is only used this once, in the form processing.
   Whatever the case you declare that data type an instance of FromData, and define how it should
   grab data attached to the request. In this case, UserNameUrlString takes one argument, so we use LiftM 
   -- for two args we would use LiftM2, three args LiftM3 etc. 
   To get that one arg, we look for a request GET variable named "user" and if we don't find it we
   use a reasonable default, in this case the empty string. The result is a value of type UserNameUrlString
   which gets passed to the ServerPartT handler in the userProfile function.

<p>Another use is... (pagination. now... sleeep.....)
