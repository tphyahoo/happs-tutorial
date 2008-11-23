<h3>File Uploads</h3>

If you want to do file uploads, look at the code that is responsible for adding a profile picture in <a href=/projectroot/src/ControllerPostActions.hs>ControllerPostActions::processformEditConsultantProfile</a>

<p>The same pattern for dealing with form data in the previous chapter holds.
   The only differences are that 

<ul>
  <li>the value for the image file that is being passed in via withData is the entire contents of a file, as a bytestring:
      <br><i>withData \$ \fd@(EditUserProfileFormData fdContact fdBlurb fdlistAsC fdimagecontents)</i>
  <li>there is an IO action in the ServerPartT handler, which writes the file:
        <br><i>liftIO \$ writeFileForce avatarpath fdimagecontents</i>
  <li>enctype="multipart/form-data" in the <a href="/projectroot/templates/editconsultantprofile.st">form itself</a>
</ul>

<p>In the above, the <i>fdimagecontents</i> variable contents the entire contents of an image file. 
   writeFileForce is just a wrapper over writeFile that creates necessary parent directories if they are missing.