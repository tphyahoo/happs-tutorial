<h3> Change Password </h3>

    <form action="/tutorial/changepassword" method="post">
    <table>
	<tr><td>Old Password:</td><td><input type="password" name="oldpass"/></td></tr>

	<tr><td>Password:</td><td><input type="password" name="password"/></td></tr>

	<tr><td>Verify Password:</td><td><input type="password" name="password2"/></td></tr>

	<tr><td><input type="submit" name="change_password" value="Change Password"></td></tr>
    </table>
    </form>
    <font color=red>
      $ errormsgAccountSettings $
    </font>

  $!
    <form action="/tutorial/actions/emailsettings" method="post">
    <table>
	<tr><td>Email:</td><td><input type="textfield" name="oldpass" value = $ email $ ></td></tr>
	<tr><td><input type="submit" name="change_settings" value="Change Settings"></td></tr>
    </table>
    </form>
 !$

