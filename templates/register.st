<h3>Register</h3>
    <form action="/tutorial/actions/newuser" method="post"><table>
	<tr><td>Username:</td><td><input type="textfield" name="username"/></td></tr>
	
	<tr><td>Password:</td><td><input type="password" name="password"/></td></tr>

	<tr><td>Verify Password:</td><td><input type="password" name="password2"/></td></tr>

	<tr><td><input type="submit" name="create" value="Create Account"></td></tr>
    </table></form>
    <font color=red>
      $ errormsgRegister $
    </font>
