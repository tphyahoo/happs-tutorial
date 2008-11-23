<div id="login">
    <form action="/tutorial/actions/login" method="post">
      <table>
	<tr><td>Username:</td> <td><input type="textfield" name="username"/></td> </tr>
 	
	<tr> <td>Password:</td> <td><input type="password" name="password"/></td> </tr>

	<tr> <td> <input type="submit" name="create" value="Log in"> </td>
	     <td> <a href="/tutorial/register">register </a></td>
	</tr>
      </table>
    </form>
    <font color=red>
      $ loginerrormsg $
    </font>


</div>
