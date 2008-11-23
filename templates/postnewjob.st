$! <h3>Post New Job</h3> !$
    <form action="/tutorial/postnewjob" method="post">
    <table>
	<tr><td>Title:</td><td><input type="textfield" rows=5 name="jobtitle" value = $jobtitle$ ></td></tr>
	<tr><td>Budget:</td><td><input type="textfield" name="jobbudget" value=$budget$ ></td></tr>
	<tr><td>Description:</td><td><input type="textfield" name="jobdescription" value=$jobdescription$ ></td></tr>

	<tr><td></td>
            <td><input type="submit" name="editjobpostsaction" value="post new job">
	        $! <input type="submit" name="editconsultantaction" value="preview">
                <input type="submit" name="editconsultantaction" value="cancel"> !$
            </td>
            <td></td>
	</tr>
    </table>
    </form>