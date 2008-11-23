<h3>Edit Job</h3>
    <form action="/tutorial/editjob" method="post">
    <input type="hidden" name="jobtitle" value=$jobname$ >
    <table>
	<tr><td>Budget:</td><td><input type="textfield" name="jobbudget" value=$budget$ ></td></tr>

	<tr><td>Description:</td><td><input type="textfield" name="jobdescription" value=$jobblurb$ ></td></tr>

	<tr><td></td>
            <td><input type="submit" name="submitbtn" value="submit">
	        $! <input type="submit" name="editconsultantaction" value="preview"> !$
                $! <input type="submit" name="submitbtn" value="delete consultant profile"> !$
            </td>
            <td></td>
	</tr>
    </table>
    </form>
<hr>
$ showJob $