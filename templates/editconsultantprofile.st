<h3>Edit Profile</h3>
    <form action="/tutorial/editconsultantprofile" enctype="multipart/form-data" method="post">
    <table>
	<tr><td>About Me:</td><td colspan=2><textarea name="consultantblurb" rows=6 cols=64>$blurb$</textarea></td>
	</tr>

	<tr><td>Contact:</td><td colspan=2><textarea name="contact" rows=3 cols=64>$contact$</textarea></td></tr>

        <tr><td>List me on the HAppS developers page:</td>
	    <td colspan=2><input type="checkbox" name="listasconsultant" value="listasconsultant" $listAsConsultantChecked$ ></td></tr> 
        
        <tr><td>Upload picture:</td> 
             <td><input type="file" name="imagecontents">
             <td>$userimage$</td>
            </td>
        </tr>
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
$ profile $