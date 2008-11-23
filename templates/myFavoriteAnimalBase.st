<html>
  $! This is a comment. !$

  <p>The next line is an included template. Look at templates/my-favorite-animal.st</p>

  $ myFavoriteAnimal() $

  <p>The next line shows another strategy for including templates, generating them
     using let bindings in haskell, rather than using parenthesis to make StringTemplate do includes.
     Note that \$ fp2 \$ would be an illegal template var name, so we use fpTwo  
     $ fpTwo $ 
</html>
