<h3>StringTemplate Basics</h3>

<p>In the previous section we rendered a template in ghci using the following command

<p>*Main Misc View Text.StringTemplate> do templates <- directoryGroupSafer "templates" ; writeFile "output.html" ( renderTemplateGroup templates [] "templates-dont-repeat-yourself" ) 

<p>Let's look more carefully at these functions.

<ul>
  <li>The directoryGroupSafer function reads in all *.st type files in a directory,
      and returns an IO STGroup value, which is basically a group
      of StringTemplates. It is a modification of the directoryGroup command that comes with Text.StringTemplate package,
      which ignores email backup files-type files with punctuation, that cause annoyance
      when using HStringTemplate functions. 
      <br>*Main Misc View Text.StringTemplate> :t (directoryGroupSafer :: String -> IO (STGroup String))
      <br>The actual type of directoryGroupSafer is a little less concrete than the above, and uses type classes.
      <br>Our :t command gives directoryGroupSafer a concrete type, and since there's no error, we know it typechecks.
  <li>renderTemplateGroup takes an STGroup, some template key/value pairs, and a named template, and renders
      the template if it is found in the STGroup. If it is not found, an error is returned.
      <br>*Main Misc View Text.StringTemplate> :t renderTemplateGroup
      <br>renderTemplateGroup :: STGroup String -> [(String, String)] -> String -> String
</ul>

<p>Next, let's look at a slightly more involved example of StringTemplate usage than we've seen so far.

<ul>
  <li>The controller: myFavoriteAnimal, in <a href="/src/ControllerBasic.hs">src/ControllerBasic.hs</a>
      <br>a snip: <br> &nbsp; renderTemplateGroup templates
                  <br> &nbsp; &nbsp; [("favoriteAnimal", "Tyrannasaurus Rex")
                                                           , ("leastFavoriteAnimal","Bambi")]
                                                    \$ "myFavoriteAnimalBase" 
  <li>The rendered page: <a href="/usingtemplates/my-favorite-animal">my favorite animal</a>
  <li>The template: <a href="/templates/myFavoriteAnimalBase.st">templates/myFavoriteAnimalBase.st</a>
  <li>A template included from inside an included templates:
      <a href="/templates/leastFavoriteAnimal.st">leastFavoriteAnimal()</a>
</ul>

<p> Try to gain an understanding of the most important features in the StringTemplate system
    by getting a sense of how the my-favorite-animal page got generated. </p>

<p> When you're done doing that, you should have enough StringTemplate knowledge to shoot yourself in the foot :)

<p> For a more in depth look at StringTemplate, see the following:

<ul><li><a href="http://www.stringtemplate.org/">stringtemplate.org</a>
    <li><a href="http://hackage.haskell.org/cgi-bin/hackage-scripts/package/HStringTemplate-0.2">
    HStringTemplate, a haskell port of StringTemplate on hackage</a>
    <li><a href="http://fmapfixreturn.wordpress.com/tag/hstringtemplate/">fmapfixreturn, Sterling Clover's blog</a>
</ul>

<p>And now, on to <a href="/tutorial/introduction-to-macid">macid</a>.