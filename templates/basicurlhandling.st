<h3>Basic Request Handling in HAppS</h3>

<p>One of the most basic functions of a web framework is to give you a way of controlling what happens when a web browser makes an http request.</p>

<p>$! Before explaining the theory of request handling in HAppS, !$ Let's look at some simple examples.</p>

<!-- who cares about MVC?... <p>In the <a href="#">FIX LINK... MVC paradigm</a> employed by Ruby on Rails and other popular frameworks, this is the task of the Controller. </p> -->

<ol>
  <li> Read the source code of the simpleHandlers function in <a href="/src/ControllerBasic.hs">ControllerBasic.hs</a> to see how urls are handled in HAppS. <b>Pay attention to the comments!</b>
  <li>Follow the urls below by clicking on them. Match what happens when you click on a link with the code in <a href="/src/ControllerBasic.hs">ControllerBasic.hs</a></li>
</ol>

<ul>
  <li>introduction to handlers: <a href="/helloworld">hello world</a></li>

  <li>exactdir and msgToSp (subdirectories of the path argument do not match):
      <a href="/exactdir-with-msgtosp">exactdir-with-msgtosp</a> ... 
      <a href="/exactdir-with-msgtosp/subdir">exactdir-with-msgtosp/subdir</a> ... 

  </li>
  <li>introduction to dir and msgToSp -- subdirectories *do* match:
      <a href="/dir-with-msgtosp">dir-with-msgtosp</a> ...
      <a href="/dir-with-msgtosp/subdir">dir-with-msgtosp/subdir</a>
  </li>
  <li>gluing handlers together / handlers as monoids: 
      <a href="/handleraddition1">handleraddition1</a> ... 
      <a href="/handleraddition2">handleraddition2</a> ...
      <a href="/handleraddition3">handleraddition3</a> ...
      <a href="/handleraddition4">handleraddition4</a> ...
      <a href="/handleraddition5">handleraddition5</a>
   </li>

  <li>the "empty" handler:
        <a href="/nohandle1">nohandle1</a> ...
	<a href="/nohandle2">nohandle2</a>
  </li>

  <li>IO in the response: <a href="/ioaction">IO Response</a> ... 
      <a href="/ioaction2">Another IO Response</a
  </li>
  <li>Formatted html: <a href="/htmlAttemptWrong">First attempt at formatted html (wrong)</a> </li>
  <li><a href="/htmlAttemptRight">Second attempt at formatted html (right)</a></li>
  <li><a href="/htmlAttemptForeignChars">Foreign characters in formatted html</a></li>

  <li>Serving static files: <a href="/templates/base.st">Using dir and fileserve "templates", we can view templates</a> ... 
      <a href="/templates/basic-url-handling.st">The template that was used to generate this page</a>
  </li>

</ul>   

<p>The static file serving example above hints at the templating system used by this tutorial to put together web pages behind the scenes.</p>

<p>We learn about <a href="/tutorial/templates-dont-repeat-yourself">using templates with HAppS</a> next.</p>