module ControllerBasic where 

import HAppS.Server
import Misc
import MiscStringTemplate
import Data.Monoid
import Control.Monad.Trans
import Text.StringTemplate 

{-
ServerPartTs take a request to a response, approximately like so
Request -> WebT -> Result -> Response

Request handlers are functions from a request to a WebT, WebT is a wrapper over a Result,
and a Response is extracted from a Result by monadic machinery which you don't need 
to understand for now.

A HAppS Server is generally a list of ServerPartTs. When the wrapped functions are run,
either they result in NoHandle or a valid response. If there's a valid response, it gets 
displayed. If NoHandle, control is passed to the next ServerPartT/handler in the list.
If all the handlers produce NoHandle as a result, the result is a 404.

ServerPartT :: (Request -> WebT m a) -> ServerPartT m a
WebT :: m (Result a) -> WebT m a
data Result a
  = NoHandle | Ok (Response -> Response) a | Escape Response

-}

-- For everything below, the m monad is always IO and a result type is always Response,
-- so nail down types with more precision, which I have found helps with debugging
-- and understanding what's going on.
type TutHandler = ServerPartT IO Response
type TutWebT = WebT IO Response


simpleHandlers :: [ServerPartT IO Response]
simpleHandlers = [

  ServerPartT $ \rq -> do
    ru  <- (return . rqURL) rq
    if ru == "/helloworld"
       then ( return . toResponse ) "hello world, this is HAppS" 
       else noHandle
      :: TutWebT
 
  -- exactdir :: (Monad m) => String -> [ServerPartT m a] -> ServerPartT m a
  -- given a url path (for the part of the path after the domain) and a list of handlers
  -- exactdir runs the handlers against the request if the request url matches the first argument.

  -- msgToSp creates a handler that will return a constant response for any request.
  -- it is useful in conjunction with exactdir and other ServerPartT constructors.

    -- argument is an exact url path.
    -- so first arg is preceded by a /.
    -- you can use exactdir "" to match the root path
    , exactdir "/exactdir-with-msgtosp" [ msgToSp "this handler uses exactdir and msgToSp. subdirectories not allowed." ]

    -- argument is a string, which matches the first element of the rqURL path.
    -- pops the rqURL array, and passes the modified request to the list of handlers.
    -- e.g., if url is http://myapp.com/dir1/dir2/dir3
    -- the first element of the rqURL path is dir1.
    -- so first arg is not preceded by a /.
    -- you cannot use dir to match the root path.
    , dir "dir-with-msgtosp" [ msgToSp "this handler uses dir and msgToSp. subdirectories are allowed." ]

  -- ServerPartTs are monoids.
  -- instance (Monad m) => Monoid (ServerPartT m a)
  -- two handlers can be glued together into one handler with mappend,
  -- and a list of handlers can be glued with mconcat, 
  -- There is a "zero" or "empty" handler which results in a 404 page for any request.
  -- The way "handler addition" works is... for  h1 `mappend` h2 ...
  -- if h1 rq is anything other than NoHandle, return that.
  -- if it is noHandle, return h2 rq
  , mappend ( exactdir "/handleraddition1" [msgToSp "handleraddition 1"] )
            ( exactdir "/handleraddition2" [msgToSp "handleraddition 2"] )

  -- more of same
  , mconcat [ mempty -- the zero handler has no effect
              , exactdir "/handleraddition3" [msgToSp "handleraddition 3"]
              , exactdir "/handleraddition4" [msgToSp "handleraddition 4"]
              , exactdir "/handleraddition5" [msgToSp "handleraddition 5"]
            ] 


  -- zero handler using mempty from the monoid instance, and the same thing again spelled out explicitly
  , exactdir "/nohandle1" [mempty] 
  , exactdir "/nohandle2" [ ServerPartT $ \rq -> WebT $ return NoHandle ] 

  , exactdir "/exactdirAndmsgToSp/anotherResponse"
                  [ msgToSp "Another response generated using exactdir and msgToSp"] 

  , (exactdir "/ioaction"
                  [ ioMsgToSp (return $ HtmlString "This is an IO value.\
                                       \It could just as easily be the result of a file read operation,\
                                       \or a database lookup." :: IO HtmlString) ] )

  , (exactdir "/ioaction2"
                  [ ioMsgToSp $ do slurp <- readFile "src/Main.hs"
                                   return $ HtmlString $ "Let's try reading the Main.hs file: .....\n" ++ slurp ])

  , (exactdir "/htmlAttemptWrong"
                  [msgToSp "first try at displaying <font color=\"red\">red formatted</font> html (wrong)"])

  , (exactdir "/htmlAttemptRight"
                  [ ( msgToSp . HtmlString ) "second attempt at displaying <font color=\"red\">red formatted</font> html (right)"])

  , (exactdir "/htmlAttemptForeignChars"
                  [ ( msgToSp . HtmlString ) $ 
                      "<html><head></head><body><font color=red>some foreign chars: ä ö ü</font></body></html>"])

  , dir "dirdemo" [ msgToSp "dir match. subpages will work" ]

  ]

-- pretty much useless little server part constructor, for demo purposes
simplematch :: String -> TutHandler
simplematch u = ServerPartT $ \rq -> do
    let ru = rqURL rq
    if ru == ("/simplematch" ++ u)
       then ( return . toResponse ) ( "matched " ++ u) 
       else noHandle :: TutWebT 

-- don't like these functions, feel they obfuscate. 
-- I wrote them when I had a less good understanding 
-- oh well, at least I don't use them in "real" code. (eg, took this out of misc.)
msgToSp :: (Monad m, ToMessage a) => a -> ServerPartT m Response
msgToSp = anyRequest . msgToWeb
msgToWeb :: (Monad m, ToMessage a) => a -> WebT m Response
msgToWeb = return . toResponse
ioMsgToSp = anyRequest .  liftIO . ( return . toResponse =<< ) 

myFavoriteAnimal :: ServerPartT IO Response
myFavoriteAnimal = 
  exactdir "/usingtemplates/my-favorite-animal"    
        [ ServerPartT $ \rq ->
            liftIO $ do templates <- directoryGroupSafer "templates"
                        let fp2 :: String
                            fp2 = renderTemplateGroup templates [("favoritePlantTwo","Venus Fly Trap")] "favoritePlant"
                            r = renderTemplateGroup templates [("favoriteAnimal", "Tyrannasaurus Rex")
                                                           , ("leastFavoriteAnimal","Bambi")
                                                    -- if you set the same template variable several times it
                                                    -- gets repeated when it gets displayed
                                                    -- I think this is reasonable, because it gives you 
                                                    -- feedback that there's probably a bug in your program
                                                           , ("favoritePlant","Ficus")
                                                           , ("favoritePlant","Wheat")
                                                           , ("favoritePlant","Sugarcane")
                                                           -- note that template variable names must be alpha
                                                           -- favoritePlant2 below would get rejected 
                                                           , ("fpTwo", fp2)]
                                                    $ "myFavoriteAnimalBase" 
                        return . toResponse . HtmlString $ r
                            -- if template key is repeated, only the first value appears
                            
        ]
  
