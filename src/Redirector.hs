-- with gracious thanks to mightybyte:
-- http://softwaresimply.blogspot.com/2008_02_01_archive.html

module Main where
import HAppS.Server
import HAppS.State
import Misc
import System.Environment



main = simpleHTTP (Conf {port=5001}) $ [ ServerPartT $ \rq -> seeOther "http://happstutorial.com" (toResponse "") ]
