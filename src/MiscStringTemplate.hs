module MiscStringTemplate (
  module Text.StringTemplate
  , directoryGroupSafer
  , renderTemplateGroup
)

where

import Text.StringTemplate hiding (directoryGroup)
import System.Directory
import System.FilePath
import Control.Applicative
import Data.List (find)
import Data.Char


-- Chooses a template from an STGroup, or errors if not found.
-- Renders that template uses attrs, and gives the string.
-- if you don't clean and a template k/v pair is repeated, it appears twice.
-- Possibly this should be a fix inside StringTemplate. Tell sclv?
-- what is the expected StringTemplate behavior according to the original program?
--clean = nubBy (\(a1,b1) (a2,b2) -> a1 == a2) . sortBy (\(a1,b1) (a2,b2) -> a1 `compare` a2)
-- but then again, why should a key be repeated twice? maybe showing a repeat is a good thing
-- as it indicates buggy behavior
-- The ToSElem type is probably either String or [String]
--renderTemplateGroup :: (ToSElem a) => STGroup String -> [(String, a)] -> [Char] -> String
renderTemplateGroup :: STGroup String -> [(String, String)] -> [Char] -> String
renderTemplateGroup gr attrs tmpl = 
       maybe ( "template not found: " ++ tmpl )
             ( toString . setManyAttribSafer attrs )
             ( getStringTemplate tmpl gr )

directoryGroupSafer :: (Stringable a) => FilePath -> IO (STGroup a)
directoryGroupSafer path = groupStringTemplates <$>
                      (fmap <$> zip . (map dropExtension)
                       <*> mapM (newSTMP <$$> (readFile . (path </>)))
                           =<< mapM checkTmplName
                           =<< return . filter (not . or . map (=='#') {-naughty emacs backup character-} )
                                        . filter ( (".st" ==) . takeExtension )
                           =<< getDirectoryContents path)
  where checkTmplName t = if  ( badTmplVarName . takeBaseName ) t
                            then fail $ "safeDirectoryGroup, bad template name: " ++ t
                            else return t

setManyAttribSafer attrs  st = 
    let mbFoundbadattr = find badTmplVarName . map fst $ attrs
    in  maybe (setManyAttrib attrs st)
              (\mbA -> newSTMP . ("setManyAttribSafer, bad template atr: "++) $ mbA)
              mbFoundbadattr

(<$$>) :: (Functor f1, Functor f) => (a -> b) -> f (f1 a) -> f (f1 b)
(<$$>) x y = ((<$>) . (<$>)) x y


badTmplVarName t = or . map (not . isAlpha) $ t