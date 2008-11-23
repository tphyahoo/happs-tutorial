module ControllerGetActions where

import Control.Monad
import Control.Monad.Reader
import HAppS.Server
import Data.List
import HAppS.Helpers.HtmlOutput
import qualified Data.ByteString.Char8 as B

import ControllerMisc
import StateStuff
import View
import FromDataInstances

import Misc
import qualified MiscMap as M
import MiscStringTemplate

viewConsultants :: RenderGlobals -> ServerPartT IO Response
viewConsultants rglobs = withData $ \(PaginationUrlData currB resPB currP resPP) -> [ ServerPartT $ \rq -> do
  consultants <- return . map unusername
                               =<< return . M.keys . M.filter (consultant . userprofile) . users
                                  =<< query AskDatastore
  let p = Pagination { currentbar = currB
                       , resultsPerBar = resPB
                       , currentpage = currP
                       , resultsPerPage = resPP
                       , baselink = "tutorial/consultants"
                       , paginationtitle = ""} 

      -- 1-column table
      consultantCells = map ( (:[]) . userlink ) $ consultants
      consultantTable = paintTable Nothing consultantCells (Just p)
      

      -- if not logged in, you get an invite to register as a consultant
      -- basically an incentive to register
      tmplattrs = maybe (def ++ [("registerAsConsultant","list yourself as a HAppS developer")])
                        (\_ -> def )
                        (return . sesUser =<< mbSession rglobs)
        where def = [("consultantList", consultantTable)]
  return . tutlayoutU rglobs tmplattrs $ "consultants"
  ]

viewConsultantsWanted :: RenderGlobals -> ServerPartT IO Response
viewConsultantsWanted rglobs = withData $ \(PaginationUrlData currB resPB currP resPP) -> [ ServerPartT $ \rq -> do
  consultantswanted <- return . map unusername . M.keys
                                     =<< return . M.filter (not . M.null . unjobs . jobs ) . users 
                                       =<< query AskDatastore
  let p = Pagination { currentbar = currB
                       ,resultsPerBar = resPB
                       , currentpage = currP
                       , resultsPerPage = resPP
                       , baselink = "tutorial/consultantswanted"
                       , paginationtitle = ""} 

      consultantCells = map ( (:[]) . userlink  ) $ consultantswanted
      consultantTable = paintTable Nothing consultantCells (Just p)
      
      -- an incentive to register
      tmplattrs = maybe (def ++ [("postJob","post a HAppS job")])
                        (\_ -> def )
                        (return . sesUser =<< mbSession rglobs)
        where def = [("ulist", consultantTable)]
  return . tutlayoutU rglobs tmplattrs $ "consultantswanted"
  ]

viewJobs :: RenderGlobals -> ServerPartT IO Response
viewJobs rglobs  = withData $ \(PaginationUrlData currB resPB currP resPP) -> 
  [ ServerPartT $ \rq -> do 
  rsListAllJobs <- query ListAllJobs  
  let pag = Pagination { currentbar = currB
                       , resultsPerBar = resPB
                       , currentpage = currP
                       , resultsPerPage = resPP
                       , baselink = "tutorial/jobs"
                       , paginationtitle = "Job Results: "}
      jobCells = map f rsListAllJobs
        where f (JobName j', (Job budget blurb), UserName postedBy) = let j = B.unpack j' in 
                [ joblink postedBy j
                  , B.unpack budget
                  , userlink postedBy
                ]
      paintAllJobsTable rglobs jobCells p = 
        paintTable (Just ["<b>project</b>","<b>budget</b>","<b>posted by</b>"])
                   jobCells
                   (Just p)
      jobTable = paintAllJobsTable rglobs jobCells pag
      -- if not logged in, you get invited to post a job,
      -- basically an incentive to register

      -- this next line should be coming from a template, and it's duplicated elsewhere, slightly bad.
      tmplattrs = maybe (def++[("postJob","post a HAppS job")]) (\_ -> def) (return . sesUser =<< mbSession rglobs)
        where def = [("jobTable",  jobTable)]
  return . tutlayoutU rglobs tmplattrs $ "jobs"
  ]

-- better name would be just viewEditProfile, since everyone gets a profile, not just consultants.
viewEditConsultantProfile :: RenderGlobals -> ServerPartT IO Response        
viewEditConsultantProfile rglobs = ServerPartT $ \rq -> do        
   case (return . sesUser =<< mbSession rglobs) of
    Nothing -> return . tutlayoutU rglobs [("errormsg", "error: no user")] $ "errortemplate"
    Just currU -> do      
      mbUis <- query $ GetUserInfos currU 
      case mbUis of
        Nothing -> return . tutlayoutU rglobs [("errormsg", "error: no user infos")] $ "errortemplate"
        Just uis -> do
          let cp = userprofile uis              
              

          uimage <- liftIO $ avatarimage currU 
              -- use show below to properly escape quotes
          let showPr = paintProfile rglobs (B.unpack . unusername $ currU) cp uimage
              attrs = [ ("username", B.unpack . unusername $ currU)
                        , ("userimage", uimage)         
                        , ("blurb", {-quote . -} B.unpack . blurb $ cp)
                        -- , ("jobsPosted",jobsPosted)
                        , ("contact", {-quote . -} B.unpack . contact $ cp)
                        , ("listAsConsultantChecked", checkedStringIfTrue $ consultant cp )
                        , ("profile",showPr)
                          ]
          return $ tutlayoutU rglobs attrs "editconsultantprofile"

viewEditJob :: UserName -> JobName -> RenderGlobals -> ServerPartT IO Response
viewEditJob pBy jN rglobs = ServerPartT $ \_ -> do        
    case ( return . sesUser =<< mbSession rglobs )of
      Nothing -> return $ tutlayoutU rglobs [("errormsg", "error: no user")] "errortemplate"
      Just currU -> do
        if currU /= pBy
           then return $ tutlayoutU rglobs
                  [("errormsg", "error: " ++ (B.unpack . unjobname $ jN) ++ " not posted by " ++ (B.unpack . unusername $ currU) )]
                    "errortemplate"
           else do
             mbJ <- lookupJob pBy jN 
             case mbJ of
               Nothing -> return $ tutlayoutU rglobs
                            [ ( "errormsg", "error, bad job: " ++ (show $ (pBy,jN) ) ) ] "errortemplate"
               Just j -> do let attrs = [ ("jobname", quote . B.unpack . unjobname $ jN)
                                        , ("budget", quote . B.unpack . jobbudget $ j)
                                        , ("jobblurb", quote . B.unpack . jobblurb $ j)
                                        , ("showJob",paintjob rglobs pBy (jN,j) )
                                      ]
                            return $ tutlayoutU rglobs attrs "editjob" 

lookupJob pBy jN = do
  mbUis <- ( query . GetUserInfos ) pBy 
  case mbUis of
    Nothing -> return Nothing
    Just uis -> return $ M.lookup jN $ (unjobs . jobs $ uis)

pageMyJobPosts rglobs = ServerPartT $ \rq -> do        
  let r = renderTemplateGroup (templates rglobs)
  mbUis <- getGlobsUserInfos rglobs -- (query . GetUserInfos) =<< ( return . mbUser $ rglobs )
  case (mbUis :: Either String (UserName,UserInfos)) of
    Left err -> return . tutlayoutU rglobs [("errormsg", err)] $ "errortemplate"
    Right  (currU,uis) -> do
          let jobPostsTable = paintUserJobsTable rglobs (unusername $ currU) (M.toList . unjobs . jobs $ uis) 1 20
          return $ tutlayoutU rglobs [("jobPostsTable",jobPostsTable)] "myjobposts"

getGlobsUserInfos :: Monad m => RenderGlobals -> WebT IO (m ( UserName,UserInfos) )
getGlobsUserInfos rglobs = do
  case (return . sesUser =<< mbSession rglobs) of
    Nothing -> fail "getUserInfos, no user in globals"
    Just un -> do
      mbUis <- query $ GetUserInfos un
      case mbUis of
        Nothing -> return $ fail "getUserInfos, no user infos"
        Just uis -> return $ return (un,uis)

viewJob rglobs  =
  withData $ \(JobLookup pBy jN) ->
    [ ServerPartT $ \rq -> do
        mbJ <- lookupJob pBy jN 
        case mbJ of
          Nothing -> return $ tutlayoutU rglobs [("errmsg", "no job found")] "errortemplate"
          Just j -> return $ tutlayoutU rglobs [("job",paintjob rglobs pBy (jN,j) )] "viewjob"
     
    ]

userProfile rglobs = 
  withData $ \(UserNameUrlString user) ->
    [ ServerPartT $ \rq -> do

        mbCP <- do mbUis <- query (GetUserInfos user)  
                   return $ do uis <- mbUis
                               return . userprofile $ uis

        case mbCP of
          Nothing -> return $ tutlayoutU rglobs [("errormsgProfile", "bad user: " ++ (B.unpack . unusername $ user) )] "viewconsultantprofile"
          Just cp  -> do
            userimg <- liftIO $ avatarimage user
            return $ tutlayoutU rglobs [("cp", paintProfile rglobs (B.unpack . unusername $ user) cp userimg)] 
                       "viewconsultantprofile"
    ]

-- viewEditJob :: RenderGlobals -> ServerPartT IO Response        
viewEditJobWD rglobs = withData $ \(JobLookup pBy jN) -> [viewEditJob pBy jN rglobs]
deleteJobWD rglobs = withData $ \(JobLookup pBy jN) -> [deleteJob pBy jN rglobs]

-- there's a lot of repeated code for viewEdit and Delete of jobs. 
-- maybe can consolidate
deleteJob pBy jN rglobs = ServerPartT $ \rq -> do        
    case (return . sesUser =<< mbSession rglobs) of
      Nothing -> return $ tutlayoutU rglobs [("errormsg", "error: no user")] "errortemplate"
      Just currU -> do
        if currU /= pBy
           then return $ tutlayoutU rglobs
                  [("errormsg", "error: " ++ (B.unpack . unjobname $ jN) ++ " not posted by " ++ (B.unpack . unusername $ currU) )]
                    "errortemplate"
           else do update $ DelJob currU jN
                   unServerPartT (pageMyJobPosts rglobs ) rq

