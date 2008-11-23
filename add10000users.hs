import Network.Download

main = mapM_ f $ [1..100]

f iter = do
  either
    (error . ("error: "++))
    (\_ -> putStrLn $ "added" ++ (show batchsize) ++ " users, iter: " ++ (show iter) )
    =<< (openURI url)    
  

url = "http://www.happstutorial.com:5002/tutorial/stresstest/atomicinsertsalljobs/" ++ (show batchsize)
batchsize = 100

