# if you run this, the app gets recompiled and killed
# when cron detects the process is gone and restarts, you should have the new app 
time ghc -iDataGraphInductive -isrc --make src/Main.hs -o happs-tutorial
sudo pkill -f happs-tutorial-head
