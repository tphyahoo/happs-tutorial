# this is a workaround to a problem that my happs app dies for reasons described at
# http://code.google.com/p/happs/issues/detail?id=40
# generate the executable first by running runServer.sh
# then add this file to your crontab (via crontab -e) so you have something like 
# thartman@thartman-laptop:~>crontab -l
# * * * * *   /home/thartman/happs-tutorial/happs-tutorial.cron.sh

if [ -z "`pgrep -f happs-tutorial`" ]; 
  then cd /home/thartman/happs-tutorial
          ./happs-tutorial 80 >>happs-tutorial.cron.out 2>>happs-tutorial.cron.err
fi

if [ -z "`pgrep -f happs-tutorial-head`" ]; 
  then cd /home/thartman/happs-tutorial-head/happs-tutorial
          ./happs-tutorial-head 5002 False >>happs-tutorial.cron.out 2>>happs-tutorial.cron.err
fi


if [ -z "`pgrep -f happs-redirector`" ]; 
  then cd /home/thartman/happs-tutorial
          ./happs-redirector
fi