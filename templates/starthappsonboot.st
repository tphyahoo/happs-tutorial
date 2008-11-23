<h3>Start HAppS Automatically At Boot Time</h3>

<p>What happens if your HAppS deployment server experiences a power outage?</p>

<p>Or what if the HAppS process just dies for
<a href="http://code.google.com/p/happs/issues/detail?id=40">mysterious reasons?</a>

<p>The way I deal with this both these issues with a public-facing happs application is to have a cron job that runs every minute, that will start the happs application if it isn't running.</p>

<p>
  thartman@thartman-laptop:~/happs-tutorial>crontab -l
<br>* * * * *   /home/thartman/happs-tutorial/happs-tutorial.cron.sh
<br>
<br>thartman@thartman-laptop:~/happs-tutorial>cat happs-tutorial.cron.sh
<br># this is a workaround to a problem that my happs app dies for reasons described at
<br># http://code.google.com/p/happs/issues/detail?id=40
<br># generate the executable first by running runServer.sh
<br># then add this file to your crontab so you have something like
<br># thartman@thartman-laptop:~>crontab -l
<br># * * * * *   /home/thartman/happs-tutorial/happs-tutorial.cron.sh
<br>
<br>if [ -z "`pgrep happs-tutorial`" ];
<br>  then cd ~/happs-tutorial
<br>          ./happs-tutorial >happs-tutorial.cron.out 2>happs-tutorial.cron.err
<br>fi
</p>