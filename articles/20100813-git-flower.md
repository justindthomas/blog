# Git Repository Available

I’ve published the Analysis Server code out to GitHub.com. Instructions for building and deploying the server from source (using Ant – NetBeans is not required) are included on the Wiki.
This is a screenshot of the Visualization interface as currently available at https://app.jdthomas.net:

![earlymap](https://ser.endipito.us/file/earlymap.png)

I’ve made some fundamental changes to how data is stored within the Analysis Server (see StatisticsManager.java on GitHub for details). Instead of building the charts off of raw NetFlow data, those flows are now normalized according to time increment (set at a 5 second interval currently). This allows me to query based on a time specified time frame without worrying about how to handle the start and stop times that might fall outside of that range. It also sets up a scenario where we can derive some interesting statistical information from the data; more on that to come soon, hopefully.

I’ll update the EC2 AMI soon and remove the old image in favor of the new. I’m planning to also add Apache and mod_security to that server to protect the web service better for anyone who chooses to use that service.
