# Flower NFA Update

I've made some significant updates both to the Flower Visualization Server and to the supporting Analysis Server:

## Visualization Server

1. I implemented changes necessary to support the additional resolutions noted in the Analysis Server section. This is transparent to the end-user (the proper resolution is automatically selected).
2. New dialog boxes were added to support AD configuration.
  ![Active Directory Integration](https://ser.endipito.us/file/ad.png)
3. Many, minor aesthetic changes were made to the Network Map output.
  ![Network Map](https://ser.endipito.us/file/networkmap2.png)
4. Fonts (@font-face) are used more consistently throughout the workings of the console.
5. The most significant recent changes to the Visualization Server are in the area chart output:
  ![Histogram](https://ser.endipito.us/file/histogram-enlarged.png)
  
I changed the chart from arithmetic to logarithmic because I found that my UDP data was being pushed down to 1 pixel as it was tremendously outweighed by the TCP data. Horizontal scale lines were added to illustrate the fact that the bottom of the graph represents an amount of data many orders of magnitude less than the top of the graph. I also wrote code to insert vertical lines at the beginning of each hour, day and month to better make sense of the data. Here is a 30-day graph shown within it's containing window (data older than a few days is not pictured because I had to reset the database when I implemented the resolution concept):

![Histogram](https://ser.endipito.us/file/histogram.png)

## Analysis Server

1. In line with the changes I noted in my last update to how statistical flow data is stored, I've introduced multiple levels of summarization to greatly decrease the time it takes to complete queries for longer time durations. The levels are configurable, but presently data is stored in 10 second, 1000 second, and 10000 second resolutions.
  
  To put that in to perspective, at a resolution of 10 seconds, a query for 30 days worth of data would need to scroll through 259,200 records. That same query would need to traverse only 2,592 records at a 1,000 second resolution and only 260 records at a 10,000 second resolution.
2. I updated the chart data generation code to take advantage of the new resolution levels. Network maps represent data within a time period with little regard for how that data is distributed, so we can use a very low resolution and access the data very quickly.
  
  Area charts are more sensitive to how data is distributed throughout a time period and I've updated the code to dynamically select a resolution based on the width (in pixels) of the ultimate output and the time period selected. For example, a chart that is 600 pixels wide wouldn't be well supported by a 30 day query split into 260 intervals. The Analysis server would select a 1,000 second resolution (2,592 intervals) to provide an adequate level of granularity with an optimal query structure.
3. Microsoft Active Directory (AD) may now be used as an authentication and authorization source with minimal configuration. I've implemented code that interfaces with AD servers using LDAP/S (or optionally, and highly discouraged, over LDAP). AD servers are identified automatically by leveraging SRV records provided by the DNS server used by the Analysis Server.
  
  To enable this capability, an administrator need only specify the fully-qualified domain name of the forest root (e.g., "internal.company.com") and the group(s) to authenticate against. Each group can be specified as "privileged" to permit management of the Flower systems themselves.
  
  No AD user data is stored within the Flower systems and all communication occurs over SSL (unencrypted authentication may be selected, but that option is not exposed yet and would be highly dangerous if not handled properly). Importing the SSL certificate from AD into the GlassFish server is a little tricky, and I'll write up a wiki entry for that soon. Java is very particular about working only with trusted certificates. It is, of course, essential that it be so.

## Overall Updates

* The whole system may be configured to use SSL (from Visualization to Analysis and from Analysis to Active Directory). I have had great success using StartSSL's free certificates.
* Both the Visualization and Analysis servers may (should!) be installed behind an Apache server. I prefer this because it permits me to use Apache for SSL and mod\_security. I've found that mod\_proxy works best for the Visualization server and mod\_jk works best for the Analysis server (mod\_proxy with Analysis causes the WSDL files to be generated with "localhost" as the destination and mod\_jk seemed unstable with the Visualization server).

To try all of this out for yourself, download, compile and install the Analysis Server from the GitHub repository and head over to http://app.jdthomas.net _(Author's Note: Now defunct.)_ and log in to your new Flower Network Flow Analysis system.
