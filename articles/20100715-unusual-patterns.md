# Flow Patterns

I’ve been noticing some interesting patterns using my Flower Analysis tool.

![unusualpatterns](https://ser.endipito.us/files/unusualpatterns.png)

For whatever reason, I’ve noticed that one of my servers is continually chatting (in 3.2 – 4MB increments) with a whole lot of servers at Google. These chats generally occur over port 80/tcp. I first noticed the traffic in an area chart that showed a spike pattern in a very consistent rhythm. I tracked down the destination addresses of the flows and determined that they belong to networks owned by Google. I then added those networks to my map.

I’ll be doing more research on this; I’m at a loss as to why this one server would be sending (or receiving) so much information to (or from) Google.

I suspect espionage.
