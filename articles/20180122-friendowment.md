# Living Life Together

About 9 months ago I was struck by an idea. For some previous days and weeks I had been ruminating on the thought that I could be doing more to help the people closest to me with their day-to-day financial struggles. My thoughts and concerns were further emphasized by the news of the day. Articles from outlets like [The Atlantic](https://www.theatlantic.com/magazine/archive/2016/05/my-secret-shame/476415/) and [Forbes](https://www.forbes.com/sites/maggiemcgrath/2016/01/06/63-of-americans-dont-have-enough-savings-to-cover-a-500-emergency/) described the unconscionable number of Americans who live paycheck-to-paycheck and are unable to address emergencies of even (what I consider to be) modest demands. And looking at the people close to me in my own life, I can see that many fit those statistics.

After just a little bit of introspection, I recall that I was there not so long ago. This isn't a challenge that's likely to be foreign to any of us for the whole of our lives; there will certainly be times when we struggle. And there will be times when each of us has excess.

## An Idea Takes Root

For some reason, the idea of a trust was raised in my consciousness. I had an inkling of an image of what a trust was all about and started doing some research. I came to rest on the concept of a "revocable trust" that could be drawn up by one individual for the benefit of a group of others. That idea percolated for some number of days and weeks. I began thinking of it as an "endowment" and pondering how a company could be organized to arrange and manage such contracts.

The name "Friendowment" hit me and I physically laughed. How stupid.

But I mentioned the name to my wife Brynn. While she may have smirked, she didn't object. As I let it sit, I decided maybe it wasn't so bad. In April, I formed Friendowment, LLC and begin reaching out to a few trusted folks to get their thoughts.

Ultimately, I decided that the trust concept was not beneficial to the mission and settled on building a financial tool that would act as a shared account to allow groups of affiliated individuals to join together to pool money for a common objective. The most obvious objective is as a rainy-day fund with (optional) controls on how much is withdrawn and how frequently. But there are many other use-cases that come to mind.

## A Cultural Challenge

One of the interesting challenges I've run up against is one of perception. When I describe the concept to almost any American, the knee-jerk response is one of restrained ridicule. Or maybe incredulousness. I'm not sure. But as the idea takes hold, people seem to come around to the idea and begin thinking about ways to make it work.

I ran a Facebook advertisement with the initial concept (really, just a brief marketing blurb at https://friendowment.us). It was a very small ad buy (about $50) but out of 1000 impressions it gathered about 50 "likes" and "loves." And only 1 "haha." I don't know what an objective measure of success is for that, but those numbers encouraged me.

Interestingly, and despite the fact that I had targeted the advertisement otherwise, folks who's location indicated they were not in the USA were over-represented. This made me a little sad. But it's not a surprise, right? Americans know that we're individualistic. I'm proposing a bit of a foreign concept. That's okay.

## The Pilot

Initially, I built a pilot application using Stripe as my ACH processor. I perceived ACH as an important component; I wanted people to be able to set recurring transfers.

I got as far as implementing the ability to transfer funds via ACH to my corporate bank account. I then began working on how to transfer funds out and discovered that I should read the fine print more closely: Stripe doesn't permit you to take funds just to hold them and move them around; they just want to take payments. I talked with their support folks (who were very helpful), but there's just no way around it right now.

Some amount of research later, I discovered that Dwolla was advertising a service that provides pretty much exactly what I need: it's white-label, allows you to create accounts for your customers, permits you to move funds between accounts, and has ACH integration. I quickly rebuilt Friendowment against Dwolla.

To do a full white-label integration, Dwolla wants a lot of money up-front. I figured I could negotiate that later, and just went about building to their Oauth implementation (they call it "Transfer API"). It isn't transparent (i.e., customers know they also have a Dwolla account and have to manage aspects of it through Dwolla) but it got me most of the way there.

Testing went well, but that Oauth integration proved to be just too terrible to stomach. I talked with Dwolla about getting their prices down for a white-label integration (their least expensive contract is for 10,000 payments per month). For a pilot, that's just silly and wasteful. Unfortunately, this seems to be a sticking point. They may have smaller plans this year (2018), but were unwilling to lower them at that time (around September 2017).

## The Blockchain

You just read that and silently went, "oh man, he's lost his dang mind" didn't you? I know, I know. It sounds crazy and I resisted it for a long time. I've long been a cryptocurrency advocate (most recently Zcash), but I've resisted thinking about it for Friendowment. I'm sensitive to the perception that it's a gimmick - I'm not interested in pursuing the fad du jour simply because it's the trend.

But the idea of smart contracts with Ethereum stuck with me. I started thinking about how I could build each "endowment" within Friendowment as a smart contract on the Ethereum blockchain. Funds could be transferred from individual Ethereum addresses to the contract address and controls could be implemented in the public blockchain and in my application. And I wouldn't have to deal with any bank-ish institutions directly (other than the obvious fact that folks would need to get their Ether from somewhere like Coinbase).

And I could fund the operations of the company through a token sale integrated with the application (e.g., endowments could be denominated partially in Philos - I came up with that name just yesterday). The tokens could be publicly traded and valued to serve as a store of value within the application and as a means to pay for the service.

So that's what I did. Over the last couple of weekends and weeks of late nights I've re-built Friendowment as an Ethereum application. The basic mechanics are all still the same as in my original pilot - everything is just denominated in ETH instead of USD. I haven't integrated any custom tokens or anything yet; everything is in ETH.

## Next Steps

Please come by and take a look. Many of the libraries I'm using are beta quality, so expect some quirks. One of the most persistent is that when transactions are made on the blockchain, the underlying library is not waiting long enough for confirmations. I've had to build asynchronous checks to be sure that data is kept up to date, so that causes some pauses and stutters. I'll eventually integrate websockets to make that less obvious (and hopefully the libraries will improve).

Thank you for reading this! I welcome your feedback, criticism, and humor. <3
