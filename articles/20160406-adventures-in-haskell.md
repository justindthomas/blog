# Thinking Functionally

I've decided that I need to make a real effort to understand functional programming.

I've written a lot of Java, Python, Javascript and Ruby. I'm comfortable with object-oriented frameworks, but have very little experience with functional languages. I've dabbled a little in Scala (that language was heavily used by the backend team of the last employer I worked with) but I never fully understood it or the underlying concepts.

With Haskell, I feel like I'm jumping in to the deep end without the object-oriented water wings that Scala allows. It's exhilirating and a little infuriating. Thankfully, `#haskell` and `#snapframework` exist on Freenode and are populated by very obliging and patient personalities.

I'll try to capture some of my more useful discoveries and realizations here in the hope that it may help others get started with Haskell.

## Tools

There are a few different methods of obtaining the Haskell compiler and basic framework. My platforms of choice are Mac OS X and Linux, so my choices here are constrained by that preference.

In general, it seems that knowledgeable folks eschew the Haskell Platform installation packages in favor of The Haskell Tool Stack. Using `brew`, stack can be installed as you would expect: `brew install haskell-stack`.

I had a lot of problems initially using Haskell Platform, specifically around library version conflicts. I would often see worrying errors about updates potentially rendering my compiler inoperable. Switching to stack has helped a lot with that.

For Ubuntu, I discovered [Halcyon](https://halcyon.sh/). I've integrated that tool with my deployment mechanisms and am happy with the results. I use Chef to automate my deploys and can pull in projects from remote repositories relatively easily with Halcyon.

_A disclaimer about the script below: the install step relies on remote code execution from the official Halcyon site. That's generally a Bad Idea&trade; and not something I recommend. It would be better to pull that script down locally and verify each step. I haven't gotten around to that yet, but it's a bit of technical debt that I need to retire soon._

~~~~ {.ruby}
bash 'install_halcyon' do
  cwd '/srv'
  environment 'LC_ALL' => 'en_US.UTF-8'
  code <<-EOH
    eval "$( curl -sL https://github.com/mietek/halcyon/raw/master/setup.sh )"
    /app/halcyon/halcyon build
    /app/halcyon/halcyon paths >/app/halcyon/halcyon-env
    EOH
  action :nothing
  notifies :create, 'template[/etc/custom-rc.d/halcyon]'
end

node[:deploys].each do |name, url|
  git "/srv/#{name}" do
    repository url
    revision 'master'
    action :nothing
    subscribes :checkout, 'bash[install_halcyon]'
    notifies :run, "bash[install_#{name}]"
  end

  bash "install_#{name}" do
    environment 'LC_ALL' => 'en_US.UTF-8'
    cwd "/srv/#{name}"
    code <<-EOH
      source /app/halcyon/halcyon-env
      /app/halcyon/halcyon install
      EOH
    action :nothing
    notifies :run, 'execute[reboot]', :delayed
  end
end
~~~~

It's unfortunate that Halcyon is not readily available on Mac OS X; there's a file unique to Halcyon that must be kept updated (`constraints`) that I overlook often when testing with just `stack` on my Mac.

## Web Frameworks

The easiest way for me to learn a new tool is to force myself to use it on a regular basis. Toward that end, I decided that building this website and using it to publish occasional articles would be a convenient way to accomplish that goal.

There are at least three popular web frameworks for Haskell: Servant, Snap and Yesod. After a relatively cursory review of the options, It seemed to me that Servant is too sparse and Yesod too opinionated for my taste. Snap seemed reasonably well-featured while still requiring the user to understand a bit about Haskell.

After a week or so of fighting with Snap, I nearly gave up. I was having a terrifically awful time trying to just get splices to work. I had been successful in getting a basic site running and in getting PostgreSQL integration working, but couldn't find a way to get data to show up in my templates. I made a desperate appeal for help in `#snapframework` and was pointed at namespaces as the likely culprit. A helpful person gave me this configuration snippet to turn off namespacing (I want to re-enable it once I get my mind wrapped around what's going on, but this works for now).

~~~~ {.haskell}
let hc = emptyHeistConfig
         & hcNamespace .~ ""
         & hcTemplateLocations .~ [loadTemplates "templates"]
         & hcLoadTimeSplices .~ defaultLoadTimeSplices
         & hcCompiledSplices .~ splices
~~~~

## More Learning

There's much more for me to learn - I haven't even begun to really understand what monads are all about. I'll probably post haskell-y stuff here occasionally and may toss un/loosely-related things up here as well (music, security opinions, etc.)
