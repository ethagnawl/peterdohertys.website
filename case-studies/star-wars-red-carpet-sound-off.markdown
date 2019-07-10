---
title: "Fake Love - Star Wars: Red Carpet Sound-Off"
published: true
---

# Introduction
I was recently tasked with building the back-end of an interactive installation whose role was to facilitate communication between the installation's component pieces. My role also ended up spilling over into development of pieces of the client applications, which will be covered below. This piece is intended to act a postmortem; I'd like to document the lessons we learned, what worked, what didn't and what to keep in mind when working on similar projects in the future.

# Overview
The installation consisted of one controller application and one-hundred and twenty client applications, each playing a video from one of two sets. The installation had a short shelf-life, but it was important that while it was running no messages were dropped and that any one of the clients could pick up where it left off using the last message it _should_ have received, if the client application crashed, the machine rebooted, etc.

### Producer
[openFrameworks => Ruby (via OSC) => RabbitMQ]

### Back-End/Broker
[RabbitMQ]

### Consumer(s)
[RabbitMQ => Ruby (via OSC) => openFrameworks]

# Lessons Learned/Suspicions Confirmed

### 1.) Resist the [protoduction](https://blog.codinghorror.com/new-programming-jargon/) siren's song.
At the outset of this project, I hacked together some proof-of-concept Ruby scripts that demonstrated how to create a RabbitMQ producer/consumer. The plan was to have the front-end devs "translate" these scripts to C++ when building the controller and client applications. Unfortunately, they were unable to find a compatible C++ RabbitMQ client and the decision was made to use the example scripts (with an OSC bridge to communicate with the C++ openFrameworks applications) in production. What I should have done at this point, was sit down and rewrite the scripts from scratch, starting with a failing test and using the knowledge that I'd learned from the original code spike to guide me. Not having a robust test suite hurt us. We had to modify these scripts significantly after deciding to use them as an essential piece of the installation and valuable time and insights were lost as a result of not having tests in place. Also, as they usually do, the documentation and "example" scripts fell out of sync and there was confusion and miscommunication as a result. Being able to point the front-end devs towards a working test suite would have saved time and frustration.

### 2.) Ship binaries or Docker-ized applications when possible.
Deploying the aforementioned Ruby scripts (which also had native dependencies) to Windows was unnecessarily complex. e.g. the tablets weren't connected to the Internet and we had to distribute Gems by copying Windows directories onto a thumbdrive. The rollout of the producer/consumer scripts would have gone much more smoothly if they'd either been written in a language capable of being compiled into a binary, been "bundled" up using something like [Traveling Ruby](https://github.com/phusion/traveling-ruby) or been deployed as a Docker-ized application. We briefly considered the last option, but decided the VirtualBox dependency was a deal-breaker. In retrospect, we should have at least tried installing VirtualBox on a few tablets and done some profiling to see whether or not it would have been too heavy-handed.

### 3.) Use the most recent Ubuntu Server LTS when provisioning new servers.
I had issues with getting our production server - equipped with UEFI boot - to boot from a USB drive and, in the heat of the moment, decided to drop-in a hard drive with an pre-existing Debian installation. This worked, but was risky and caused some issues (WiFi didn't work on the production machine, I had to manually disable hibernation/screensaver and the X year old hard drive could very well have failed). In retrospect, I think the boot problem may have been caused by using [Unetbootin](https://unetbootin.github.io/) to build the bootable image, as I was eventually able to get one of the ThinkPads to boot from an image created with [Rufus](https://rufus.akeo.ie/). I also didn't get around to burning a bootable cd/dvd, which may have worked. As usual, it would have been worth spending a few additional hours on figuring out a proper solution, as the workaround ended up eating up more time than the research and implementation of the correct solution would have. I'll need to do more research about UEFI, as I've never had issues installing GNU/Linux* in the past.

### 4.) Don't rely on the router to set a static IP.
Since the router used in the installation was going to be assigning the server an IP, I figured that'd be _good enough_ - turns out it wasn't. The server was randomly going offline and disrupting client communication. (I still don't know the exact cause, but I'm fairly certain that it had something to do with DHCP, as the issue was resolved by assigning the server a static IP.) Configuring a static IP on a Linux server is easy enough and, if the network will allow for it, there's no reason not to do it.

### 5.) Use `Logger` instead of `puts` when writing Ruby programs.
It's easy enough to have `Logger` redirect to `stdout` during development, if that's desired, and logging will work out of the box in production.

### 6.) Keep development and production environments as similar as possible.
Per [the Twelve-Factor Manifesto](http://12factor.net/), it's essential to _keep the development and production environments as similar as possible_. We were developing the aforementioned Ruby programs on Linux/OS X and - surprise! - we ran into issues deploying them to the production Windows environment. There were a few filepath issues, which were easily rectified, but the process monitoring library we were planning to use (which _someone on the Internet_ claimed worked on Windows...) wasn't able to get past its bootstrapping phase because Windows, apparently, doesn't support process forking. We were unable to find a suitable replacement and had to settle for a crude `begin/rescue/retry` wrapper around our application code. If we'd developed the scripts on Windows from the outset, we would have realized very quickly that our process monitoring library wasn't going to work and would have had time to make use of something like [win32-service](https://github.com/djberg96/win32-service).

### 7.) Docker is worthy of the hype that's built up around it.
Even though I'd never used [Docker](https://www.docker.com/) before, I was able to create a Dockerfile that extended the official RabbitMQ image and modify it to our needs (installing plugins, creating users, etc.), spin up a container and start doing real work in a couple of hours. Since we were deploying the server directly onto a Linux box (instead of to Mac/Windows running Docker Machine), deployment was a breeze.

### 8.) RabbitMQ and Bunny
[RabbitMQ](http://rabbitmq.com/) and the Ruby [Bunny client](https://github.com/ruby-amqp/bunny)  were both a pleasure to work with and were perfectly suited to our one-to-many communication strategy. Having never worked with either before, I found both to be well documented and intuitive. I was able to achieve our desired workflow of message production/subscription/acknowledgement/redelivery without too much head-scratching or any (as far as I know) bugs. The only issue we ran into involved subscribers randomly dropping their connection to RabbitMQ and this was solved by specifying a 10 second heart beat when initializing Bunny clients. (For some reason Bunny's [default heartbeat is 0 or no heartbeat](rubybunny.info/articles/connecting.html#using_a_map_of_parameters).)

### 9.) System Bootstrap Scripts
My [System Bootstrap Scripts](https://github.com/ethagnawl/system-bootstrap-scripts/) saved hours of configuration on the various Linux machines used in development and production. I was able to spin-up and move between machines with ease, as all of my favorite programs, aliases, configs, etc. were present and worked as expected.

### 10.) Tmux/inator
[Tmux](https://github.com/tmux/tmux) and [Tmuxinator](https://github.com/tmuxinator/tmuxinator) proved their worth once again on this project. In addition to their roles in development and testing (need to spin up a fleet of 120 TCP clients for testing purposes? [just script your Tmuxinator project config](https://github.com/tmuxinator/tmuxinator/issues/341#issuecomment-152912350)!), we created a command-center-style Tmuxinator project on the production server that was kicked-off by a startup script and allowed admins to monitor RabbitMQ, run demo producers/consumers, configure/start Ngrok, etc.

### 11.) Ngrok
[Ngrok](https://ngrok.com/) made it easy to allow admins to SSH into the production server to deal with integration issues when the project was being constructed on-site. Of course, we could also have done this using reverse tunneling, but I found Ngrok to be more user-friendly and it doesn't require you to maintain a publicly available server.

### 12.) RubyInstaller
The Windows [RubyInstaller](http://rubyinstaller.org/) was a pleasure to work with and did everything it purported to - with respect to installing particular Ruby versions and setting up the necessary paths. My only criticism is: it'd be nice if RubyInstaller bundled [DevKit](http://rubyinstaller.org/add-ons/devkit/) and the necessary version of Ruby Gems required for building native extensions. DevKit won't build native extensions without first updating Ruby Gems and the documentation wasn't very clear about this.

### 13.) OSC Ruby
The [OSC Ruby](https://github.com/aberant/osc-ruby) gem was used to facilitate communication between the native applications and their Ruby counterparts. With the exception of some issues we ran into building EventMachine on the production Windows machines, it worked wonderfully.

# Summary
Despite the issues mentioned above, the installation worked flawlessly while it was deployed and was a lot of fun to work on. I've worked on a few "physical" projects and they're a welcomed change of pace from my day-to-day work doing web/mobile development - it's surprisingly satisfying to see people interact with your software in the real-world.

## Update - 3/28/18
Inspired by this project, I recently put together a proof-of-concept RabbitMQ C client which could have been used in this project's front-end applicaton to pull messages directly from RabbitMQ and would have allowed us to circumvent the Ruby/OSC Rube Goldberg machine mentioned above. The client/demo can be found here: [rabbitmq-c-client-demo](https://github.com/ethagnawl/rabbitmq-c-client-demo)

# Collaborators
- [\@caitlinmorris](https://github.com/caitlinmorris)
- [\@jedahan](https://github.com/jedahan)
- [\@laserpilot](https://github.com/laserpilot)

# Resources
- [Bunny](https://github.com/ruby-amqp/bunny)
- [DevKit/gem update](https://github.com/rubygems/rubygems/issues/977#issuecomment-54642706)
- [Digital Ocean](http://digitalocean.com/)
- [Docker](http://www.docker.com/)
- [Install RabbitMQ via Docker on OS X](https://coderwall.com/p/uqp34w/install-rabbitmq-via-docker-in-os-x)
- [Ngrok](https://ngrok.com/)
- [RabbitMQ](http://rabbitmq.com/)
- [RubyInstaller](http://rubyinstaller.org/)
- [Ruby OSC](https://github.com/aberant/osc-ruby)
- [Tmux](https://github.com/tmux/tmux)
- [Tmuxinator](https://github.com/tmuxinator/tmuxinator)

*"Although it will make some people unhappy, I’m nonetheless referring to GNU/Linux by the far more commonly used name — just plain “Linux” — after the first reference. For more on this issue, Wikipedians have compiled a host of relevant sources." - [@dangillmor](https://twitter.com/dangillmor)
