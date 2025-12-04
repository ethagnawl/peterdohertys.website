---
title: Creative Tech Tips & Tricks
id: creative-tech-tips-and-tricks
published: false
---

# Creative Tech Tips & Tricks

## Introduction
This post is a loose collection of tips and tricks I've learned over the years while working on interactive installations. It was inspired by my recent appearance on the "Field Notes" podcast, produced by [Mike Subelsky](https://www.subelsky.com/). I put together a bunch of notes that I'd wanted to share during our conversation but it went in a different direction and I didn't get the chance.

The first draft is based on those notes but I plan to keep updating this document as time goes on. If anyone reading this has tips of their own, please [let me know](/contact.html) and I'll be happy to include them, along with a credit. This post is going to have a back-end, DevOps, security and Linux bias to start, so front-end (all flavors), Mac and Windows tips are most welcome.

NOTE: This post may contain affiliate links.

<section class="remote-access">
### [Remote Access 🔗](#remote-access)

It is often necessary to "remote in" to a machine running within an installation. The most common use cases are testing, maintenance, debugging and deployments. There's also commonly a need to (temporarily) expose a machine on-site or your local development box to a client or vendor for testing, integration or demos.

There are countless solutions to this problem and this is only a short list of options I'm familiar with. Some are quick, dirty and free; others are enterprise-grade, paid solutions.

It's worth noting that this must be done with caution and you should ***absolutely*** be sure to check with stakeholders before exposing machines to the world -- even temporarily.

#### [Cloudflare Warped](https://developers.cloudflare.com/cloudflare-one/team-and-resources/devices/warp/)
Many orgs are already using Cloudflare and Warped is a great way to easily and securely grant remote access to machines (e.g. ML training or inference farm). To the end user (i.e. someone who may not be overly technical), this is as simple as starting a desktop widget (Windows) or system service (e.g. systemd on Linux). You can further refine access using Cloudflare Gateway and Zero Trust policies (e.g. specific ports or client machines meets certain criteria). You can also configure ever more sophisticated topologies and create a private network which spans multiple servers and makes them available to multiple clients. So, while Warped is similar to a traditional VPN is some ways, it is also easier to setup (initially, anyways) and more flexible.

Cloudflare is [a problematic company](https://www.wired.com/story/free-speech-issue-cloudflare/), though, and should be avoided when possible.

#### KVM Switches
Sometimes you may need lower level access to remote machines. For example, to rescue a machine after a borked update or to have the ability to power machines on or off.

- [JetKVM](https://www.cnx-software.com/2025/03/21/jetkvm-a-69-kvm-over-ip-solution-with-open-source-software/)
- [PiKVM](https://pikvm.org/)

#### [ngrok](https://ngrok.com/)
Quick, easy, secure and reasonably priced. Lots of options for security from very simple (e.g. HTTP basic) to very complex (e.g. OAuth/OpenID Connect and role-based access controls).

Dead simple for clients: here's a URL and an authentication scheme.

Multiple authentication schemes to choose from: HTTP basic, OAuth, email domain filtering, etc.

Allows for fine-grained control for access to specific ports.

Supports custom subdomains (e.g. somecorp.ngrok.io).

#### [OpenVPN](https://openvpn.net/client/)
The stalwart FLOSS VPN. May still be preferred by corporate IT departments and can be be more flexible in some scenarios (e.g. smuggling traffic over 443).

#### [Parsec](https://parsec.app/teams)
Fast and high FPS but can get expensive.

#### [Raspberry Pi Connect](https://openvpn.net/client/)
This is a relatively new feature provided out-of-the-box with recent versions of Raspberry Pi OS and can be installed on older versions using aptitude. I haven't used this _yet_ but will at least try it in the future.

#### SSH
SSH is often _good enough_ for my purposes. It's fast, secure (probably; YMMV)  and available on just about every platform. It's usually trivial to connect machines on a local network but granting remote access can sometimes be necessary and is covered by some of the other parts of this section.

A few tips for safe and secure SSH usage:

- disable root logins (e.g. `PermitRootLogin no`)
- require public key-based authentication (e.g. `PasswordAuthentication no`)
- periodically monitor your logs (e.g. /var/log/auth.log) for login attempts
- consider using intrusion prevention tools like [Fail2Ban]
- consider [using PAM to run a script that'll notify you when a login is attempted, failed, etc.](https://linuxiac.com/how-to-get-notified-on-ssh-logins-on-linux/)

#### SSH Reverse Tunnel / Remote Port Forwarding
Dead simple but requires a remote server (publicly available or otherwise available to the interested parties) to act as a bridge.

See: [https://www.digitalocean.com/community/tutorials/ssh-port-forwarding](https://www.digitalocean.com/community/tutorials/ssh-port-forwarding)

#### [sshuttle](https://github.com/sshuttle/sshuttle)
From the project's GitHub page:

> Transparent proxy server that works as a poor man's VPN. Forwards over ssh. Doesn't require admin. Works with Linux and MacOS. Supports DNS tunneling.

I've used it to quickly jump onto a client's network (where I already had SSH access) when needing to communicate with multiple machines and multiple ports on each.

#### Static IP
Your internet service provider may offer static IPs. This approach can be good in a pinch for granting public access to a network (e.g. allowing people to connect to a VPN) but it can be expensive and flaky. In my opinion, this should only be used as a last resort.

#### [Tailscale](https://tailscale.com/)
The NKOTB. I haven't used it yet but people seem to like tailscale quite a bit. There's also the self-hosted, open source variant: [headscale](https://headscale.net/stable/).

I believe it's similar to Wireguard with some more sophisticated topological options and authentication schemes.

#### [TeamViewer](https://www.teamviewer.com/en/)
No SSH access. Corpo friendly ... sometimes. Slow. ~~Windows only.~~ TeamViewer now has clients for every major platform -- including mobile. Only one client can use the machine at a time.

However, TeamViewer does offer simple file sharing, screencaps, etc. and is generally a _good enough_ solution in many cases and especially for less technical folks.

#### VNC-over-SSH
Slow and klunky but it works.

#### [Wireguard](https://www.wireguard.com/)
Open, modern, fast and simple VPN solution.

#### [ZoneMinder](https://zoneminder.com/)
A great way to keep tabs on what's happening in the physical space using commodity hardware. Be very sure to get written consent from the client and take great care to use a strong authentication scheme. You may also want to require viewers be on the VPN, their IPs are on an allow-list, etc.

#### [Windows Remote Desktop](https://support.microsoft.com/en-us/windows/how-to-use-remote-desktop-5fe128d5-8fb1-7a23-3b8a-41e636865e8c)
This feature was shockingly good out-of-the-box in Windows 10 ... not sure about 11, though.
</section>

<section class="logging">
### [Logging 🔗](#logging)
A robust and efficient logging strategy can make a world of difference when monitoring the health of an exhibit or trying to deduce what went wrong when investigating a bug report.

#### Levels
Your logging library (you are using one and not just `print` ... right?) almost certainly has a configuration option and methods for various levels, like DEBUG, INFO, WARNING, ERROR, CRITICAL. Thinking through which methods are used where can go a long way towards having useful (i.e. non-spammy) logs and preventing your exhibits from unexpectedly running out of disk space.

#### Filtering
You should carefully consider whether your system could accidentally log data containing PII or other sensitive information (e.g. API keys). For example, data gathered from guests using the interactive experience. Really, you should avoid capturing user data whenever possible, so you can prevent the possibility of a breach but that's a different story.

Most logging utilities offer some sort of filtering or callback mechanism which you can use to achieve this and if you're logging structured data (e.g. JSON) you can _probably_ replace known keys with "****" or similar (e.g. `{"email": "foo@bar.com}` => `{"email": "****"}`). _Do_ think long and hard about this, though, as it's very difficult to cover every corner case. Consider whether you can avoid logging this data altogether -- this also applies to exception handlers which may dump context containing sensitive values.

#### Performance
It's important to consider what side effects your logging might introduce. For example, when logging querysets in a Django application, you may wind up inadvertently or repeatedly evaluating them, which can result in unexpected database queries. This all varies by language/library but using log level methods (e.g. `logger.debug`) doesn't always prevent this and you should look into what options you have for lazy logging.

For example, when using the Python loguru in a Django application:

 ```
# this _only_ results in a DB query in the DEBUG logging environment
# see: https://loguru.readthedocs.io/en/stable/overview.html#lazy-evaluation-of-expensive-functions
items = Item.objects.all()
logger.opt(lazy=True).debug(
    "{msg}",
    msg=λ: f"Items {items.count()}",
)
 ```

#### [Watchtower](https://pypi.org/project/watchtower/)
This library pipes Python logs to AWS CloudWatch (in a sensible way) for warehousing, analysis, etc.

This is extremely valuable for remote monitoring of exhibits and might prevent the need for a remote access solution and the associated security concerns. When configured "correctly", it can also prevent exhibits from accidentally filling up the local disk with verbose logs or log files which aren't purged or swept. You'll also want to configure your CloudWatch log group's retention settings to delete these files after a reasonable amount of time, too, to save on storage costs and reduce the likelihood of data leaks.
</section>

<!-- <section class="pipelines"> -->
<!-- ### Pipelines -->
<!-- #### Kestra -->
<!-- #### Prefect -->
<!-- #### Temporal -->
<!-- </section> -->

<section class="provisioning">
### [Provisioning 🔗](#provisioning)
#### Ansible
Ansible is a great way to simply and predictably provision machines using a client and server strategy. A really compelling aspect of this approach is that clients don't need any dependencies beyond SSH and a modern system Python.

So, to give a concrete example, Ansible should _just work_ once OpenSSH has been installed, enabled and any firewall rules (if necessary) have been applied when exhibits are running modern versions of Ubuntu.

#### Bad USB
If you're working with Windows machines, you _may_ be able to take advantage of the [BadUSB](https://en.wikipedia.org/wiki/BadUSB) exploit to provision them using a suitable USB device (Flipper Zero, [Bash Bunny](https://amzn.to/4pjQVBL), DIY build, etc.). To the chagrin of IT departments everywhere, it'll probably _just work_ but it's possible there are mitigation measures in place, depending on who initially procured and provisioned the machines.

In this scenario, you would write a batch script which does whatever provisioning is necessary and configure it as the payload for the "bad" USB device. You'd then walk around and ... just plug it into each target machine. You may need to manually modify the script for each machine (e.g. assign known static IP) but you could also use MAC addresses or similar to dynamically set values which must be unique. You could also have this batch script report back to a command-and-control server so you can track the progress of your provisioning project.

#### Docker
In my opinion, Docker (or Podman, if you prefer) is invaluable when building interactive installations. It makes bringing new or replacement machines online almost trivial and (mostly, YMMV) reproducible. "Golden images" are certainly useful but they can be flaky, error prone (a bogus invocation of `dd` is a recipe for disaster), consume gigabytes of disk and/or bandwidth, increase the likelihood of data breaches (i.e they may contain unencrypted user data if you're not careful) and can take a long time to use when provisioning machines.

Note: I'm actually not advocating against "golden images" and I do think taking snapshots regularly is a worthwhile exercise but this does need to be done with care.

In an ideal workflow, you would have a common OS image that you use to provision each machine (e.g. latest Ubuntu LTS); an Ansible Playbook which installs Docker, OpenSSH and any other common system utilities; your Dockerfile(s) and a Compose config which handles mounting files and devices, networking and restarts.

I'm not saying anything new here but it's worth taking a step back and appreciating just how much Docker can speed up development and increase parity between development, testing, CI and production systems. I've done this sort of provisioning every which way, from cloned disk images to Bash scripts, and no approach compares in terms of reproducibility, flexibility and efficiency.

#### [UV](https://docs.astral.sh/uv/)
UV makes provisioning systems which use Python _dead simple_. Install UV and ... just run your Python program. Specify Python and package versions in the script metadata (so freaking nice for quick utility scripts) or project config file and UV pretty much handles the rest. It's truly a revelation.

#### Scripts
When in doubt, just use a shell script (Bash, Batch, Powershell or otherwise). Where applicable, you should *absolutely* use '[strict mode](http://redsymbol.net/articles/unofficial-bash-strict-mode/)'.

Alternatively, you can use system Python or Ruby to round off some of the sharp corners inherent in shell scripting (i.e. no data types, tricky error handling, etc.).
</section>

<section class="networking">
### [Networking 🔗](#networking)
#### Local DNS
It can be a bit of a pain to setup and manage but makes the install _much_ simpler on developers if they don't need to concern themselves with individual IPs.

dnsmasq is probably your best bet but bind9 is also tried and true, if more complex to setup and manage.

#### What's running on my machine's ports?
The net-tools package includes a utility called `netstat` which makes answering this question simple:

```
sudo netstat -tulpn
Active Internet connections (only servers)
Proto Recv-Q Send-Q Local Address           Foreign Address         State       PID/Program name
tcp        0      0 127.0.0.1:3306          0.0.0.0:*               LISTEN      1587/mariadbd
tcp        0      0 0.0.0.0:8000            0.0.0.0:*               LISTEN      556266/python3
```

#### What is my IP?
##### Linux
Regrettably, Ubuntu stopped shipping `ifconfig` by default in 18.04. I've been using this command so long that it's seared into my brain stem and I'm regularly frustrated when I try to use it on a modern Ubuntu install and am met with `command not found`.

`ifconfig` may or may not be included in other Debian-based distros by default but you _can_ still install via the `net-tools` package (includes many other extremely useful utilities) and it's then available via `ifconfig` if your `$PATH` contains `/sbin/` or `/sbin/ifconfig` or via `sudo ifconfig`, if not.

Alternatively, you can use the modern equivalent: `ip addr`

`ip` is **very** powerful for reading *and* writing network configs but can be a bit obtuse and it's very easy to mess up args/flags. See `man ip`

##### Mac OS
###### Shell
You _should_ still be able to use `ifconfig` on modern Mac OS releases.

###### GUI
Should be something like: System Preferences/Settings → Network → connection (e.g. Wi-Fi or Ethernet)

##### Windows
###### Powershell
`ipconfig`

###### GUI
Should be something like:

- Control Panel → Network and Sharing Center
- Click on your active connection (e.g. Wi-Fi)
- Click "Details"
- Look for "IPv4 Address"

#### nmap
With great power comes great responsibility. 🕷️

#### Can I connect to a machine?

Use `ping` like:

```
ping -c 1 10.0.0.234 | grep "1 received"
```

#### Can I connect to a port on that machine?

Use `telnet` like:
```
telnet 10.0.0.234 8000
Trying 10.0.0.234...
Connected to 10.0.0.234.
Escape character is '^]'.
```

... how about 10.0.0.234 8888?
```
telnet 10.0.0.234 8888
Trying 10.0.0.234...
telnet: Unable to connect to remote host: Connection refused
```

#### Set Static IP
Setting static IPs on exhibits and servers is _always_ a good idea, as it prevents unexpected IP reassignment from disrupting system comms. It also helps -- for the same reasons -- if you're going to use a local DNS server.

- [Mac](https://www.macinstruct.com/tutorials/how-to-set-a-static-ip-address-on-a-mac/)
- [Ubuntu](https://learnubuntu.com/set-static-ip/)
- [Windows](https://www.windowscentral.com/software-apps/windows-11/how-to-configure-a-static-ip-on-windows-10-or-11)
</section>

<section class="messaging">
### [Messaging 🔗](#messaging)
Installations (especially those with multiple exhibits or touchpoints) often have a need for the machines to exchange data, subscribe to event notifications, etc. There are many, *many* ways to approach this problem (both in the architecture _and_ implementation) and this post will just barely scratch the surface of the options available. I may revisit this topic in a future post, though, so stay tuned!

#### HTTP
A perfectly viable and dead simple strategy for intra-exhibit comms is using HTTP. This is commonly achieved using REST or GraphQL (which conveniently has support for subscriptions but YMMV). There are *an unlimited* number of ways to slice this but I've been using [FastAPI](https://fastapi.tiangolo.com/) recently to great effect. It's productive, efficient and robust (thanks, [Pydantic](https://docs.pydantic.dev/latest/)!).

#### Long Polling!
It's best not to complicate things until you need to. If you can get away with it (i.e. your installation isn't going to DDOS its back-end or components) long-polling is a perfectly reasonable approach to "subscribing" to values which change over time. Well behaved HTTP headers (e.g. content-digest) can aid in this workflow but the client can also just run a loop and compare values over time (i.e. does the latest response match the previous one?).

#### AWS SQS
SQS is an extremely powerful messaging solution (FIFO, in particular) and can be a nice, clean way to allow for comms between a local network and the outside world. There are client libraries for almost every language and this particular AWS GUI is actually quite good for testing, reviewing DLQ, etc.

#### RabbitMQ
RabbitMQ is an established and powerful messaging broker which offers advantages over simpler options, like ZeroMQ. It can be configured to persist queues and messages across client and server crashes. It also offers very sophisticated message routing using "topics" which clients can subscribe to (e.g. "events.guest.enter") or message headers which allow for basic logical filtering (e.g. AND/OR) of messages.

I used RabbitMQ to great effect on the [Fake Love - Star Wars: Red Carpet Sound-Off](/case-studies/star-wars-red-carpet-sound-off.html) project to broadcast messages from the admin application to the dozens of tablets running in the exhibit. We opted into using a durable queue and messages so that if the server or client crashed unexpectedly, all of the components could pick up either in the state where they left off or where they should be.

RabbitMQ does require running a dedicated broker process or server (unlike ZMQ) but there's a Docker image and you can also used hosted services (Amazon MQ is API-compatible) if you don't want to manage the broker yourself.

#### WebSockets
WebSockets are a great, efficient way to allow exhibits to subscribe to some source of truth for events. Historically, certain languages/frameworks, like Node, were better suited to supporting WebSockets but they're now widely supported and the addition of `async` and friends have simplified their integration into many languages.

Though, it's worth noting that support _is_ still lacking even in very popular web frameworks, like Django, which requires use of [the Channels package](https://channels.readthedocs.io/en/latest/) and a compatible (i.e. async) webserver. Contrast this to Rails which added support via [Action Cable](https://guides.rubyonrails.org/action_cable_overview.html) many years ago. Though, even Rails was _years_ behind Node in terms of support and ease-of-use.

#### ZeroMQ
ZeroMQ is a great option for intra-exhibit comms. It's an in-memory, lightweight and simple approach to messaging which doesn't require a dedicated broker server or process. It doesn't offer sophisticated routing or message durability, like RabbitMQ and others, but it is very flexible and offers sophisticated-enough routing and messaging strategies, like pub-sub (i.e. one-to-many) and push-pull (one-to-one-of-many).

To give a concrete example, I once used ZMQ in an application that was distributing raw video frames from a producer to multiple consumers which were running inference on the frames.
</section>

<section class="hardware">
### [Hardware 🔗](#hardware)
#### [Ethernet cable](https://amzn.to/49WUbhB)
It's good to have a spare Ethernet cable around to stand in for a bad one (this is more common than you might think!), connect to a network while waiting for Wi-Fi, etc.

#### [Flipper Zero](https://shop.flipperzero.one/products/flipper-zero)
The Flipper Zero is colloquially known as the hacker's multi-tool and it's a great one to have on hand for testing sensors (e.g. RFID, NFC), triggering environmental controls, red-teaming your system, etc. You can probably come up with lots of other fun use cases if you find yourself with time to kill.

#### Pi
In addition to often being capable enough to act as exhibits themselves, you'll never regret having a spare Pi on hand to stand in for a server, act as a VPN/jumpbox, test/debug sensors, etc.

The [Pi 5](https://amzn.to/4iCXBYZ) is great for all around use and, in many cases could be used as a spare exhibit server in a pinch. They're also very capable dev machines in case someone's machine is broken/lost/forgotten/etc. The [Pi Zero](https://amzn.to/3KxW0qI) is also quite capable and inexpensive enough to keep one or two in your bag.

#### [Portable LCD monitor](https://amzn.to/4pOB2D6)
These can be extremely useful when waiting for production hardware, provisioning a headless system, etc.

#### N Piece Tool Set
You never know when you might need to quickly open a piece of hardware, assemble a piece of shelving, etc.

#### Multitool
You'll never regret having a capable multitool on hand. I've had the [Leatherman Wingman](https://amzn.to/4rBli7Z) (confiscated at SFO, unfortunately -- ship to your install!) and currently use the [Victorinox Spirit](https://amzn.to/48NRptT).

#### Sharpie
Label all the things.

#### Label Maker
Label all the things.

#### Gaffer Tape & Sharpie
DIY thing labeler

#### Spare laptop and bootable USB
#### [USB drive](https://amzn.to/446Iy3X)
Never underestimate the value of a [sneakernet](https://en.wikipedia.org/wiki/Sneakernet).

It's not uncommon to arrive at an install site ready to begin work only to find that the network hasn't been installed, is down for maintenance, etc. The ability to copy files between air-gapped machines in order to get the install started can be critical to adhering to the project's timeline. Sure, you could make the case that it's not your fault and go _pencils down_ until the network is ready but ... why be a jerk if you don't have to? It's better to be resourceful.


#### [USB keyboard + touchpad combo](https://amzn.to/48UyiNR)
These are pretty inexpensive (~$25 USD), fit in almost any bag and can be *extremely* handy when provisioning or debugging exhibits.

#### Bootable Linux CD/DVD
This tip isn't as relevant as it once was, as most modern machines no longer have optical drives. However, it still can't hurt to have a bootable Ubuntu CD/DVD on hand. Though, it's worth noting that only the most minimal modern Ubuntu distributions will fit on a CD-R.

#### [Bootable Linux USB Drive](https://amzn.to/446Iy3X)
Having a bootable USB drive on hand can be a lifesaver when provisioning machines, rescuing broken machines (e.g. borked kernel update), resizing hard drive partitions, etc. These are so inexpensive and easy to create these days that there's really no excuse to _not_ have one in your bag.

#### [Wire Strippers](https://amzn.to/4pQacL0)
Handy for repairing broken electrical wiring, breadboard experimentation, modifying sensors, wiring speakers, etc.
</section>

<!-- <section class="tasks"> -->
<!-- ### Tasks -->
<!-- #### Celery Redbeat -->
<!-- #### Cron -->
<!-- #### Lambda -->
<!-- #### Mac launchd -->
<!-- #### Systemd -->
<!-- #### Windows Task Scheduler -->
<!-- </section> -->

<section class="security">
### Security
#### Physical
##### Hardware
Put hardware behind lock-and-key whenever possible. If you're not able to do so, you should try to lock any open ports -- especially [USB](https://amzn.to/4iCVWCJ).

##### Site Access
Be sure to arrange for badges, access windows, etc. before arriving on-site. It's also critical to have the phone number of a person on-site who has the ability to sign guests into the install space if no badges will be made available or they're not available for whatever reason.

This all helps ensure that everyone's time is used efficiently. I have spent _many_ hours waiting for physical access to a space during install because of assumptions made about how/when physical access would be made available. Though, hilariously, I have been able to use some of that time productively because I could still access the install space's Wi-Fi network -- so don't just sit on your hands if you are made to wait!

Also, being nice to the security, office manager, etc. on-site goes a long way towards getting access and everyone having a pleasant experience.

#### Disk/Partition Encryption
Encrypt your disks or/or disk partitions. Seriously, just do it.

Every OS now makes this possible and it's usually pretty straightforward and well documented. This will prevent anyone who walks off with one of your exhibits from accessing API keys, user data, etc.

#### Self-Signed Certs
Self-signed certs might seem like overkill and like a headache to manage but I maintain they're worth the effort. If you have any user data being captured and exchanged within the components of your exhibit, you're duty-bound to use them.

If Alice happened to find her way onto the exhibit's Wi-Fi network and started sniffing traffic, any user data exchanged between system components (e.g. email address, photos, etc.) would be hers for the taking. So, take the extra few hours to integrate them and, importantly, do it early in the project's lifecycle!

#### Password Managers
Seriously, use one!

##### AWS Secrets
##### Bitwarden
##### Hashicorp Vault
##### KeePass
Quick, dirty and free. The encrypted password database can also be stored in and distributed with Git.
</section>

<!-- <section class="support"> -->
<!-- ### Support -->
<!-- - firefighting guide -->
<!-- - phonebook -->
<!-- - screencaps -->
<!-- </section> -->

## Resources
In addition to the links provided above, here's a list of useful and related resources:

- [Blair Neal's Creative Technology Taxonomy](https://ablairneal.com/a-creative-technology-taxonomy)
