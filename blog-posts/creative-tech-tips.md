---
title: Creative Tech Tips & Tricks
published: false
---

## Introduction
This post is a loose collection of tips and tricks I've learned over the years while working on interactive installations. It was inspired by my recent appearance on the "Field Notes" podcast, produced by my friend Mike Subelsky. I'd wanted to share some of these during our conversation but it went in a different way and I never got the chance. So, without further ado:

## Creative Tech Tips & Tricks

### [Remote Access](#remote-access)

It is often necessary to "remote in" to a machine running within an installation. There are countless reasons for this but the most common are: testing, maintenance, debugging and deployments. Orthogonally, there's also commonly a need to temporarily expose a machine on-site or your local development box to a client or a vendor for testing, development or demos.

There are countless solutions to this problem and this is only a short list of options I'm familiar with. Some are quick, dirty and free; others are enterprise-grade, paid solutions.

It's worth noting that this must be done with caution and you should absolutely check with stakeholders before exposing machines to the world -- even temporarily.

#### Cloudflare Warped
Many orgs are already using Cloudflare and Warped is a great way to easily and securely grant remote access to machines (e.g. ML training or inference farm). To the end user (i.e. someone who may not be overly technical), this is as simple as starting a desktop widget (Windows) or system service (e.g. systemd on Linux). You can further refine access using Cloudflare Gateway and Zero Trust policies (e.g. specific ports or client machines meets certain criteria). You can also configure ever more sophisticated topologies and create a private network which spans multiple servers and makes them available to multiple clients. So, while Warped is similar to a traditional VPN is some ways, it is also easier to setup (initially, anyways) and more flexible.

#### ngrok
Quick, easy, secure and reasonably priced. Lots of options for security from very simple (e.g. HTTP basic) to very complex (e.g. OAuth/OpenID Connect and role-based access controls).

Dead simple for clients: here's a URL and an authentication scheme.

Multiple authentication schemes to choose from: HTTP basic, OAuth, email domain filtering, etc.

Allows for fine-grained control for access to specific ports.

Supports custom subdomains (e.g. somecorp.ngrok.io).

#### Wireguard

#### Parsec
https://parsec.app/teams

#### OpenVPN
#### Raspberry Pi Connect
[This](https://www.raspberrypi.com/software/connect/) is a relatively new feature provided out-of-the-box with recent versions of Raspberry Pi OS and can be installed on older versions using aptitude. I haven't used this _yet_ but will at least try it in the future.

#### SSH Proxy Traffic
#### SSH Reverse Tunnel
#### [sshuttle](https://github.com/sshuttle/sshuttle)
From the project's GitHub page:

> Transparent proxy server that works as a poor man's VPN. Forwards over ssh. Doesn't require admin. Works with Linux and MacOS. Supports DNS tunneling.

I've used it to quickly jump onto a client's network (where I already had SSH access) when needing to communicate with multiple machines and multiple ports on each.

#### [Tailscale](https://tailscale.com/)
The NKOTB. I haven't used it yet but people seem to like tailscale quite a bit. There's also the self-hosted, open source variant: [headscale](https://headscale.net/stable/).

I believe it's similar to Wireguard with some more sophisticated topological options and authentication schemes.

#### TeamViewer
No SSH. Corpo friendly ... sometimes. Slow. Windows only. Users can stomp on each other's sessions -- I think?

Simple file sharing, screencaps, etc.


#### VNC-over-SSH
#### ZoneMinder

#### Windows Remote Desktop
Shockingly good in Windows 10 ... not sure about 11.

#### JetKVM
https://www.cnx-software.com/2025/03/21/jetkvm-a-69-kvm-over-ip-solution-with-open-source-software/

#### PiKVM
https://pikvm.org/

### [Logging](#logging)
#### Levels
Your logging library (you are using one and not just `print` ... right?) almost certainly has a configuration option and methods for various levels, like DEBUG, INFO, WARNING, ERROR, CRITICAL. Thinking through which methods are used where can go a long way towards having useful logs and preventing your exhibits from unexpectedly running out of disk space.

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

### Pipelines
#### Kestra
#### Prefect
#### Temporal

### [Provisioning](#provisioning)
#### Ansible
#### Bad USB
If you're working with Windows machines, you _may_ be able to take advantage of the [BadUSB](https://en.wikipedia.org/wiki/BadUSB) exploit to provision them using a suitable USB device (Flipper Zero, Bash Bunny, DIY build, etc.). To the chagrin of IT departments everywhere, it'll probably _just work_ but it's possible there are mitigation measures in place, depending on who initially procured and provisioned the machines.

In this scenario, you would write a batch script which does whatever provisioning is necessary and configure it as the payload for the "bad" USB device. You'd then walk around and ... just plug it into each target machine. You may need to manually modify the script for each machine (e.g. assign known static IP) but you could also use MAC addresses or similar to dynamically set values which must be unique. You could also have this batch script report back to a command-and-control server so you can track the progress of your provisioning project.

#### Docker
#### Scripts
When in doubt, just use a shell script (Bash, Batch, Powershell or otherwise). Where applicable, you should *absolutely* use '[strict mode](http://redsymbol.net/articles/unofficial-bash-strict-mode/)'.

Alternatively, you can use system Python or Ruby to round off some of the sharp corners inherent in shell scripting (i.e. no data types, tricky error handling, etc.).

### [Network Debugging](#network-debugging)
change to networking with subsections? we want to include a section for setting static ips

#### What's running on a machine's ports?
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

### Messaging
#### Long Polling!
It's best not to complicate things until you need to. If you can get away with it (i.e. your installation isn't going to DDOS itself) long-polling is a perfectly reasonable solution.

#### SQS
#### RabbitMQ
#### WebSockets
#### ZeroMQ

### [Hardware](#hardware)
#### Ethernet cable
It's good to have a spare Ethernet cable around to stand in for a bad one (this is more common than you might think!), connect to a network while waiting for Wi-Fi, etc.

#### Flipper Zero
#### Pi
#### Portable LCD monitor
Can be extremely useful when waiting for production hardware, provisioning a headless system, etc.

#### N Piece Tool Set
You never know when you might need to quickly open a piece of hardware, assemble a piece of shelving, etc.

#### Sharpie
Label all the things.

#### Label Maker
Label all the things.

#### Gaffer Tape & Sharpie
DIY label maker

#### Spare laptop and bootable USB
#### USB drive
Never underestimate the value of a [sneakernet](https://en.wikipedia.org/wiki/Sneakernet).

It's not uncommon to arrive at an install site ready to begin work only to find that the network hasn't been installed, is down for maintenance, etc. The ability to copy files between air-gapped machines in order to get the install started can be critical to adhering to the project's timeline. Sure, you could make the case that it's not your fault and go _pencils down_ until the network is ready but ... why be a jerk if you don't have to? It's better to be resourceful.


#### USB keyboard + touchpad combo
These are very inexpensive, fit in almost any bag and can be extremely useful when provisioning or debugging exhibits.

#### Ubuntu DVD

#### Wire Strippers
Handy for repairing broken electrical wiring, modifying sensors, wiring speakers, etc.

### Networking
#### Local DNS

### Tasks
#### Celery Redbeat
#### Cron
#### Lambda
#### Mac launchd
#### Systemd
#### Windows Task Scheduler

### Security
