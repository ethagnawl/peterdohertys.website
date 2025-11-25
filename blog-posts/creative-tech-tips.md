---
title: Creative Tech Tips & Tricks
published: false
---

## Introduction
This post is a loose collection of tips and tricks I've learned over the years while working on interactive installations. It was inspired by my recent appearance on the "Field Notes" podcast, produced by my friend Mike Subelsky. I'd wanted to share some of these during our conversation but it went in a different way and I never got the chance. So, without further ado:

## Tips & Tricks

### Remote Access
Why is this useful?

#### Cloudflare Warped
Many orgs are already using Cloudflare and Warped is a great way to easily and securely grant remote access to machines (e.g. ML training or inference farm). To the end user (i.e. someone who may not be overly technical), this is as simple as starting a desktop widget (Windows) or system service (e.g. systemd on Linux). You can further refine access using Cloudflare Gateway and Zero Trust policies (e.g. specific ports or client machines meets certain criteria). You can also configure ever more sophisticated topologies and create a private network which spans multiple servers and makes them available to multiple clients. So, while Warped is similar to a traditional VPN is some ways, it is also easier to setup (initially, anyways) and more flexible.

#### ngrok
Quick, easy, secure and reasonably priced. Lots of options for security from very simple (e.g. HTTP basic) to very complex (e.g. OAuth/OpenID Connect and role-based access controls).

Dead simple for clients: here's a URL and an authentication scheme.

Multiple authentication schemes to choose from: HTTP basic, OAuth, email domain filtering, etc.

Allows for fine-grained control for access to specific ports.

Supports custom subdomains (e.g. somecorp.ngrok.io).

#### Wireguard
#### OpenVPN
#### Raspberry Pi Connect
[This](https://www.raspberrypi.com/software/connect/) is a relatively new feature provided out-of-the-box with recent versions of Raspberry Pi OS and can be installed on older versions using aptitude. I haven't used this _yet_ but will at least try it in the future.

#### SSH Proxy Traffic
#### SSH Reverse Tunnel
#### sshuttle
#### Tailscale
The NKOTB. I haven't used it yet but people seem to like [tailscale](https://tailscale.com/) quite a bit. There's also the self-hosted, open source variant: [headscale](https://headscale.net/stable/).

I believe it's similar to Wireguard with some more sophisticated topological options and authentication schemes.

#### TeamViewer
No SSH. Corpo friendly ... sometimes. Slow. Windows only.

Simple filesharing, screencaps, etc.


#### VNC-over-SSH
#### ZoneMinder

#### Windows Remote Desktop
Shockingly good in Windows 10 ... not sure about 11.

#### JetKVM
https://www.cnx-software.com/2025/03/21/jetkvm-a-69-kvm-over-ip-solution-with-open-source-software/

#### PiKVM
https://pikvm.org/

### Logging
#### Watchtower

### Pipelines
#### Kestra
#### Prefect
#### Temporal

### Provisioning
#### Ansible
#### Bad USB
#### Docker
#### Scripts
When in doubt, just use a shell script (Bash, Batch, Powershell or otherwise). Where applicable, you should *absolutely* use '[strict mode](http://redsymbol.net/articles/unofficial-bash-strict-mode/)'.

Alternatively, you can use system Python or Ruby to round off some of the sharp corners inherent in shell scripting (i.e. no data types, tricky error handling, etc.).

### Debugging
#### nmap
With great power comes great responsibility.

#### ping
Can I connect to 10.0.0.234 at all?

```
ping -c 1 10.0.0.234 | grep "1 received"
```

#### telnet
Can I connect to 10.0.0.234 8000?
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
#### SQS
#### RabbitMQ
#### ZeroMQ

### Hardware
#### Ethernet cable
#### Flipper Zero
#### Gaffer tape
#### Pi
#### Portable LCD monitor
#### Sharpie
#### Spare laptop and bootable USB
#### USB drive
#### USB keyboard + touchpad combo
#### Ubuntu DVD

### Networking
#### Local DNS

### Tasks
#### Celery Redbeat
#### Cron
#### Lambda
#### Mac launchd
#### Systemd
#### Windows Task Scheduler
