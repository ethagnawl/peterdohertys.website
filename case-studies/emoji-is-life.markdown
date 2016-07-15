---
title: Emoji is Life
---

# Introduction
I was recently tasked with building the back-end of an interactive installation whose role was to facilitate communication between the installation's component pieces. My role also ended up spilling over into development of pieces of the client applications, which will be covered below. This piece is intended to act a postmortem; I'd like to document the lessons we learned, what worked, what didn't and what to keep in mind when working on similar projects in the future.

# Overview
The installation consisted of one controller application and one-hundred and twenty client applications, each playing a video from one of two sets. The installation had a short shelf-life, but it was important that while it was running no messages were dropped and that any one of the clients could pick up where it left off using the last message it _should_ have received, if the client application crashed, the machine rebooted, etc.
