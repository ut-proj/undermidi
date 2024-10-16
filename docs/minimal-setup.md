# Least Effort System Setup

This mini-guide is provided to help developers get `undermidi` up and running as quickly as possibly and music-ready.

## Overview

* git, Erlang, rebar3, undermidi
* System MIDI
* Adding a Software MIDI Device
* Making Sounds
* Bonus:
  * VST Plugins+MIDI for Beautiful/Free Sampled Instruments
  * Setting Up a VST Host
  * Making Beautiful Music
  * Non-Free Recommendations
  
## Project Dependencies

The installation of git, Erlang, and rebar3 won't be covered here -- lots of great resources for those, all easily searchable. Right now, the best way to work with undermidi is to simply clone it:

``` shell
$ git clone git@github.com:ut-proj/undermidi.git
$ cd undermidi
```

A `Makefile` target is provided to download the Erlang Ports/Go `midiserver` to the location expected by `undermidi`:

``` shell
TBD
```

Once that's done, you have the software dependencies in place and are ready setup MIDI for your OS.

## System MIDI

Setting up MIDI is different for every operating system (and sometimes distro, too). See the following:

* MacOS
* Linux

In addition to system setup, those will also provide instructions for setting up plugins, soundfonts, DAWs, etc.
