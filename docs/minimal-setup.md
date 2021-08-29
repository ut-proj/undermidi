# Least Effort System Setup

This mini-guide is provided to help developers get `undermidi` up and running as quickly as possibly and music-ready.

## Overview

* Erlang, rebar3, undermidi, midiserver
* System MIDI
* Adding a Software MIDI Device
* Making Sounds
* Bonus:
  * VST Plugins+MIDI for Beautiful/Free Sampled Instruments
  * Setting Up a VST Host
  * Making Beautiful Music
  * Non-Free Recommendations
  
## Project Dependencies

TBD

## System MIDI

After you've downloaded `midiserver` and set the executable bit, run it with these options to see your current system MIDI devices:

```shell
$ /path/to/midiserver -list
```

At the very least -- on macos --, you should see something like this:

```
MIDI IN Ports:
	[0] IAC Driver Bus 1
MIDI OUT Ports:
	[0] IAC Driver Bus 1
```

For Mac users, if you don't see any output, then you'll want to open `Applications -> Utilities -> Audio MIDI Setup` and then from the `Window` menu, select `Show MIDI Studio`. Once that window is displayed, you'll want to check the box "Device is online" (should be under the "Model" dropdown). Once checked, re-run the `midiserver -list` command, and you should see the expected output.

## Adding a Software MIDI Device

### Garage Band

Start up GarageBand, create a new empty project, add a new instrument track when prompted, accepting all the defaults. 

## Making Sounds

TBD

## Bonus

TBD

### Plugins + MIDI for Beautiful/Free Sampled Instruments

#### Free Plugin Host for Mac: AU Lab

You can download it [here](https://www.apple.com/apple-music/apple-digital-masters/). For it to be useful, though, you will need to download AU plugins. Fortunately, most plugins are offered in this format as well as VST format.

### Plugins

A small list of free but excellent VST/AU plugins ...

* Modern synth: [Surge](https://surge-synthesizer.github.io/)
* Classic 80s synth: [OB-Xd](https://www.discodsp.com/obxd/)
* Classic 70s synth: [KORG FB-3300](https://www.fullbucket.de/music/fb3300.html)


### Making Beautiful Music

TBD

### Non-Free Recommendations

* Modern synth: [SynthMaster]()
* Classic 60s-70s synth: [Moog modular V]() (System 55 emulation)
* Sampled piano
  * bright: []()
  * dark: []()
* Reverb
  * room: []()
  * vintage: []()
