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

TBD

## Making Sounds

TBD

## Bonus

TBD

### VST Plugins+MIDI for Beautiful/Free Sampled Instruments

TBD

### Setting Up a VST 

TBD

### Making Beautiful Music

TBD

### Non-Free Recommendations

TBD
