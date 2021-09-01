# MacOS Setup

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

On Mac OS X, probably the fastest, cheapest way to get set up with MIDI-capable sound is with GarageBand.

Steps:

* Start up GarageBand
* Create a new empty project
* When prompted, add a new instrument track
* The default is electric piano, but you can easily switch that to any other instrument you may prefer.

## Making Sounds

Keeping GarageBand open, switch to a terminal window open to the directory where you cloned `undermidi`.

You can now follow the instructions given in the project [README.md](https://github.com/ut-proj/undermidi#api), e.g., the Max Richter example.

## Free Plugin Host for Mac: AU Lab

You can download it [here](https://www.apple.com/apple-music/apple-digital-masters/). For it to be useful, though, you will need to download AU plugins. Fortunately, most plugins are offered in this format as well as VST format.

## Almost-Free DAW

TBD
