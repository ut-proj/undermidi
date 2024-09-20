# MacOS Setup

## System MIDI

After following the instructions in the project README file, you should be able to see the system MIDI devices with:

``` lisp
lfe> (undermidi:list-devices)
```

If you don't see any output, then you'll want to open `Applications -> Utilities -> Audio MIDI Setup` and then from the `Window` menu, select `Show MIDI Studio`. Once that window is displayed, you'll want to check the box "Device is online" (should be under the "Model" dropdown). Once checked, re-run the function above, and you should see the expected output.

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
