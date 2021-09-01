# Linux Setup

## System MIDI

After you've downloaded `midiserver` and set the executable bit, run it with these options to see your current system MIDI devices:

```shell
$ /path/to/midiserver list-devices
```

On Ubuntu-based distributions, you will see something like this:

```text
MIDI IN devices:
	[0] Midi Through:Midi Through Port-0 14:0
MIDI OUT devices:
	[0] Midi Through:Midi Through Port-0 14:0
```

In and off themselves, these aren't useful; you'll need to add a MIDI device that _is_ useful, though. In Linux, software MIDI devices register with the OS, so we just need to start up the MIDI software of our choosing in order to get a useful device (and corresponding device ID).


## Using Software MIDI Devices

### Yoshimi

The first software MIDI device we'll look at is Yoshimi: it's a simple software synth that's easy to use, easy to get a sound out of. Install with the following:

```
$ sudo apt-get install -y yoshimi
```

Once installed, run it, and then list your devices again:

```shell
$ /path/to/midiserver list-devices
```
```text
MIDI IN devices:
	[0] Midi Through:Midi Through Port-0 14:0
MIDI OUT devices:
	[0] Midi Through:Midi Through Port-0 14:0
	[1] yoshimi:input 128:0
```

The synth has the system device ID of 1, so we can test it on MIDI channel 0 with the following:

```shell
$ /path/to/midiserver example 1 0
```

We'll look at a more sophisticated synth in a bit, but next let's play the `midiserver` example with a sampled grand piano ...

### Qsynth and Soundfonts

[sites.google.com/site/soundfonts4u](https://sites.google.com/site/soundfonts4u/)

[Steinway-v3.8](https://drive.google.com/file/d/17Zqi3CcLcxgkJMRjkinJfBY74MjJhRnx/view?usp=sharing)

```shell
$ sudo apt-get install -y qsynth
```

```shell
$ /path/to/midiserver list-devices
```

``` shell
MIDI IN devices:
	[0] Midi Through:Midi Through Port-0 14:0
MIDI OUT devices:
	[0] Midi Through:Midi Through Port-0 14:0
	[1] yoshimi:input 128:0
	[2] FLUID Synth (16999):Synth input port (16999:0) 129:0
```

Load soundfont

```shell
$ /path/to/midiserver example 2 3
```

### XXX

If you want to be generating diverse sounds via software synthesis, then XXX is a good choice.

```shell
$ sudo apt-get install -y XXX
```
