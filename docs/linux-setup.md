# Linux Setup

## System MIDI

You will need to have the libasound2 header files installed, since this is used by the Erlang NIF for Linux support:

``` shell
$ sudo apt-get install -y libasound2-dev
```

Additionally, there are some tools and libaries that will be good to have for verious tasks. Those may be installed with the following:

``` shell
$ sudo apt-get install -y \
	alsa-utils \
	libasound2 \
	libasound2-plugins \
	pulseaudio \
	pulseaudio-utils \
	--no-install-recommends \
```

Building the undermidi project is done per the instructions in the project README (the same for all supported operating systems).

## Using Software MIDI Devices

### Yoshimi

The first software MIDI device we'll look at is Yoshimi: it's a simple software synth that's easy to use, easy to get a sound out of. Install with the following:

```
$ sudo apt-get install -y yoshimi
```

Once installed, run it, and then list your devices again:

```lisp
lfe> (undermidi:list-devices)
```

### Qsynth and Soundfonts

[sites.google.com/site/soundfonts4u](https://sites.google.com/site/soundfonts4u/)

[Steinway-v3.8](https://drive.google.com/file/d/17Zqi3CcLcxgkJMRjkinJfBY74MjJhRnx/view?usp=sharing)

```shell
$ sudo apt-get install -y qsynth
```

Once installed, run it, and then list your devices again:

```lisp
lfe> (undermidi:list-devices)
```

Load soundfont:

TBD

