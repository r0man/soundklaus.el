# soundklaus.el [![Build Status](https://travis-ci.org/r0man/soundklaus.el.png)](https://travis-ci.org/r0man/soundklaus.el)

Play [SoundCloud](https://soundcloud.com) music in [Emacs](http://www.gnu.org/software/emacs/) via [EMMS](http://www.gnu.org/software/emms).

![](http://imgs.xkcd.com/comics/techno.png)

## Prerequisites

This mode requires [Emacs](http://www.gnu.org/software/emacs/) 24. Other required dependencies are [curl](http://curl.haxx.se) to stream tracks, [mp3info](http://ibiblio.org/mp3info) to tag them, and a music player that is supported by *EMMS*. [Music Player Daemon](http://www.musicpd.org) is known to work out of the box, [VLC Media Player](http://www.videolan.org) after some tweaking.

## Installation

*soundklaus.el* is available on [MELPA](http://melpa.milkbox.net). Instructions on how to configure *MELPA* can be found [here](http://melpa.milkbox.net/#/getting-started). After configuring *MELPA* you can install *soundklaus.el* with the following command:

`M-x package-install [RET] soundklaus [RET]`

## Configuration

This mode requires a working [EMMS](http://www.gnu.org/software/emms) setup for Emacs. The following code from the *EMMS* [Quick-Start Guide](http://www.gnu.org/software/emms/quickstart.html) should work with some minor tweaks.

``` emacs-lisp
(require 'emms-setup)
(emms-standard)
(emms-default-players)
```

### Music Player Daemon

If you are using the [Music Player Daemon](http://www.musicpd.org) everything should work out of the box after adding the player to the list of *EMMS* players.

``` emacs-lisp
(require 'emms-player-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
```

### VLC Media Player

The default configuration of the [VLC Media Player](http://www.videolan.org) that ships with *EMMS* doesn't support HTTPS. This can be fixed by changing the regular expression of the player.

``` emacs-lisp
(require 'emms-player-vlc)
(define-emms-simple-player vlc '(file url)
  (concat "\\`\\(https?\\|mms\\)://\\|"
	  (emms-player-simple-regexp
	   "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
	   "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
	   "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"))
  "vlc" "--intf=rc")
```

### MPlayer

[MPlayer](http://www.mplayerhq.hu) can't follow redirect links when playing streams. This is currently required by *soundklaus.el*, so *MPlayer* is not supported at the moment.


## Usage

You can search songs with `M-x soundklaus-tracks` and playlists with `M-x soundklaus-playlists`. Your own tracks and playlists are available with `M-x soundklaus-my-tracks` and `M-x soundklaus-my-playlists`. In the `*soundklaus*` buffer you can move to the next song with `C-n` or `n`, and to the previous one with `C-p` or `p`. Pressing `RET` plays the current song, and `a` adds the current song at point to the *EMMS* playlist. All other available key bindings can be seen with `M-x describe-minor-mode [RET] soundklaus-mode`.

## Authentication

Some commands like `M-x soundklaus-activities` or `M-x soundklaus-my-tracks` need access to your *SoundCloud* account. Your web browser should open Emacs via `emacsclient` for the [OAuth2](http://oauth.net/2) callback URL `soundklaus://oauth/callback`. 

On a Linux system you can configure this by running `M-x soundklaus-desktop-entry-save`. This will write the following content to the `~/.local/share/applications/soundklaus.desktop` file in your home directory.


```
[Desktop Entry]
Name=SoundKlaus
Exec=emacsclient %u
Icon=emacs-icon
Type=Application
Terminal=false
MimeType=x-scheme-handler/soundklaus;
```

To setup this functionality on other operating systems take a look at the [system setup](http://orgmode.org/worg/org-contrib/org-protocol.html#sec-3) section in [org-protocol.el](http://orgmode.org/worg/org-contrib/org-protocol.html) and adapt it for *soundklaus.el*.

In detail what should be done is this:

- Make sure emacs runs as a server or as a deamon (because we use emacsclient for this)
- Run these commands (some gnome utils may be necessary for them). Assuming emacs is in /usr/bin/emacsclient in anycase, replace with whatever `$ which emacs` gives you.
```
$ gconftool-2 -s /desktop/gnome/url-handlers/soundklaus/command '/usr/bin/emacsclient %s' --type String
$ gconftool-2 -s /desktop/gnome/url-handlers/soundklaus/enabled --type Boolean true
```
- Use firefox for this and add a new protocol handler with [http://kb.mozillazine.org/Register_protocol#Linux](this guide on registering new protocols). Remember to change foo with soundklaus
- Add this line to /usr/share/applications/defaults.list
```
x-scheme-handler/soundklaus=soundklaus.desktop
```

Now you can start the [OAuth2](http://oauth.net/2) authentication dance with `M-x soundklaus-connect`. You should get redirected to *SoundCloud* and allow *soundklaus.el* to access your account. After pressing the `Connect` button on the *SoundCloud* page the browser should open Emacs and set the `soundklaus-access-token` customization variable. You should save and load this variable from a safe place for future sessions.

## Screenshot

![](https://raw.githubusercontent.com/r0man/soundklaus.el/master/screenshot.jpg)

## License

Copyright © 2014 r0man

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
