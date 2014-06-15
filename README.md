# soundklaus.el [![Build Status](https://travis-ci.org/r0man/soundklaus.el.png)](https://travis-ci.org/r0man/soundklaus.el)

Play [SoundCloud](https://soundcloud.com) music in [Emacs](http://www.gnu.org/software/emacs/) via [EMMS](http://www.gnu.org/software/emms).

![](http://imgs.xkcd.com/comics/techno.png)

## Prerequisites

This mode requires [Emacs](http://www.gnu.org/software/emacs/) 24. Optional dependencies are [curl](http://curl.haxx.se) and [mp3info](http://ibiblio.org/mp3info).

## Installation

*soundklaus.el* is available on [MELPA](http://melpa.milkbox.net).

You can install `soundklaus.el` with the following command:

`M-x package-install [RET] soundklaus [RET]`

## Configuration

This mode requires a working [EMMS](http://www.gnu.org/software/emms) setup for Emacs. The following code from the *EMMS* [Quick-Start Guide](http://www.gnu.org/software/emms/quickstart.html) should work with some minor tweaks.

``` emacs-lisp
(require 'emms-setup)
(emms-standard)
(emms-default-players)
```

### [Music Player Daemon](http://www.musicpd.org)

If you are using the Music Player Daemon (MPD) everything should work out of the box after adding the player to the list of *EMMS* players.

``` emacs-lisp
(require 'emms-player-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd)
```

### [VLC Media Player](http://www.videolan.org)

The default configuration of *VLC* that ships with *EMMS* doesn't support HTTPS. This can be fixed by changing the regular expression of the player.

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

## Usage

You can search songs with `M-x soundklaus-tracks` and playlists with `M-x soundklaus-playlists`. In the `*soundklaus*` buffer you can move to the next song with `C-n` or `n`, and to the previous one with `C-p` or `p`. Pressing `RET` plays the current song, and `a` adds the current song at point to the *EMMS* playlist.

## Authentication

Some commands like `M-x soundklaus-activities` or `M-x soundklaus-my-tracks` need access to your *SoundCloud* account. Your web browser should open Emacs via `emacsclient` for the [OAuth2](http://oauth.net/2) callback URL  `soundklaus://oauth/callback`. On a Linux system you can configure this by adding the following content to the  `~/.local/share/applications/soundklaus.desktop` file in your home directory.

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

Now you can start the [OAuth2](http://oauth.net/2) authentication dance with `M-x soundklaus-connect`. You should get redirected to *SoundCloud* and allow *soundklaus.el* to access your account. After pressing the `Connect` button on the *SoundCloud* page the browser should open Emacs and set the `soundklaus-access-token` customization variable. You should save and load this variable from a safe place for future sessions.

## Screenshot

![](https://raw.githubusercontent.com/r0man/soundklaus.el/master/screenshot.jpg)

## License

Copyright © 2014 r0man

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
