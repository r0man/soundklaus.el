# soundklaus.el

Play [SoundCloud](https://soundcloud.com) music in [Emacs](http://www.gnu.org/software/emacs/) via [EMMS](http://www.gnu.org/software/emms).

![](http://imgs.xkcd.com/comics/techno.png)

## Configuration

This mode requires a working [EMMS](http://www.gnu.org/software/emms) setup for Emacs. Follow the EMMS [Quick-Start Guide](http://www.gnu.org/software/emms/quickstart.html) to configure EMMS.

## Authentication

You need to authorize *soundklaus.el* to access your SoundCloud account. Your web browser should open Emacs via `emacsclient` for the [OAuth2](http://oauth.net/2) callback URL  `soundklaus://oauth/callback`. On a Linux system you can configure this by adding the following content to the  `~/.local/share/applications/soundklaus.desktop` file in your home directory.

	[Desktop Entry]
	Name=SoundKlaus
	Exec=emacsclient %u
	Icon=emacs-icon
	Type=Application
	Terminal=false
	MimeType=x-scheme-handler/soundklaus;

Now you can start the [OAuth2](http://oauth.net/2) authentication dance with `M-x soundklaus-connect`.

## License

Copyright © 2014 r0man

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.
