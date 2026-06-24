# my_config.el

Personal Emacs configuration, currently based on [Doom Emacs](https://github.com/doomemacs/doomemacs).

At the moment, this repository is intended to be used as Doom's private configuration directory (`$DOOMDIR`), not as a standalone Emacs package.

## What's included

- Doom module selection in `init.el`
- Personal configuration in `config.el`
- Extra package declarations in `packages.el`
- Emacs Custom settings in `custom.el`
- Example private gptel backend config in `gptel-private.el.example`

Notable packages/configured integrations include:

- `gptel`
- `copilot.el`
- `telega`
- `ement`
- `dirvish`
- `protobuf-mode`
- `capnp-mode`
- `eat`

## Installation

Install Doom Emacs first:

```sh
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
```

Then install this configuration as your Doom config directory:

```sh
git clone https://github.com/xz-dev/my_config.el ~/.config/doom
~/.config/emacs/bin/doom sync
```

Add Doom's binary directory to your shell `PATH` if desired:

```sh
export PATH="$HOME/.config/emacs/bin:$PATH"
```

After changing `init.el` or `packages.el`, run:

```sh
doom sync
```

## Private local configuration

Secrets and private endpoint metadata should not be committed.

This repository ignores:

```text
gptel-private.el
```

To configure a private gptel backend locally:

```sh
cp gptel-private.el.example gptel-private.el
```

Then edit `gptel-private.el` with your private endpoint, model, and backend name.

Store API keys in `~/.authinfo` or `~/.authinfo.gpg`, for example:

```authinfo
machine your-host.example.com login apikey password YOUR_API_KEY
```

Make sure your authinfo file is private:

```sh
chmod 600 ~/.authinfo
```

## License

MIT License. See [LICENSE](LICENSE).
