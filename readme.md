## About
1. [Themelios](https://github.com/a-schaefers/themelios) is built into a NixOS
[livedisk](https://github.com/a-schaefers/nix-config/blob/master/iso/myrescueiso.nix)
and bootstraps machine-specific installations, using the corresponding machine-name/configuration.sh
and machine-name/default.nix files from within the
[hosts/ directory](https://github.com/a-schaefers/nix-config/tree/master/hosts).

2. [NixOS](https://nixos.org/) with
[lib/recimport.nix](https://github.com/a-schaefers/nix-config/blob/master/lib/recimport.nix)
auto-imports _everything_ in the
[modules/ directory](https://github.com/a-schaefers/nix-config/tree/master/modules). The
corresponding hosts/machine-name/default.nix file determines which modules should be enabled.

## Install
- My laptop:
```bash
[root@nixos:~] themelios latitude a-schaefers/nix-config
```

- My workstation:
```bash
[root@nixos:~] themelios hpz a-schaefers/nix-config
```

## Credits
- Everyone in #nixos and #emacs on freenode, for always helping me out.
