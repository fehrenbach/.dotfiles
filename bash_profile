export PATH="/home/stefan/.cabal/bin/:/home/stefan/bin/:/home/stefan/.npm-packages/bin/:$PATH"

# OPAM configuration
. /home/stefan/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
if [ -e /home/stefan/.nix-profile/etc/profile.d/nix.sh ]; then . /home/stefan/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
