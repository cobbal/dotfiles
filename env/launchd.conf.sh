#!/bin/zsh

case ${1} in
    (launchctl)
        varset() {
            printf "launchctl setenv %s %s" "${1}" "${2}"
            launchctl setenv "${1}" "${2}"
        }
        varunset() {
            printf "launchctl unsetenv %s" "${1}"
            launchctl unsetenv "${1}"
        }
        ;;
    (export|exec)
        varset() {
            export ${${1}}=${2}
        }
        varunset() {
            unset ${${1}}
        }
        ;;
    (*)
        echo "usage: $0 launchctl"
        echo "       $0 export"
        echo "       $0 exec <command> ..."
        varset() {
        }
        varunset() {
        }
        ;;
esac

function {
    local nix_link="$HOME/.nix-profile"

    local OPAM="${HOME}/.opam/4.06.1"

    local lpath
    lpath=()
    lpath+="/run/current-system/sw/bin"
    # lpath+="${nix_link}/bin"
    # lpath+="${nix_link}/sbin"
    # lpath+="/nix/var/nix/profiles/default/bin"
    # lpath+="/nix/var/nix/profiles/default/sbin"
    lpath+="${HOME}/.ghcup/bin"
    lpath+="${HOME}/bin"
    lpath+="${HOME}/.local/bin"
    lpath+="${HOME}/.swiftly/bin"
    lpath+="${HOME}/.config/emacs/bin"
    lpath+="/usr/local/bin"
    lpath+="/opt/homebrew/bin"
    lpath+="/usr/local/sbin"
    lpath+="/usr/bin"
    lpath+="/bin"
    lpath+="/usr/sbin"
    lpath+="/sbin"
    lpath+="/Library/TeX/texbin"
    lpath+="/Applications/Mathematica.app/Contents/MacOS"
    lpath+="/Applications/LilyPond.app/Contents/Resources/bin"
    # lpath+="/Applications/VMware Fusion.app/Contents/Library"
    lpath+="/Applications/Racket/bin"
    lpath+="${HOME}/Applications/Skim.app/Contents/SharedSupport"
    lpath+="${HOME}/.gem/ruby/latest/bin"
    lpath+="${HOME}/Library/Python/3.7/bin"
    lpath+="${HOME}/Library/Python/3.9/bin"
    lpath+="${HOME}/Library/Python/2.7/bin"
    lpath+="${HOME}/Library/Android/sdk/platform-tools"
    lpath+="${HOME}/go/bin"
    lpath+="${HOME}/.mint/bin"
    lpath+="${HOME}/.dotnet/tools"
    lpath+="${HOME}/.rd/bin"
    lpath+="${HOME}/app-support/JetBrains/Toolbox/scripts"
    lpath+="/opt/homebrew/opt/dotnet@6/bin"

    lpath+="${OPAM}/bin"
    lpath+="${HOME}/.cargo/bin"
    lpath+="${HOME}/.perl5/bin"
    lpath+="${HOME}/Desktop/programs/esp/xtensa-esp32-elf/bin"
    lpath+="${HOME}/.swiftenv/shims"
    varset PATH ${(j.:.)lpath}
    # typeset -U path

    varunset MANPATH

    local lperl5lib
    lperl5lib=()
    lperl5lib+="${OPAM}/lib/perl5"
    lperl5lib+="${HOME}/.perl5/lib/perl5"
    varset PERL5LIB ${(j.:.)lperl5lib}

    # varset PYTHONSTARTUP "${HOME}/.python"
    # varset PYTHONPATH "/usr/local/lib/python"
    varset ANDROID_SDK_ROOT "${HOME}/Library/Android/sdk"

    varset JYTHON_HOME "/usr/local/Library/LinkedKegs/jython/libexec"
    varset NODE_PATH "/usr/local/lib/node:/usr/local/lib/node_modules"
    varset GOPATH "${HOME}/go"
    varset HOMEBREW_EDITOR "${HOME}/bin/cemacs"
    varset HOMEBREW_NO_ANALYTICS 1
    varset HOMEBREW_CASK_OPTS "--appdir=${HOME}/Applications"
    varset DOTNET_ROOT "/opt/homebrew/opt/dotnet/libexec"
    varset SWIFTLY_HOME_DIR "${HOME}/.swiftly"
    varset SWIFTLY_BIN_DIR "${HOME}/.swiftly/bin"

    # EC2 stuff
    # varset JAVA_HOME "$(/usr/libexec/java_home)"
    # varset EC2_PRIVATE_KEY "$(/bin/ls ${HOME}/.ec2/pk-*.pem | /usr/bin/head -1)"
    # varset EC2_CERT "$(/bin/ls ${HOME}/.ec2/cert-*.pem | /usr/bin/head -1)"
    # varset AWS_CREDENTIAL_FILE "${HOME}/.ec2/credentials"
    # varset EC2_AMITOOL_HOME "/usr/local/Library/LinkedKegs/ec2-ami-tools/libexec"
    # varset EC2_HOME "/usr/local/Library/LinkedKegs/ec2-api-tools/libexec"
    # varset AWS_SNS_HOME "/usr/local/Library/LinkedKegs/aws-sns-cli/jars"

    #export CLICOLOR=1
    #export LSCOLORS=ExFxCxDxBxegedabagacad

    varset EDITOR "vim"

    # OPAM
    varset OCAML_TOPLEVEL_PATH "${OPAM}/lib/toplevel"
    varset OPAMUTF8MSGS "1"
    # varset CAML_LD_LIBRARY_PATH "${HOME}/.opam/system/lib/stublibs:/usr/local/lib/ocaml/stublibs"
    varset CAML_LD_LIBRARY_PATH "${OPAM}/lib/stublibs"

    #CPAN
    varset PERL_LOCAL_LIB_ROOT "${HOME}/.perl5"
    varset PERL_MB_OPT "--install_base \"${HOME}/.perl5\""
    varset PERL_MM_OPT "INSTALL_BASE=${HOME}/.perl5"

    # nvm
    varset NVM_DIR "$HOME/.nvm"

    # rust, or the autocomplete for it
    varset RUST_SRC_PATH "$HOME/.multirust/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src"

    # nix stuff
    # varset NIX_PATH "nixpkgs=${HOME}/.nix-defexpr/channels/nixpkgs"
    # varset NIX_PATH "nixpkgs=${HOME}/programs/nixpkgs"
    # local nixpath
    # nixpath=()
    # nixpath+="darwin-config=$HOME/.nixpkgs/darwin-configuration.nix"
    # nixpath+="/nix/var/nix/profiles/per-user/root/channels"
    # nixpath+="${HOME}/.nix-defexpr/channels"
    # varset NIX_PATH ${(j.:.)nixpath}

    # varset NIX_SSL_CERT_FILE "${HOME}/.nix-profile/etc/ssl/certs/ca-bundle.crt"
    # varset SSL_CERT_FILE "${HOME}/.nix-profile/etc/ssl/certs/ca-bundle.crt"
    # varset CURL_CA_BUNDLE "${HOME}/.nix-profile/etc/ssl/certs/ca-bundle.crt"
    # varset SSL_CERT_FILE "/nix/store/kllhf4y7haqindwzrsx8hwv4ji41mpns-nss-cacert-3.26/etc/ssl/certs/ca-bundle.crt"
    # varset CURL_CA_BUNDLE "/nix/store/kllhf4y7haqindwzrsx8hwv4ji41mpns-nss-cacert-3.26/etc/ssl/certs/ca-bundle.crt"
    # varset NIX_BUILD_CORES "8"

    # SUPER HACKY NIX GHC STUFF
    # varset NIX_CFLAGS_COMPILE "-idirafter /usr/include -F/System/Library/Frameworks"
    # varset NIX_CFLAGS_LINK "-L/usr/lib"

    # SUPER HACKY NIX XCODE STUFF
    # varset NIX_LDFLAGS_AFTER "-L/usr/lib -F/Library/Frameworks -F/System/Library/Frameworks"
    # varset NIX_ENFORCE_PURITY ""

    varset IDF_PATH "${HOME}/Desktop/programs/esp/esp-idf"
}

unfunction varset
unfunction varunset

case ${1} in
    (exec)
        shift
        exec "$@"
        ;;
    (*)
        ;;
esac
