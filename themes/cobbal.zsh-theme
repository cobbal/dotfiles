# ZSH Theme

local bg_color="$BG[009]"

local MACHINE_COOKIE="$((
  set -e
  echo "$(ifconfig | grep -o '..:..:..:..:..:..' | head -n 1)-$(uname -s)" | openssl sha1 | tr a-f A-F | grep -Eo '[0-9A-F]{40}'
) 2>/dev/null )"
case "$MACHINE_COOKIE" in
    (686F22F212EF7D09BB659EF12208E8C7B4C5928B) # romulus
        bg_color="$BG[237]"
        ;;
    (E2F17D3518FE11380D190E3C69D124FF52DCE70A) # cirrus
        bg_color="$BG[022]"
        ;;
    (BD57C2529C519948B9849A4C795F6A03A284F9AE) # vmware
        bg_color="$BG[130]"
        ;;
    (9C363C1A31800282F4F8B8F16B0CB6DDEAFC979E) # osx-vm
        bg_color="$BG[058]"
        ;;
    (E4C342046B59FA82EEEE5EAA96BBFE4601CC6C60) # remus
        bg_color="$BG[052]"
        ;;
    (46D23A97D5B7786D0CF908D1256877895475C61A) # romulus (linux)
        bg_color="$BG[062]"
        ;;
    (41B53843D830C04F1D8C6A073EB9B87889EE9F86) # rpi
        bg_color="$BG[017]"
        ;;
esac

local prompt_reset="$reset_color$bg_color$FG[231]$FX[bold]"
local return_code="%{$reset_color%}%(?..%{$BG[009]%} %? ↵"$' \n)'

PROMPT='${return_code}%{$prompt_reset%} %2c %{$prompt_reset$FG[046]%}%(!.#.») %{$reset_color%} '
# RPROMPT='%{$reset_color$bg_color$FG[231]%} [%*] %{$reset_color%}'

ZSH_THEME_GIT_PROMPT_ADDED="%{$FG[082]%}✚%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$FG[166]%}✹%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_DELETED="%{$FG[160]%}✖%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_RENAMED="%{$FG[220]%}➜%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$FG[082]%}═%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$FG[190]%}✭%{$prompt_reset%}"

ZSH_THEME_GIT_PROMPT_PREFIX=" %{$fg[green]%}±%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$prompt_reset%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="⚡"
