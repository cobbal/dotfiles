# ZSH Theme

local bg_color="$BG[009]"

local MACHINE_COOKIE="$( (
  set -e
  echo "$(ifconfig | grep -o '..:..:..:..:..:..' | head -n 1)-$(uname -s)" | openssl sha1 | tr a-f A-F | grep -Eo '[0-9A-F]{40}'
) 2>/dev/null )"
case "$MACHINE_COOKIE" in
    (686F22F212EF7D09BB659EF12208E8C7B4C5928B|0C399532FC55EA27CB568618459FA97E491B9EE3|AD6D8474DFA7DA3EBCB883447D9FF54D4E74C881) # romulus
        bg_color="$BG[237]"
        ;;
    (43D263C1B1E056867ADA0B63ACB4E7042B0B9F07) # cirrus
        bg_color="$BG[022]"
        ;;
    (BD57C2529C519948B9849A4C795F6A03A284F9AE|8BBF4CAAD5EE6EEEFCDB563AD0437B865514474C) # vmware
        bg_color="$BG[130]"
        ;;
    (9C363C1A31800282F4F8B8F16B0CB6DDEAFC979E) # osx-vm
        bg_color="$BG[058]"
        ;;
    (E4C342046B59FA82EEEE5EAA96BBFE4601CC6C60) # remus
        bg_color="$BG[052]"
        ;;
    (55733EBCC769AFA29573A7CB93101B010CBA2081|46D23A97D5B7786D0CF908D1256877895475C61A|204583658F4D874DBBF8201BB704984A81E634B6) # romulus (linux)
        bg_color="$BG[062]"
        ;;
    (41B53843D830C04F1D8C6A073EB9B87889EE9F86) # rpi
        bg_color="$BG[017]"
        ;;
    (D2380ADE2A7ABCEC41651E66C17DC0F5009BAB88) # work
        bg_color="$BG[062]"
        ;;
esac

local prompt_reset="$reset_color$bg_color$FG[231]$FX[bold]"
local return_code="%{$reset_color%}%(?..%{$BG[009]%} %? ↵ %{$reset_color%}"$'\n)'

PROMPT='${return_code}%{$prompt_reset%} %2c %{$prompt_reset$FG[046]%}%(!.#.») %{$reset_color%} '
if (( SHLVL > 2 )); then
    RPROMPT="[$(( SHLVL - 2 ))]"
else
    RPROMPT=''
fi
# RPROMPT='%{$reset_color$bg_color$FG[231]%} [%*] %{$reset_color%}'
