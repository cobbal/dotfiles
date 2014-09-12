# ZSH Theme

local bg_color="$BG[000]"
local prompt_reset="$reset_color$bg_color$FG[231]$FX[bold]"
local return_code="%{$reset_color%}%(?..%{$BG[009]%} %? ↵"$' \n)'

PROMPT='${return_code}%{$prompt_reset%} %2c %{$prompt_reset$FG[046]%}%(!.#.») %{$reset_color%} '
RPROMPT='%{$reset_color$bg_color$FG[231]%} [%*] %{$reset_color%}'

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
