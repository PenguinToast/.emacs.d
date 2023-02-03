export TERM=xterm-256color

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI with non-daemon as alternate

alias emax="emacsclient -t"                      # used to be "emacs -nw"
alias semac="sudo emacsclient -t"                # used to be "sudo emacs -nw"
alias emacsc="emacsclient -c -a emacs"           # new - opens the GUI with

alias tmux="tmux -2"

# tmux history config
export HISTSIZE=100000
export HISTFILESIZE=$HISTSIZE

if [[ -n "${TMUX}" ]]; then
    mkdir -p "$HOME/.tmux/history"
    tmux_pane=$(tmux display-message -t $TMUX_PANE -p  "#I_#P")
    export HISTFILE="$HOME/.tmux/history/pane_${tmux_pane}.hist"

    if [[ -n "${PROMPT_COMMAND}" ]]; then
        export PROMPT_COMMAND="$PROMPT_COMMAND; history -a"
    else
        export PROMPT_COMMAND="history -a"
    fi

    history -c
    history -r
fi

if [ -f ~/.ssh/agent.env ] ; then
    . ~/.ssh/agent.env > /dev/null
    if ! kill -0 $SSH_AGENT_PID > /dev/null 2>&1; then
        echo "Stale agent file found. Spawning a new agent. "
        eval `ssh-agent | tee ~/.ssh/agent.env`
        ssh-add
    fi
else
    echo "Starting ssh-agent"
    eval `ssh-agent | tee ~/.ssh/agent.env`
    ssh-add
fi
