
if ! echo "$PATH" 2>/dev/null | grep -F /.prefix/bin 1>/dev/null 2>/dev/null; then
  export PATH=$HOME/bin:$HOME/.prefix/bin:$HOME/.local/bin:$HOME/.cabal/bin:$PATH
fi

export EDITOR="vim"
export BROWSER="firefox"

export WINEARCH=win64
export BD_DEBUG_MASK=0
export AWT_TOOLKIT=MToolkit
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -e $HOME/.local-profile ]; then
  source $HOME/.local-profile
fi
