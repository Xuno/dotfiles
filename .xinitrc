#!/bin/sh

source ~/.profile

xrdb -load ~/.Xdefaults
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b
xset +dpms dpms 7200 7200 7200
xsetroot -cursor_name left_ptr

# Load the newest one generated by arandr automatically
XRANDR="$HOME/.screenlayout/$(ls -t ~/.screenlayout/ | head -n 1)"
if [ -s "$XRANDR" ]; then
    sh "$XRANDR"
fi

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

if [ -s ~/.local-xprofile ]; then
    sh ~/.local-xprofile
fi

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx -r

killall --user $USER compton xscreensaver
compton --backend glx --paint-on-overlay --glx-no-stencil --unredir-if-possible -b
xscreensaver &

nitrogen --restore

exec xmonad
