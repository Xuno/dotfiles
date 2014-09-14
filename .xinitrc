#!/bin/sh

source .profile

xrdb -load ~/.Xdefaults
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b
xsetroot -cursor_name left_ptr

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"
fcitx -r

case $1 in
    xfce4)
        exec startxfce4
        ;;
    e17)
        exec enlightenment_start
        ;;
    *)
        killall compton nitrogen xscreensaver
        nitrogen --restore &
        compton --backend glx --vsync opengl-swc --paint-on-overlay --glx-no-stencil --unredir-if-possible -b
        xscreensaver &
        exec xmonad
        ;;
esac
