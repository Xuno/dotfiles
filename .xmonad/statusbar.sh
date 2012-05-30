#!/bin/bash

#Layout
BAR_H=12
BIGBAR_W=65
HEIGHT=16

IFS='|'

CRIT="#99cc66"
BAR_FG="#3475aa"
DZEN_FG="#cccccc"
DZEN_FG2="#999999"
DZEN_BG="#333333"
COLOR_SEP=$DZEN_FG2

CPULoad0=0
CPULoad1=0
CPUTemp=0

VOL_MUTE_CMD="amixer sset Master toggle"
MPD_TOGGLE_CMD="ncmpcpp toggle"

printVolInfo() {
	Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
	Mute=$(amixer get Master | grep "Mono:" | awk '{print $6}')
	echo -n "^fg($DZEN_FG2) ^ca(1,$VOL_MUTE_CMD)VOL^ca() "
	if [[ $Mute == "[off]" ]]; then
		echo -n "$(echo $Perc | gdbar -fg $CRIT -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()off"
	else
		echo -n "$(echo $Perc | gdbar -fg $BAR_FG -h $BAR_H -w $BIGBAR_W -s o -ss 1 -sw 2 -nonl) "
		echo -n "^fg()${Perc}%"
	fi
	return
}

printCPUInfo() {
	[[ $CPULoad0 -gt 70 ]] && CPULoad0="^fg($CRIT)$CPULoad0^fg()"
	[[ $CPULoad1 -gt 70 ]] && CPULoad1="^fg($CRIT)$CPULoad1^fg()"
	echo -n "^fg($DZEN_FG2)CPU ^fg($BAR_FG)${CPULoad0}%^fg($DZEN_FG2)/^fg($BAR_FG)${CPULoad1}%"
	return
}

printTempInfo() {
	CPUTemp=$(acpi --thermal | head -n 1 | awk '{print substr($4,0,2)}')
	[[ $CPUTemp -gt 70 ]] && CPUTemp="^fg($CRIT)$CPUTemp^fg()"
	echo -n "^fg($DZEN_FG2)TEMP ^fg($BAR_FG)${CPUTemp}°"
	return
}

printMemInfo() {
	[[ $MemPerc -gt 70 ]] && MemPerc="^fg($CRIT)$MemPerc^fg()"
	echo -n "^fg($DZEN_FG2)MEM ^fg($BAR_FG)${MemPerc}%"
	return
}

printMpdInfo() {
	MPDON=$(ps -A | grep -c mpd)
	if [[ $MPDON == "0" ]]; then
		echo -n "^fg($DZEN_FG2)^ca(1,mpd)MPD^ca() ^fg()Off"
	else
		echo -n "^fg($DZEN_FG2)^ca(1,$MPD_TOGGLE_CMD)MPD^ca() $MpdInfo"
	fi
	return
}

printS() {
	echo -n " ^fg($COLOR_SEP)|^fg() "
	return
}

printDateInfo() {
    echo -n "^fg()$(date '+%Y^fg(#777).^fg()%m^fg(#777).^fg()%d^fg(#777)/^fg()%a | %H^fg(#777):^fg()%M^fg(#777):^fg()%S')"
	return
}

printBar() {
    while true; do
        read CPULoad0 CPULoad1 CPUFreq MemUsed MemPerc MpdInfo
        printVolInfo
        printS
        printCPUInfo
        printS
        printTempInfo
        printS
        printMemInfo
        printS
        printMpdInfo
        echo -n "^p(_RIGHT)^p(-230)"
        printDateInfo
        echo
    done
}

conky -c `dirname $0`/conkyrc | printBar | dzen2 -fn 'WenQuanYi Micro Hei Mono-10' -h $HEIGHT -ta l -fg $DZEN_FG -bg $DZEN_BG -y 784 -xs 1
