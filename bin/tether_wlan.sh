#!/bin/bash
if [ "`whoami`" != "root" ]; then
    exit 1
fi
rc.d start hostapd
ifconfig wlan1 192.168.233.1
systemctl start dnsmasq
echo 1 >/proc/sys/net/ipv4/ip_forward
iptables -A INPUT -i wlan1 -j ACCEPT
iptables -A FORWARD -i wlan1 -j ACCEPT
iptables -A OUTPUT -o wlan1 -j ACCEPT
if [ "$1" == "" ]; then
  iptables -t nat -A POSTROUTING -s 192.168.233.0/24 -o eth0 -j MASQUERADE
else
  iptables -t nat -A POSTROUTING -s 192.168.233.0/24 -o $1 -j MASQUERADE
fi
