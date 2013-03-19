#!/usr/bin/env bash

if [ "$1" == "" ]; then
    echo expect infile
    exit 1
fi

if [ ! -f "$1" ]; then
    echo infile not exist
    exit 1
fi

infile=$1

if [ "$2" == "" ]; then
    echo expect outfile
    exit 1
fi

outfile=${2%.*}.m4a

if [ -f "$outfile" ]; then exit 1; fi

echo $infile -\> $outfile

WAVFILE=/dev/shm/44.1kHz.wav
AACFILE=/dev/shm/nero.m4a

rm -rf "$WAVFILE" "$AACFILE"

ffmpeg -v error -y -i "$infile" -vn -sn -filter aresample=44100 "$WAVFILE" || exit 1
neroAacEnc -2pass -br 128000 -if "$WAVFILE" -of "$AACFILE" 2> /dev/null || exit 1
ffmpeg -v error -y -i "$AACFILE" -i "$infile" -map:0 0:a:0 -c:a copy -map_metadata 1 -map_chapters 1 "$outfile" || exit 1

if [ "${infile%.*}.flac" == "$infile" ]; then

    track_gain=`metaflac --show-tag=replaygain_track_gain "$infile" | cut -d = -f 2`
    track_peak=`metaflac --show-tag=replaygain_track_peak "$infile" | cut -d = -f 2`
    album_gain=`metaflac --show-tag=replaygain_album_gain "$infile" | cut -d = -f 2`
    album_peak=`metaflac --show-tag=replaygain_album_peak "$infile" | cut -d = -f 2`


    if [ "$track_gain" != "" ] ; then
        neroAacTag "$outfile" -meta-user:replaygain_track_gain="$track_gain" 2> /dev/null
    fi

    if [ "$track_peak" != "" ] ; then
        neroAacTag "$outfile" -meta-user:replaygain_track_peak="$track_peak" 2> /dev/null
    fi

    if [ "$album_gain" != "" ] ; then
        neroAacTag "$outfile" -meta-user:replaygain_album_gain="$album_gain" 2> /dev/null
    fi

    if [ "$album_peak" != "" ] ; then
        neroAacTag "$outfile" -meta-user:replaygain_album_peak="$album_peak" 2> /dev/null
    fi

fi
