#!/usr/bin/env bash

if [ "$1" == "" ]; then
    echo expect indir
    exit 1
fi

if [ ! -d "$1" ]; then
    echo indir not exist
    exit 1
fi

indir=$1

if [ "$2" == "" ]; then
    echo expect outdir
    exit 1
fi

if [ ! -d "$2" ]; then
    echo outdir not exist
    exit 1
fi

outdir=$2

echo "$indir" -\> "$outdir"

rm -f /tmp/mkdir.sh /tmp/cover.sh /tmp/main.sh

cd "$indir"
find . -type d -fprintf /tmp/mkdir.sh "$outdir/%p\000"
find . -type f -name \*.jpg -fprintf /tmp/cover.sh "convert -background white -gravity center -scale 50x50 -extent 50x50 \"$indir/%p\" \"\`dirname \"$outdir/%p\"\`/cover.bmp\"\n"
find . -type f \( -name \*.flac -or -name \*.m4a -or -name \*.mp3 -or -name \*.wav \) -fprintf /tmp/main.sh "2aac.sh \"$indir/%p\" \"$outdir/%p\"\n"
cd -

cat /tmp/mkdir.sh | xargs -0 mkdir -p
