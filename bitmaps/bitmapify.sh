#!/bin/sh

for f in $@ ; do
    ff=$(echo $f | sed -se 's/\.png$/.bmp/')
    convert -monochrome -negate -colors 2 -depth 1 "$f" "$ff"
done
