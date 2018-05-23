#!/usr/bin/env bash

echo "# ChangeLog"
echo
for tt in $(git tag --sort=-version:refname)
do
    echo "## [$(git tag $tt -l)] - $(git log -1 --format="%ai" $tt | cut -d ' ' -f 1)"
    echo
    git tag -n999 -l $tt | sed '1 s/^[0-9]\(\.[0-9]\)* *//' | sed 's/^    //g'
    echo
done
