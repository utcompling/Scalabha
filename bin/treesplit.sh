#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
mkdir -p /tmp/treesplit
rm -rf /tmp/treesplit/*
pushd /tmp/treesplit

for langDir in $root/*
do
    lang=$( basename $langDir )
    for collDir in $langDir/parsed/*
    do
        coll=$( basename $collDir )
        for filePath in $collDir/*
        do
            file=$( basename $filePath )
            prefix=$( basename $file .tree )
            split -d -l1 $filePath $prefix.
            dest=$langDir/tree/src/$coll/$prefix/
            mkdir -p $dest
            for i in *
            do
                mv $i $dest/$i.tree
            done
        done
    done
done

popd
