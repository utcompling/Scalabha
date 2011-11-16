#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
prefixPat=[^.]*\.

case $1 in
    kin)
        echo "tokenizing kin"
        orig=$root/kin/orig
        txt=$root/kin/txt
        tok=$root/kin/tok
        echo "scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt"
        scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt
        for collection in $txt/*; do
            mkdir -p $tok/`basename $collection`
            for file in $collection/*; do
                fileBase=`basename $file .txt`
                lang=${fileBase#$prefixPat}
                echo "normalize-text-standalone.pl -$lang < $file | tokenize-text.pl --$lang > $tok/`basename $collection`/$fileBase.tok"
                normalize-text-standalone.pl -$lang < $file | tokenize-text.pl --$lang > $tok/`basename $collection`/$fileBase.tok
            done
        done
        ;;
    mlg)
        echo "tokenizing mlg"
        scalabha run opennlp.scalabha.preproc.X2TXT -x $root/mlg/orig -t $root/mlg/txt
        ;;
    "")
        echo "tokenizing all"
        tokenize.sh kin; tokenize.sh mlg
        ;;
esac
