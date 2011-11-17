#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
prefixPat=[^.]*\.
debug=0

case $1 in
    kin|mlg)
        echo "tokenizing $1"
        orig=$root/$1/orig
        txt=/tmp/$1/txt
        tok=$root/$1/tok
        echo "running: scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt"
        scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt
        echo -ne "running normalize-text-standalone.pl and tokenize-text.pl"
        for collection in $txt/*; do
            mkdir -p $tok/`basename $collection`
            for file in $collection/*; do
                fileBase=`basename $file .txt`
                lang=${fileBase#$prefixPat}
                if [[ $debug -gt 0 ]]; then
                    echo "normalize-text-standalone.pl -$lang < $file | tokenize-text.pl --$lang > $tok/`basename $collection`/$fileBase.tok"
                else
                    echo -ne "."
                fi
                normalize-text-standalone.pl -$lang < $file | tokenize-text.pl --$lang > $tok/`basename $collection`/$fileBase.tok
            done
        done
        echo ""
        ;;
    all)
        echo "tokenizing all"
        tokenize.sh kin; tokenize.sh mlg
        ;;
    *)
        echo "Usage: tokenize.sh all|kin|mlg"
        echo "   Runs the tokenizer for all files, just kin, or just mlg."
        echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
esac
