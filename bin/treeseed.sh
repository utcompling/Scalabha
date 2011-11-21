#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2

case $1 in
    kin|mlg)
        echo "treeseeding $1"
        tok=$root/$1/tok
        treesrc=$root/$1/tree/src
        scalabha run opennlp.scalabha.preproc.TOK2TREE -i $tok -o $treesrc
        ;;
    all)
        echo "treeseeding all"
        treeseed.sh kin; treeseed.sh mlg
        ;;
    *\.tok)
        if [[ -e $1 ]]; then
            echo "treeseeding $1"
            fullpath=`readlink -f $1`
            filename=`basename $fullpath .tok`
            collection=$(basename $(dirname $fullpath))
            langroot=$(dirname $(dirname $(dirname $fullpath)))
            scalabha run opennlp.scalabha.preproc.TOK2TREE -i $1 -o $langroot/tree/src/$collection/$filename
        else
            echo "could not find $1"
        fi
        ;;
    *)
        echo "Usage: tokenize.sh all | kin | mlg | path/to/tokfile.tok"
        echo "   Runs the tokenizer for all files, just kin, just mlg, or a specific tok file."
        echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
        ;;
esac
