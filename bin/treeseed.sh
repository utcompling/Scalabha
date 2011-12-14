#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
exit_code=0

case $1 in
    kin|mlg)
        echo "treeseeding $1"
        tok=$root/$1/tok
        treesrc=$root/$1/tree/src
        scalabha run opennlp.scalabha.tree.Tok2Trees -i $tok -o $treesrc
        (( exit_code += $? ))
        ;;
    all)
        echo "treeseeding all"
        treeseed.sh kin
        (( exit_code += $? ))
        treeseed.sh mlg
        (( exit_code += $? ))
        ;;
    *\.tok)
        if [[ -e $1 ]]; then
            echo "treeseeding $1"
            fullpath=`readlink -f $1`
            filename=`basename $fullpath .tok`
            collection=$(basename $(dirname $fullpath))
            langroot=$(dirname $(dirname $(dirname $fullpath)))
            echo "running: scalabha run opennlp.scalabha.tree.Tok2Trees -i $1 -o $langroot/tree/src/$collection/$filename"
            scalabha run opennlp.scalabha.tree.Tok2Trees -i $1 -o $langroot/tree/src/$collection/$filename
            (( exit_code += $? ))
        else
            echo "could not find $1"
            (( exit_code += 1 ))
        fi
        ;;
    *)
        echo "Usage: treeseed.sh path/to/tokfile.tok"
        echo "   Runs the treeseeder for a specific tok file."
        echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
        ;;
esac

exit $exit_code
