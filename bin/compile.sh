#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2

usage() {
    echo "Usage: tokenize.sh all | kin | mlg | path/to/tokfile.tok"
    echo "   Runs the tokenizer for all files, just kin, just mlg, or a specific tok file."
    echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
    exit 1
}


case $1 in
    kin|mlg)
        echo "compiling $1"
        treesrc=$root/$1/tree/src
        treedst=$root/$1/tree
        for collection in $treesrc/* ; do
            echo $collection
            for tree in $collection/* ; do
                echo "compiling $tree/ to $treedst/`basename $collection`/`basename $tree`.tree"
            done
        done
        ;;
    all)
        echo "compiling all"
        treeseed.sh kin; treeseed.sh mlg
        ;;
    *)
        fullpath=`readlink -f $1`
        case $fullpath in
            */tree/src/*\.eng|*/tree/src/*\.fra|*/tree/src/*\.kin|*/tree/src/*\.mlg)
                if [[ -e $fullpath ]]; then
                    echo "compiling $fullpath"
                else
                    echo 
                    echo $fullpath does not exist.
                    echo 
                    usage
                fi
                ;;
            *)
                usage
                ;;
        esac
        ;;
esac
