#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
exit_code=0

usage() {
    echo "Usage: treemerge.sh all | kin | mlg | path/to/tokfile.tok"
    echo "   Runs the source tree merger for all files, just kin, just mlg, or a specific collection of tree files."
    echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
    exit 1
}


case $1 in
    kin|mlg)
        echo "merging $1"
        treesrc=$root/$1/tree/src
        treedst=$root/$1/tree
        for collection in $treesrc/* ; do
            echo $collection
            for tree in $collection/* ; do
                treemerge.sh $tree
                (( exit_code += $? ))
            done
        done
        ;;
    all)
        echo "merging all"
        treemerge.sh kin
        (( exit_code += $? ))
        treemerge.sh mlg
        (( exit_code += $? ))
        ;;
    *)
        fullpath=`readlink -f $1`
        case $fullpath in
            */tree/src/*\.eng|*/tree/src/*\.fra|*/tree/src/*\.kin|*/tree/src/*\.mlg)
                if [[ -e $fullpath ]]; then
                    base=$( basename $fullpath )
                    collection=$( basename $( dirname $fullpath ) )
                    lang=$( basename $( dirname $( dirname $( dirname $( dirname $fullpath ) ) ) ) )
                    echo "merging $fullpath to $root/$lang/tree/$collection/$base.tree"
                    #echo "running: scalabha run opennlp.scalabha.tree.Merge -i $fullpath -o $root/$lang/tree/$collection/$base.tree"
                    scalabha run opennlp.scalabha.tree.Merge -i $fullpath -o $root/$lang/tree/$collection/$base.tree
                    (( exit_code += $? ))
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

exit $exit_code
