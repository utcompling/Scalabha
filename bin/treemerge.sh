#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
exit_code=0

usage() {
    echo "Usage: treemerge.sh all | kin | mlg | path/to/tree/src/something.kin or something.mlg"
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
                if [[ ( $exit_code -ne 0 ) && ( $2 != "-f" )  ]]; then
                    exit $exit_code
                else
                    treemerge.sh $tree
                    (( exit_code += $? ))
                fi
            done
        done
        ;;
    all)
        echo "merging all"
        treemerge.sh kin $2
        (( exit_code += $? ))
        if [[ ( $exit_code -ne 0 ) && ( $2 != "-f" ) ]]; then
            exit $exit_code
        else
            treemerge.sh mlg $2
            (( exit_code += $? ))
        fi
        ;;
    *)
        #fullpath=`readlink -f $1`
        fullpath=$1
        case $fullpath in
            */tree/src/*\.eng/|*/tree/src/*\.eng|*/tree/src/*\.fra/|*/tree/src/*\.fra|*/tree/src/*\.kin/|*/tree/src/*\.kin|*/tree/src/*\.mlg/|*/tree/src/*\.mlg)
                # wow, how ugly was that? Don't answer that. Bash case statements don't really do regexes, just path expansion.
                if [[ -e $fullpath ]]; then
                    base=$( basename $fullpath )
                    collection=$( basename $( dirname $fullpath ) )
                    lang=$( basename $( dirname $( dirname $( dirname $( dirname $fullpath ) ) ) ) )
                    echo "merging $fullpath to $root/$lang/tree/$collection/$base.tree"
                    #echo "running: scalabha run opennlp.scalabha.tree.Merge -i $fullpath -o $root/$lang/tree/$collection/$base.tree"
                    if [[ $2 == "-f" ]]; then
                      scalabha run opennlp.scalabha.tree.Merge -f -i $fullpath -o $root/$lang/tree/$collection/$base.tree
                    else
                      scalabha run opennlp.scalabha.tree.Merge --pprintErrs -i $fullpath -o $root/$lang/tree/$collection/$base.tree
                    fi
                    (( exit_code += $? ))
                else
                    echo 
                    echo $fullpath does not exist.
                    echo 
                    usage
                fi
                ;;
            *)
                echo
                echo "Dont' know what to do with $fullpath"
                echo
                usage
                ;;
        esac
        ;;
esac

exit $exit_code
