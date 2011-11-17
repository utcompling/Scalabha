#!/bin/bash

root=$LDMT_MURI_DIR/data/phase2
prefixPat=[^.]*\.
debug=false
exitCode=0

case $1 in
    kin|mlg)
        echo "validating $1"
        orig=$root/$1/orig
        txt=/tmp/$1/txt
        echo "running: scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt"
        scalabha run opennlp.scalabha.preproc.X2TXT -x $orig -t $txt
        (( exitCode += $? ))
        echo ""
        ;;
    all)
        echo "validating all"
        validate-xml.sh kin
        (( exitCode += $? ))
        validate-xml.sh mlg
        (( exitCode += $? ))
        ;;
    *)
        echo "Usage: validate-xml all|kin|mlg"
        echo "   Runs the X2TXT transform for all files, just kin, or just mlg and dumps the text"
        echo "   output to /tmp. During the transform, we check for several classes of errors, so"
        echo "   this can also be treated as a validation."
        echo ""
        echo "   Be sure that \$LDMT_MURI_DIR is set. (Current value:'$LDMT_MURI_DIR')."
esac

if [[ $exitCode -eq 1 ]]; then
    echo "There was 1 error."
else
    echo "There were $exitCode errors."
fi
echo ""
exit $exitCode
