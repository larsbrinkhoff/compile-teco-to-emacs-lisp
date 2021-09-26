rm -f output.el*
emacs -Q --batch --load run.el
if cmp test.el output.el; then
    echo PASS
    exit 0
else
    echo FAIL
    exit 1
fi
