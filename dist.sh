#!/bin/sh -e
set -x

cd $(dirname $0)
home=$(pwd)
name=$(basename $home)
name_and_date=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

make -C doc

git tag -f $name_and_date
git archive --prefix=$name_and_date/ $name_and_date | \
    ( cd $TMPDIR && tar xvf - )

echo '(progn (load "dist.lisp") (load "example.lisp") (quit))' | clbuild lisp 

rsync -a doc example $TMPDIR/$name_and_date

cd $TMPDIR

tgz=$TMPDIR/${name_and_date}.tgz
tar czf $tgz $name_and_date
gpg -b -a $tgz

mkdir -p ~/bob/public_html/atdoc/download/

rsync -av \
    $name_and_date/doc \
    $name_and_date/example \
    ~/bob/public_html/atdoc/

rsync $tgz $tgz.asc ~/bob/public_html/atdoc/download/

rm -f ~/bob/public_html/atdoc/download/atdoc.tar.gz 
rm -f ~/bob/public_html/atdoc/download/atdoc.tar.gz.asc

ln -sf ${name_and_date}.tgz ~/bob/public_html/atdoc/download/atdoc.tar.gz
ln -sf ${name_and_date}.tgz.asc ~/bob/public_html/atdoc/download/atdoc.tar.gz.asc

echo done
exit 0
rsync -av ~/bob/public_html bob.askja.de
