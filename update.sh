BASEDIR=~/.emacs.d/site-lisp

cd $BASEDIR/bbdb
cvs update
autoconf
./configure
make all

cd $BASEDIR/emms
git pull
make

cd $BASEDIR/twittering-mode
git pull
emacs -q -batch -f batch-byte-compile twittering-mode.el

cd $BASEDIR/w3m
cvs update
make

cd $BASEDIR/yasnippet
svn update
rake compile

cd $BASEDIR/bitlbee
git pull
