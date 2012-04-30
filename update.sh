BASEDIR=~/.emacs.d/manual-packages

cd $BASEDIR/bbdb
cvs update
autoconf
./configure
make all

cd $BASEDIR/bitlbee
git pull

cd $BASEDIR/emms
git pull
make

cd $BASEDIR
wget -N http://tromey.com/elpa/package.el

cd $BASEDIR/predictive
git pull

cd $BASEDIR/twittering-mode
git pull
emacs -q -batch -f batch-byte-compile twittering-mode.el

cd $BASEDIR/typopunct
git pull

cd $BASEDIR/w3m
cvs update
make

