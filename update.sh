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

#cd $BASEDIR/icicles
# http://www.emacswiki.org/emacs/Icicles_-_Libraries
#wget "http://www.emacswiki.org/emacs/download/icicles.el" 
#wget "http://www.emacswiki.org/emacs/download/icicles-chg.el" 
#wget "http://www.emacswiki.org/emacs/download/icicles-cmd1.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-cmd2.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-doc1.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-doc2.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-face.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-fn.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-mac.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-mcmd.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-mode.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-opt.el"
#wget "http://www.emacswiki.org/emacs/download/icicles-var.el"
