# moz
#http://www.emacswiki.org/emacs/MozRepl

BASEDIR=~/.emacs.d/site-lisp/

echo "Installing bitlbee.el"
cd $BASEDIR
git clone git://pmade.com/elisp

echo "Installing graphviz mode"
wget http://users.skynet.be/ppareit/projects/graphviz-dot-mode/graphviz-dot-mode.el

echo "Installing growl.el"
wget http://www.emacswiki.org/cgi-bin/wiki/download/growl.el

echo "Installing no-word.el"
wget http://www.emacswiki.org/emacs/download/no-word.el

echo "Installing predictive mode"
git clone http://www.dr-qubit.org/git/predictive.git predictive

echo "Installing twittering mode"
git clone git://github.com/hayamiz/twittering-mode.git

echo "Installing Window-number.el"
wget http://www.emacswiki.org/emacs/download/window-number.el

echo "Installing w3m-mode"
#http://emacs-w3m.namazu.org/
echo "No password is set.  Just hit Enter/Return key."
cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m

echo "Installing bbdb"
echo "No password is set.  Just hit Enter/Return key."
cvs -d :pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb login
cvs -d :pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb checkout bbdb

echo "Installing EMMS"
git clone git://git.sv.gnu.org/emms.git

echo "Installing yasnippet"
svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet

echo "Installing CSV-mode"
wget http://www.emacswiki.org/emacs/download/csv-mode.el

echo "Installing PHP-mode"
wget http://www.emacswiki.org/emacs/download/php-mode.el

