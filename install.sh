# moz
#http://www.emacswiki.org/emacs/MozRepl

PACKAGEDIR=~/.emacs.d/manual-packages
TMPDIR=/tmp

# Get the date from the latest checkout and format it as 'yyymmdd'
function get_version () {
    if [ "$repo" == "cvs" ]; then
        version=`cvs log -N | grep ^date: | sort | tail -1 | cut -c7-16 | sed 's|/||g'`
    elif [ "$repo" == "git" ]; then
        version=`git log -1 -format="%ci" | cut -c-10 | sed 's|-||g'`
    fi
}

# Checkout the source from cvs or git
function get_source () {
    if [ "$repo" == "cvs" ]; then
        echo "No password is set.  Just hit Enter/Return key."
        cvs -d $url login 
        cvs -d $url checkout $package
    elif [ "$repo" == "git" ]; then
        git clone $url
    fi
}

# Delete source and the directory used to assemble the package 
function cleanup () {
    rm -r $package
    rm -r $pkgdir
}

# Create a new package:
# 1. Check out the source code
# 2. Get the version
# 3. Copy the necessary to the new package directory
# 4. Create a ...-pkg.el manifest
# 5. Create a tarball from the package directory
# 6. Make tarball to $PACKAGEDIR and clean up the files that were
#    created during step 1, 3 and 4.
function make_package () {
    pkgname=$package-$repo
    pkgdir=$pkgname-$version

    cd $TMPDIR
    
    get_source
    
    cd $package
    if [ -n "$build" ]; then
        $build
    fi
    get_version
    cd ..

    mkdir $pkgdir
    cp -r `$sources` $pkgdir/
    echo "(define-package \"${pkgname}\" \"${version}\" \
\"${description}\") " > $pkgdir/$pkgname-pkg.el

    tar -cf $pkgdir.tar $pkgdir
    mv $pkgdir.tar $PACKAGEDIR
    cleanup
}

### Packages

# Install the package.el package manager
function get_package_el () {
    cd ~/.emacs.d/
    wget "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el"
}


function get_bbdb() {
    package=bbdb
    description='A phone number and address database program for Emacs'
    repo=cvs
    url=:pserver:anonymous@bbdb.cvs.sourceforge.net:/cvsroot/bbdb
    build='make autoloads'
    sources='ls bbdb/lisp/*.el | grep -v xemacs'

    make_package
}

function get_emms () {
    package=emms
    description='The Emacs Multimedia System'
    url=git://git.sv.gnu.org/emms.git
    repo=git
    build=""
    sources='ls emms/lisp/*.el'
    
    make_package
}

#http://emacs-w3m.namazu.org/
function get_w3m () {
    package=emacs-w3m
    description='An Emacs interface to w3m'
    repo=cvs
    url=:pserver:anonymous@cvs.namazu.org:/storage/cvsroot
    build=""
    sources='ls -d emacs-w3m/*.el emacs-w3m/shimbun'

    make_package
}


# Needs proper version ( is csv id tag )
function get_csvmode () {
    cd $PACKAGEDIR
    wget http://centaur.maths.qmul.ac.uk/Emacs/files/csv-mode.el
}

# Needs version
function get_moz () {
    cd $PACKAGEDIR
    wget https://raw.github.com/bard/mozrepl/master/chrome/content/moz.el
}

# Imports fine
function get_noword () {
    cd $PACKAGEDIR
    wget http://www.emacswiki.org/emacs/download/no-word.el
}

### Contributed to Marmelade.

# Import bitlbee.el/bitlbee.el
function get_bitlbee () {
    cd $PACKAGEDIR
    git clone git://github.com/BramvdKroef/bitlbee.el.git
}

# Needs version
function get_lorem_ipsum () {
    cd $PACKAGEDIR
    wget http://www.emacswiki.org/emacs/download/lorem-ipsum.el
}

# Import twittering-mode-[version]/twittering-mode.el
# http://twmode.sourceforge.net/
# git clone git://github.com/hayamiz/twittering-mode.git
function get_twitter () {
    cd $PACKAGEDIR
    wget http://sourceforge.net/projects/twmode/files/twittering-mode-2.0.0/twittering-mode-2.0.0.tar.gz
    tar -xf twittering-mode-2.0.0.tar.gz
    rm twittering-mode-2.0.0.tar.gz
}

# git://github.com/emacsmirror/typopunct.git
# Needs version
function get_typopunct () {
    cd $PACKAGEDIR
    wget 'https://raw.github.com/emacsmirror/typopunct/d9da7e57d0d0172c6313a02d9b4b896b222bbffe/typopunct.el'
}
