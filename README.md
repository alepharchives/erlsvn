# erlsvn

erlsvn - is a svn dump parser + some associate functionality.

# Setup a SVN repo for experimenting

    cd ~/junk
    svnadmin create my-svn-repo
    cat > my-svn-repo/hooks/pre-revprop-change
    #!/bin/sh
    exit 0
    ^D
    chmod +x my-svn-repo/hooks/pre-revprop-change
    svnsync init file:///home/tobbe/junk/my-svn-repo https://svn.hq.kred/repos/user/tobbe
    svnsync sync file:///home/tobbe/junk/my-svn-repo
    svnadmin dump my-svn-repo > my-svn-repo.dump


