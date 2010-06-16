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

# Run the converter

    #
    # Remove the put to disable step by step execution
    # Arg1: Name of the svn dump-file
    # Arg2: Name of a (non-existing) directory to hold the git repo.
    #
    1 > put(dbg,true),svn2git:run("my-svn-repo.dump","my-svn-repo-git").



