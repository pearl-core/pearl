unset HOME PEARL_ROOT PEARL_HOME

function pearlSetUp(){
    pearlRootSetUp
    pearlHomeSetUp
}

function pearlTearDown(){
    pearlRootTearDown
    pearlHomeTearDown
}

function pearlHomeSetUp(){
    export HOME=$(TMPDIR=/tmp mktemp -d -t pearl-user-home.XXXXXXX)
    mkdir -p $HOME
    export PEARL_HOME=${HOME}/.config/pearl
    mkdir -p $PEARL_HOME
    mkdir -p $PEARL_HOME/bin
    touch $PEARL_HOME/pearl.conf
    mkdir -p $PEARL_HOME/repos
    mkdir -p $PEARL_HOME/packages
    mkdir -p $PEARL_HOME/tmp
}

function pearlHomeTearDown(){
    rm -rf $PEARL_HOME
    rm -rf $HOME
    unset PEARL_HOME
}

# The Pearl setup is useful when the package requires
# library dependencies such as $PEARL_ROOT/lib/utils/utils.sh
function pearlRootSetUp() {
    export PEARL_ROOT=$(TMPDIR=/tmp mktemp -d -t pearl-test-root.XXXXXXX)
    mkdir -p $PEARL_ROOT/bin
    mkdir -p $PEARL_ROOT/etc
    touch $PEARL_ROOT/etc/pearl.conf.template
    mkdir -p $PEARL_ROOT/lib/utils
}

function pearlRootTearDown(){
    rm -rf $PEARL_ROOT
    unset PEARL_ROOT
}
