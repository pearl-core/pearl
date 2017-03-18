
unset PEARL_ROOT PEARL_HOME
OLD_PWD=$PWD

function pearlSetUp(){
    pearlRootSetUp
    pearlHomeSetUp
}

function pearlRootSetUp(){
    export PEARL_ROOT=$(TMPDIR=/tmp mktemp -d -t pearl-test-root.XXXXXXX)
    mkdir -p $PEARL_ROOT/bin
    mkdir -p $PEARL_ROOT/etc
    touch $PEARL_ROOT/etc/pearl.conf.template
    mkdir -p $PEARL_ROOT/lib/utils
    echo "echo sourced utils.sh" > $PEARL_ROOT/lib/utils/utils.sh
    echo "echo sourced osx-compat.sh" > $PEARL_ROOT/lib/utils/osx-compat.sh
    echo "echo sourced osx-compat.fish" > $PEARL_ROOT/lib/utils/osx-compat.fish
    echo "1.2.3" > $PEARL_ROOT/VERSION
}

function pearlHomeSetUp(){
    HOME=$(TMPDIR=/tmp mktemp -d -t pearl-user-home.XXXXXXX)
    mkdir -p $HOME
    export PEARL_HOME=${HOME}/.config/pearl
    mkdir -p $PEARL_HOME
    mkdir -p $PEARL_HOME/bin
    mkdir -p $PEARL_HOME/etc
    touch $PEARL_HOME/etc/pearl.conf
    mkdir -p $PEARL_HOME/repos
    mkdir -p $PEARL_HOME/packages
    mkdir -p $PEARL_HOME/tmp
}

function pearlTearDown(){
    cd $OLD_PWD
    rm -rf $PEARL_HOME
    rm -rf $HOME
    rm -rf $PEARL_ROOT
    unset PEARL_ROOT PEARL_HOME
}

function setUpUnitTests(){
    OUTPUT_DIR="${SHUNIT_TMPDIR}/output"
    mkdir "${OUTPUT_DIR}"
    STDOUTF="${OUTPUT_DIR}/stdout"
    STDERRF="${OUTPUT_DIR}/stderr"
}

function assertCommandSuccess(){
    $(set -e
      "$@" > $STDOUTF 2> $STDERRF
    )
    assertTrue "The command $1 did not return 0 exit status" $?
}

function assertCommandFail(){
    $(set -e
      "$@" > $STDOUTF 2> $STDERRF
    )
    assertFalse "The command $1 returned 0 exit status" $?
}

# $1: expected exit status
# $2-: The command under test
function assertCommandFailOnStatus(){
    local status=$1
    shift
    $(set -e
      "$@" > $STDOUTF 2> $STDERRF
    )
    assertEquals $status $?
}
