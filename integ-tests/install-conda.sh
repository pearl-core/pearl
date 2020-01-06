#!/usr/bin/env bash

set -eu

cd /tmp

# https://docs.conda.io/en/latest/miniconda_hashes.html
if [[ $(uname) == "Linux" ]]
then
    MINICONDA_HASH="bfe34e1fa28d6d75a7ad05fd02fa5472275673d5f5621b77380898dee1be15d2"
    MACHINE="Linux"
else
    MINICONDA_HASH="5cf91dde8f6024061c8b9239a1b4c34380238297adbdb9ef2061eb9d1a7f69bc"
    MACHINE="MacOSX"
fi

MINICONDA_VERSION="4.7.12.1"
MINICONDA_FILE="Miniconda3-${MINICONDA_VERSION}-${MACHINE}-x86_64.sh"
CONDA_DIR="${HOME}/miniconda3"


wget --quiet https://repo.continuum.io/miniconda/${MINICONDA_FILE}
echo "${MINICONDA_HASH} *${MINICONDA_FILE}" | sha256sum -c -
/usr/bin/env bash ${MINICONDA_FILE} -f -b -p ${CONDA_DIR}
rm ${MINICONDA_FILE}