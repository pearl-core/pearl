#!/usr/bin/env bash

set -eu

cd /tmp

# https://docs.conda.io/en/latest/miniconda_hashes.html
if [[ $(uname) == "Linux" ]]
then
    MINICONDA_HASH="bb2e3cedd2e78a8bb6872ab3ab5b1266a90f8c7004a22d8dc2ea5effeb6a439a"
    MACHINE="Linux"
else
    MINICONDA_HASH="ccc1bded923a790cd61cd17c83c3dcc374dc0415cfa7fb1f71e6a2438236543d"
    MACHINE="MacOSX"
fi

MINICONDA_VERSION="4.8.3"
MINICONDA_FILE="Miniconda3-py37_${MINICONDA_VERSION}-${MACHINE}-x86_64.sh"
CONDA_DIR="${HOME}/miniconda3"


wget --quiet https://repo.continuum.io/miniconda/${MINICONDA_FILE}
echo "${MINICONDA_HASH} *${MINICONDA_FILE}" | sha256sum -c -
/usr/bin/env bash ${MINICONDA_FILE} -f -b -p ${CONDA_DIR}
rm ${MINICONDA_FILE}