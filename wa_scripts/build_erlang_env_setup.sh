#!/usr/bin/env bash
set -euxo pipefail

export TMP_DIR=$(mktemp -d)
export PLATFORM=platform010
export PLATFORM_PATH=/usr/local/fbcode/$PLATFORM
export NCURSES_PATH=/mnt/gvfs/third-party2/ncurses/8c772974e4b062465b06ab94cdf5f78f9484d53f/6.1/platform010/76ebdda
export OPENSSL_PATH=/mnt/gvfs/third-party2/openssl/42ea077b4808b1277c8420c95903620205ab0ae8/1.1.1/platform010/76ebdda
export SYSTEMTAP_PATH=/mnt/gvfs/third-party2/systemtap/e114897070c1a58100e33cbb1cb32bf4d8e13c0e/4.7/platform010/5e1a20f
DEPENDENCIES=("$NCURSES_PATH" "$SYSTEMTAP_PATH")

export ERL_AFLAGS="+P65536 +Q1024"

for DEP in "${DEPENDENCIES[@]}"; do
    PATH=$DEP/bin:$PATH
done

export DEST_DIR=${DEST_DIR:-"$(pwd)/release"}

export ERL_TOP=$(pwd)
export PATH=$ERL_TOP/bin:$PATH

# For each dependency add its "include" and "lib" directories to compiler and linker flags
CFLAGS=""
CFLAGS="$CFLAGS -O2"
CFLAGS="$CFLAGS -Wl,-rpath=$PLATFORM_PATH/lib"
CFLAGS="$CFLAGS -Wno-error=undef"
CFLAGS="$CFLAGS -fdata-sections"
CFLAGS="$CFLAGS -ffunction-sections"
CFLAGS="$CFLAGS -g"
CFLAGS="$CFLAGS -ggdb"
CFLAGS="$CFLAGS -march=haswell"
CFLAGS="$CFLAGS -mtune=skylake"
for DEP in "${DEPENDENCIES[@]}"; do
    CFLAGS="$CFLAGS -I$DEP/include"
done

LDFLAGS=""
LDFLAGS="$LDFLAGS -Wl,--emit-relocs"
LDFLAGS="$LDFLAGS -Wl,-rpath=$PLATFORM_PATH/lib"
LDFLAGS="$LDFLAGS -Wl,-z,common-page-size=2097152"
LDFLAGS="$LDFLAGS -Wl,-z,max-page-size=2097152"
LDFLAGS="$LDFLAGS -Wl,-z,notext"
LDFLAGS="$LDFLAGS -Wl,-z,now"
LDFLAGS="$LDFLAGS -Wl,-z,relro"
for DEP in "${DEPENDENCIES[@]}"; do
    LDFLAGS="$LDFLAGS -L$DEP/lib"
done

if [ "${USE_CLANG-}" = "1" ]; then
    CC_PAR=clang.par
    CXX_PAR=clang++.par
else
    CC_PAR=gcc.par
    CXX_PAR=g++.par
fi

CC=$TMP_DIR/$CC_PAR.sh
CXX=$TMP_DIR/$CXX_PAR.sh
/usr/local/fbcode/bin/$CC_PAR --platform $PLATFORM --gen-script "$CC"
/usr/local/fbcode/bin/$CXX_PAR --platform $PLATFORM --gen-script "$CXX"

export CC=$CC
export CFLAGS=$CFLAGS
export CPPFLAGS=$CFLAGS
export CXX=$CXX
export LD=$CC
export LDFLAGS=$LDFLAGS
