#!/usr/bin/env bash
set -euxo pipefail

script_loc="$(dirname "${BASH_SOURCE[0]}")"
source "$script_loc/build_erlang_env_setup.sh"

./configure \
    --prefix=/ \
    --disable-sctp \
    --enable-lock-counter \
    --with-dynamic-trace=systemtap \
    --with-microstate-accounting=extra \
    --with-ssl="$OPENSSL_PATH" \
    --without-javac \
    --without-jinterface \
    --without-odbc

make clean

make V=1 -j

# We patch sources of preloaded .beam files, recreate them
# make -C ${ERL_TOP}/erts/preloaded/src clean
# make V=1 -j preloaded
# make -C ${ERL_TOP}/erts/preloaded/src copy

# Build only essential BEAM types for production release
# Use following line to include more targets for manual debugging/testing
# INTERESTING_TARGETS=(gprof frmptr icount)
INTERESTING_TARGETS=(debug frmptr)
for TYPE in "${INTERESTING_TARGETS[@]}"
do
    make V=1 -C "$ERL_TOP/erts/emulator" -j "$TYPE"
done

rm -rf "$DEST_DIR"
install -m 0755 -d "$DEST_DIR"

make RELEASE_ROOT="$DEST_DIR" DOC_TARGETS='chunks man' -j8 release release_docs

ERTS_VSN=$(sed -n "s/^VSN[  ]*=[  ]*\\(.*\\)/\\1/p" < erts/vsn.mk)

# Install different BEAM types
install -vm 0755 -t "$DEST_DIR/erts-$ERTS_VSN/bin" \
    bin/x86_64-pc-linux-gnu/beam.*.smp \
    bin/x86_64-pc-linux-gnu/erl_child_setup.*

# gdb helpers
install -vm 0644 -t "$DEST_DIR/erts-$ERTS_VSN/lib" \
    erts/etc/unix/etp-commands \
    bin/x86_64-pc-linux-gnu/jit-reader.so

# install erlang_ls.config for vscode navigation.
install -vm 0644 -t "$DEST_DIR" erlang_ls.config

# comment out jit-reader in etp-commands.
sed -i -e "s/jit-reader-load/#jit-reader-load/" "$DEST_DIR/erts-$ERTS_VSN/lib/etp-commands"

cd "$DEST_DIR"
./Install -minimal "$DEST_DIR"

# Make it relocatable, see reltool_target.erl (in OTP) about dyn_erl
cd "$DEST_DIR/bin"
rm erl
ln -sfv "../erts-$ERTS_VSN/bin/erl" erl
cd "../erts-$ERTS_VSN/bin"
rm erl
ln -sfv dyn_erl erl

# copy the trampoline script to launch erl from dotslash.
install -vm 0755 "$ERL_TOP/wa_scripts/erl_trampoline.sh" "$DEST_DIR/bin"

echo "Release installed in $DEST_DIR"
