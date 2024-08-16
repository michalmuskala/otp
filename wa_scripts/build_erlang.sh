#!/usr/bin/env bash
set -euxo pipefail

script_loc="$(dirname "${BASH_SOURCE[0]}")"
source "$script_loc/build_erlang_env_setup.sh"

# ets-write-concurrency-locks: change from default 64 to maximum 256
#  for all write-concurrent hashset tables. This reduces lock contention
#  of db_hash_slot rwlock at the expense of allocating ~3KiB more per
#  write-concurrent ETS set/bag/duplicate_bag table.
./configure \
    --prefix=/ \
    --disable-evp-hmac \
    --disable-sctp \
    --enable-builtin-zlib \
    --enable-dynamic-ssl-lib \
    --enable-lock-counter \
    --with-dynamic-trace=systemtap \
    --with-ets-write-concurrency-locks=256 \
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

make RELEASE_ROOT="$DEST_DIR" DOC_TARGETS='chunks' -j8 release release_docs

ERTS_VSN=$(sed -n "s/^VSN[  ]*=[  ]*\\(.*\\)/\\1/p" < erts/vsn.mk)

# Round up the size of the text segment to a multiple of 2MiB for THP
( cd "$script_loc" ; make )
for f in bin/x86_64-pc-linux-gnu/beam.*
do
    $script_loc/round-up-text-segment $f
    chmod 0755 $f
done

for emu_type in frmptr opt
do
    $script_loc/bolt.sh jit $emu_type
    if [ "$emu_type" = "opt" ]
    then
	cp bin/x86_64-pc-linux-gnu/beam.{jit,smp}
    else
	cp bin/x86_64-pc-linux-gnu/beam.$emu_type.{jit,smp}
    fi
done

# Install different BEAM types
install -vm 0755 -t "$DEST_DIR/erts-$ERTS_VSN/bin" \
    bin/x86_64-pc-linux-gnu/beam.smp \
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
