#!/bin/sh -eux

WHATSAPP_OTP_BOLT_DIRECTORY=$(pwd)/wa_scripts/bolt

ERL_TOP=$(pwd)
PATH=$ERL_TOP/release/bin:$PATH

if [ $# -ne 2 ]
then
    echo "usage: $0 flavor type"
    exit 1
fi

PERF2BOLT=$WHATSAPP_OTP_BOLT_DIRECTORY/perf2bolt
if [ ! -f "$PERF2BOLT" ]
then
    echo "can't find $PERF2BOLT, exiting"
    exit 1
fi

LLVM_BOLT=$WHATSAPP_OTP_BOLT_DIRECTORY/llvm-bolt
if [ ! -f "$LLVM_BOLT" ]
then
    echo "can't find $LLVM_BOLT, exiting"
    exit 1
fi

case $1 in
    emu|jit|smp)
        EMU_FLAVOR=$1
        ;;
    *)
        echo "unknown flavor $1"
        exit 1
        ;;
esac

case $2 in
    opt|debug|gcov|valgrind|gprof|lcnt|frmptr)
        EMU_TYPE=$2
        ;;
    *)
        echo "unknown type $2"
        exit 1
        ;;
esac

case $EMU_TYPE in
    opt)
        EMU=beam.$EMU_FLAVOR
        PERF_DATA=perf.data.$EMU_FLAVOR
        ;;
    *)
        EMU=beam.$EMU_TYPE.$EMU_FLAVOR
        PERF_DATA=perf.data.$EMU_TYPE.$EMU_FLAVOR
        ;;
esac

rm -f "$HOME/.dialyzer_plt"
rm -f "$PERF_DATA"

if [ -z "${SANDCASTLE-}" ] || hostname | grep -q "^devvm"
then
  echo "perf(1) might not be usable on this host, downloading a profile from Manifold"
  "$ERL_TOP/wa_scripts/bolt_perf_for_local_dev_build.data"
else
  perf record -e cycles:u -j any,u -o "$PERF_DATA" \
      ./bin/erl \
      -emu_type "$EMU_TYPE" \
      -emu_flavor "$EMU_FLAVOR" \
      +B \
      -boot no_dot_erlang \
      -noinput \
      -run dialyzer plain_cl \
      -extra \
      --build_plt \
      --apps compiler crypto erts kernel stdlib syntax_tools asn1 edoc et ftp inets mnesia observer public_key sasl runtime_tools snmp ssl tftp wx xmerl tools parsetools
fi
BEAM=$ERL_TOP/bin/x86_64-pc-linux-gnu/$EMU

rm -rf "$EMU.fdata" "$EMU.yaml"
$PERF2BOLT \
    --ignore-build-id \
    "$BEAM" \
    -p "$PERF_DATA" \
    -o "$EMU.fdata" \
    -w "$EMU.yaml"

rm -f "$BEAM-bolted"
$LLVM_BOLT \
    "$BEAM" \
    -o "$BEAM-bolted" \
    -b "$EMU.yaml" \
    -dyno-stats \
    -plt=all \
    -relocs \
    -reorder-blocks=cache+ \
    -reorder-functions=hfsort+ \
    -split-all-cold \
    -split-eh \
    -split-functions=3 \
    -update-debug-sections \
    -use-compact-aligner=true \
    -use-gnu-stack=1 \
    -use-old-text=0

mv "$BEAM-bolted" "$BEAM"
