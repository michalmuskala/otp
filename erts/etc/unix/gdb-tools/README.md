# Erlang/OTP GDB Tools

This repository contains GDB tools that cannot
be part of the main Erlang/OTP repository for licensing
reasons. The tools are used when building and debugging
Erlang/OTP with gdb and are installed as needed by the
Erlang/OTP build system.

Right now the only tool in this repository is the jit-reader
which is a plugin to gdb that allows it to read the stack of
Erlang processes when using the JIT. This allows commands
such as `backtrace` to show Erlang stackframes. For example:

```
(gdb) bt
#0  0x00007fffa40005ee in global::call_bif_shared ()
#1  0x00007fffa411eaac in erl_eval:eval_lc1/7 () at erl_eval.erl:929
#2  0x00007fffa411e7bc in erl_eval:eval_lc/7 () at erl_eval.erl:917
#3  0x00007fffa4296528 in lists:map_1/2 () at lists.erl:2082
#4  0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
#5  0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
#6  0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
.....
#98 0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
#99 0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
#100 0x00007fffa4296554 in lists:map_1/2 () at lists.erl:2082
#101 0x00007fffa42963d4 in lists:map/2 () at lists.erl:2077
#102 0x00007fffa411e45c in erl_eval:do_apply/7 () at erl_eval.erl:904
#103 0x00007fffa45f4c58 in shell:exprs/7 () at shell.erl:893
#104 0x00007fffa45f42c4 in shell:eval_exprs/7 () at shell.erl:849
#105 0x00007fffa45f3dac in shell:eval_loop/4 () at shell.erl:834
#106 0x00007fffa40024b8 in erts_beamasm:normal_exit/0-CodeInfoPrologue ()
Backtrace stopped: previous frame inner to this frame (corrupt stack?)
(gdb) 
```

## Updates of jit-reader

When updating jit-reader you first need to update this repository
and push the new version here. Then you need to run
`./otp_build update_gdb_tools` in the Erlang/OTP repo for
it to use the correct commit.
