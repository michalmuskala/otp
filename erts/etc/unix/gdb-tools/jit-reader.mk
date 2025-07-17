EXTRA_LIBS = $(BINDIR)/jit-reader.so

jit-reader: $(EXTRA_LIBS)

$(OBJDIR)/jit-reader.o: $(GDB_TOOLS)/jit-reader.c $(GDB_TOOLS)/jit-reader.h
	$(V_CC) $(DED_CFLAGS) -I$(ETC) -o $@ -c $<

$(BINDIR)/jit-reader.so: $(OBJDIR)/jit-reader.o
	$(V_LD) $(DED_LDFLAGS) -o $@ $^
