CFLAGS = -g -Wall
EXE = round-up-text-segment
OBJS = round-up-text-segment.o

.PHONY: all
all: $(EXE)

$(EXE): $(OBJS)
	$(CC) -o $@ $^

.PHONY: clean
clean:
	-rm -f $(EXE) $(OBJS)

