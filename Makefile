CC := rustc
SOURCES := $(wildcard *.rs)
MAIN := main.rs
BIN := calc

all: $(BIN)
$(BIN): $(SOURCES)
	$(CC) -o $(BIN) $(MAIN)

clean:
	rm -f $(BIN)

.PHONY: clean
