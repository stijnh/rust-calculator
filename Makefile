SOURCES := $(wildcard *.rs)
BIN := calc


$(BIN): $(SOURCES)
	rustc -o $(BIN) main.rs

