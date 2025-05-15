JAVAC = javac
SRC = src
BIN = bin
SOURCES = Main.java $(wildcard $(SRC)/*.java)
TARGET = $(BIN)/Main.class

all: $(TARGET)

$(TARGET): $(SOURCES)
	mkdir -p $(BIN)
	$(JAVAC) -d $(BIN) $(SOURCES)

run: $(TARGET)
	java -cp $(BIN) Main < input/1.in

clean:
	rm -rf $(BIN)
