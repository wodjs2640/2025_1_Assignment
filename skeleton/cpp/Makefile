CXX = g++
CXXFLAGS = -O2 -std=gnu++17 -Iinclude
SRCDIR = src
TARGET = main
SOURCES = $(SRCDIR)/main.cpp $(SRCDIR)/hw3.cpp

all: $(TARGET)

$(TARGET): $(SOURCES)
	$(CXX) $(CXXFLAGS) $(SOURCES) -o $(TARGET)

run: $(TARGET)
	./$(TARGET) < input/1.in

clean:
	rm -f $(TARGET)
