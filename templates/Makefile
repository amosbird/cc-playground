CPPFLAGS = -MMD -MP -DCC_PLAYGROUND
CXXFLAGS ?= -std=c++1z
CXX_DBG_FLAGS = $(CXXFLAGS) -g -O0
CXX ?= g++
LD = $(CXX)
LDFLAGS := $(CXXFLAGS) $(LDFLAGS)

deb_test: test.deb.o snippet.deb.o
	$(LD) $< -o $@ $(LDFLAGS) -lgtest_main -lgtest

rel_test: test.rel.o snippet.rel.o
	$(LD) $< -o $@ $(LDFLAGS) -lgtest_main -lgtest

deb: main.deb.o snippet.deb.o
	$(LD) $^ -o $@ $(LDFLAGS)

rel: main.rel.o snippet.rel.o
	$(LD) $^ -o $@ $(LDFLAGS)

bench: bench.rel.o snippet.rel.o
	$(LD) $< -o $@ $(LDFLAGS) -lbenchmark

%.deb.o: %.cpp .dir-locals.el .envrc
	$(CXX) $(CPPFLAGS) $(CXX_DBG_FLAGS) -c $< -o $@

%.rel.o: %.cpp .dir-locals.el .envrc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean:
	$(RM) -rf *.o *.d deb rel deb_test rel_test

DEPS := main.deb.d main.rel.d test.deb.d test.rel.d snippet.deb.d snippet.rel.d
-include $(DEPS)

.PHONY: clean
