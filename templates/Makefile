CPPFLAGS = -MMD -MP
CXXFLAGS ?= -std=c++17
CXX_DBG_FLAGS = $(CXXFLAGS) -g
CXX ?= g++
LD = $(CXX)
LDFLAGS := $(CXXFLAGS) $(LDFLAGS)
LD_TEST_FLAGS = $(LDFLAGS) -lgtest -lgtest_main

deb_test: test.deb.o snippet.deb.o
	$(LD) $(LD_TEST_FLAGS) $^ -o $@

rel_test: test.rel.o snippet.rel.o
	$(LD) $(LD_TEST_FLAGS) $^ -o $@

deb: main.deb.o snippet.deb.o
	$(LD) $(LDFLAGS) $^ -o $@

rel: main.rel.o snippet.rel.o
	$(LD) $(LDFLAGS) $^ -o $@

%.deb.o: %.cpp .dir-locals.el
	$(CXX) $(CPPFLAGS) $(CXX_DBG_FLAGS) -c $< -o $@

%.rel.o: %.cpp .dir-locals.el
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean:
	$(RM) -rf *.o *.d deb rel deb_test rel_test

DEPS := main.deb.d main.rel.d test.deb.d test.rel.d snippet.deb.d snippet.rel.d
-include $(DEPS)

.PHONY: clean
