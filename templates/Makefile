CPPFLAGS = -MMD -MP -DCC_PLAYGROUND
CXXFLAGS ?= -std=c++1z
CXX_DBG_FLAGS = $(CXXFLAGS) -g -O0
CXX ?= g++
LD = $(CXX)
LDFLAGS := $(CXXFLAGS) $(LDFLAGS)

deb: snippet.deb.o
	$(LD) $^ -o $@ $(LDFLAGS)

rel: snippet.rel.o
	$(LD) $^ -o $@ $(LDFLAGS)

%.deb.o: %.cpp .dir-locals.el .envrc
	$(CXX) $(CPPFLAGS) $(CXX_DBG_FLAGS) -c $< -o $@

%.rel.o: %.cpp .dir-locals.el .envrc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean:
	$(RM) -rf *.o *.d deb rel

DEPS := snippet.deb.d snippet.rel.d
-include $(DEPS)

.PHONY: clean
