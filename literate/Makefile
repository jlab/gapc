# execute like this:
# ./lity < ../parser.y > ../../report/parser.tex

include ../makefiles/lexyaccxx.mf

YFLAGS = -d

TMP = lit_parse.cc lit_parse.hh lit_parse.output literate.cc lity


.PHONY: all

all: lity

literate.cc: lit_parse.cc

lity: literate.o lit_parse.o
	$(CXX) -o $@ $^ $(LDFLAGS) $(LDLIBS)

.PHONY: clean

clean:
	rm -f $(TMP)
