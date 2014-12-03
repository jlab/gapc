include ../../../config.mf

.PHONY: all
	
all: shapemfepfx nowindow 

shapemfepfx: ../../grammar/adpf_nonamb.gap ../../../gapc ../../gapc_filter/nonamb_answer.hh
	../../../gapc -I../../gapc_filter -i $@ --tab-all --window-mode $< --kbacktrack -o $@.cc --no-coopt-class
	sed -i 's|cat $$(RTLIB)/generic_main.cc|cat main.cc|' $@.mf
	make -f $@.mf CXXFLAGS_EXTRA=" -I../../gapc_filter -ffast-math"
	mv $@ main
	
nowindow: ../../grammar/adpf_nonamb.gap ../../../gapc ../../gapc_filter/nonamb_answer.hh
	../../../gapc -i shapemfepfx --tab-all $< --kbacktrack -o shapemfepfx.cc --no-coopt-class
	sed -i 's|cat $$(RTLIB)/generic_main.cc|cat main.cc|' shapemfepfx.mf
	make -f shapemfepfx.mf CXXFLAGS_EXTRA=" -I../../gapc_filter -ffast-math"
	mv shapemfepfx nowindow

clean:
	rm -f shapemfepfx.o shapemfepfx.cc shapemfepfx.mf shapemfepfx.hh shapemfepfx main shapemfepfx.o shapemfepfx_main.cc string.o nowindow.o nowindow.cc nowindow.mf nowindow.hh nowindow_main.cc nowindow_main.o test test.o nowindow *.d *.o
