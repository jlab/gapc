include ../../../config.mf

.PHONY: all

all: pfsampleshrep 

pfsampleshrep: ../../grammar/adpf_nonamb.gap ../../../gapc
	../../../gapc -i $@ -t  $< --sample -o $@.cc
	sed -i 's|cat $$(RTLIB)/generic_main.cc|cat main.cc|' $@.mf
	make -f $@.mf CXXFLAGS_EXTRA=" -I../../gapc_filter -ffast-math"
	mv $@ main
		
clean:
	rm -f pfsampleshrep_main.cc pfsampleshrep_main.d pfsampleshrep_main.o pfsampleshrep.o pfsampleshrep.d pfsampleshrep.cc pfsampleshrep.mf pfsampleshrep.hh main main.o string.o string.d

