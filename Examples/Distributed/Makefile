INSTANCES?=5

.SECONDARY: minimal.prj

build: minimal $(foreach v,$(shell seq $(INSTANCES)),instances/$v/minimal)

%: %.prj %.icl
	cpm make

%.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks-dist
	cpm project $@ set -h 2000M -s 20M -dynamics

%/minimal: minimal
	mkdir -p $(dir $@)
	cp $< $@
	cp -r $<-www $@-www

clean:
	$(RM) -r instances Clean\ System\ Files minimal $(addprefix minimal,-www -sapl -data)

clobber:
	$(RM) minimal.prj
