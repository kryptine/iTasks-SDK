INSTANCES?=5

.SECONDARY: AsyncShareTest.prj

build: AsyncShareTest $(foreach v,$(shell seq $(INSTANCES)),instances/$v/AsyncShareTest)

%: %.prj %.icl
	cpm make

%.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks-git
	cpm project $@ set -h 2000M -s 20M -dynamics

%/AsyncShareTest: AsyncShareTest
	mkdir -p $(dir $@)
	cp $< $@
	cp -r $<-www $@-www

clean:
	$(RM) -r instances Clean\ System\ Files AsyncShareTest $(addprefix AsyncShareTest,-www -sapl -data)

clobber:
	$(RM) AsyncShareTest.prj
