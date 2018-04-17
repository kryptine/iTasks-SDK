INSTANCES?=5

.SECONDARY: RemoteShareExamples.prj

build: RemoteShareExamples $(foreach v,$(shell seq $(INSTANCES)),instances/$v/RemoteShareExamples)

%: %.prj %.icl
	cpm make

%.prj:
	cpm project $(basename $@) create
	cpm project $@ target iTasks-git
	cpm project $@ set -h 2000M -s 20M -dynamics

%/RemoteShareExamples: RemoteShareExamples
	mkdir -p $(dir $@)
	cp $< $@
	cp -r $<-www $@-www

clean:
	$(RM) -r instances Clean\ System\ Files RemoteShareExamples $(addprefix RemoteShareExamples,-www -sapl -data)

clobber:
	$(RM) RemoteShareExamples.prj
