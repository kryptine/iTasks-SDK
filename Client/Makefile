JSFILES=$(shell find app/ -type f -name '*.js')
BUILDCMD=sencha app build

default: testing

testing: $(JSFILES)
	$(BUILDCMD) -e testing
	cp build/itwc/testing/all-classes.js .

production: $(JSFILES)
	$(BUILDCMD)
	cp build/itwc/production/all-classes.js .

clean:
	rm -rf build

.PHONY: clean
