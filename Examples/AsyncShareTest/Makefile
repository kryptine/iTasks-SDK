SHELL = /bin/zsh
INSTANCES = 5

build:
	cpm make;
	rm -rf instances;
	for n in {0..${INSTANCES}} ; do \
		mkdir -p instances/$$n;\
		cp AsyncShareTest instances/$$n/;\
		cp -r AsyncShareTest-www instances/$$n/;\
	done

clean:
	rm -rf instances;
	rm -rf Clean\ System\ Files;
	rm AsyncShareTest;
	rm -rf AsyncShareTest-www;