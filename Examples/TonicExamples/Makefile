CPM=cpm
PROJ=TonicExamples

default:
	$(CPM) project $(PROJ).prj build

force:
	$(CPM) project $(PROJ).prj build --force

clean:
	rm -rf "Clean System Files" $(PROJ).exe $(PROJ).prp sapl $(PROJ)-data tonic

debug:
	cocl-tonic -P . $(PROJ).icl

rebuild:
	make clean && make

.PHONY: clean debug default force rebuild
