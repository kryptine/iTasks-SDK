CPM=cpm
PROJ=Auction

default:
	$(CPM) project $(PROJ).prj build
	cp "Clean System Files/tonic-Auction.dot" .
	cp "Clean System Files/tonic-Auction.json" .

force:
	$(CPM) project $(PROJ).prj build --force

clean:
	rm -rf "Clean System Files" $(PROJ).exe $(PROJ).prp

debug:
	cocl-tonic -P . $(PROJ).icl

rebuild:
	make clean && make

.PHONY: clean debug default force rebuild
