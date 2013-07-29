CPM=cpm
PROJ=Auction

default:
	$(CPM) project $(PROJ).prj build

force:
	$(CPM) project $(PROJ).prj build --force

clean:
	rm -rf "Clean System Files" $(PROJ).exe $(PROJ).prp

debug:
	cocl-tonic -P . Auction.icl

.PHONY: clean debug default force
