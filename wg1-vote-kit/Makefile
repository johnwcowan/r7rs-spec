
.PHONY: all clean extract

all: WG1BallotResults

extract:
	./extract_ballots.sh

clean:
	rm -f *~ WG1Ballot* WG1ReBallot*

WG1BallotResults: WG1Ballot
	./write-ballot.pl $< > $@
