
.PHONY: all clean

all: WG1BallotResults

extract:
	./extract_ballots.sh

clean:
	rm -f *~ WG1Ballot* WG1ReBallot*

WG1Ballot: extract

WG1BallotResults:
	./write-ballot.pl WG1Ballot $< > $@
