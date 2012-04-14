
.PHONY: all clean extract

all: extract WG1BallotResults

extract:
	./extract_ballots.sh

clean:
	rm -f *~ WG1Ballot* WG1ReBallot*

WG1Ballot:
	curl -s "http://trac.sacrideo.us/wg/report/1?format=csv&sort=component&asc=1" | ./generate-ballot.scm > $@

WG1BallotResults:
	./write-ballot.pl WG1Ballot > $@
