#!/bin/sh

if [ -z "$1" ]; then
  DB=backup/db/trac.db
else
  DB="$1"
fi

#BALLOTS=`sqlite3 $DB "select name from wiki where name like 'WG1Ballot%' group by name"`
BALLOTS="WG1Ballot WG1BallotCowan WG1BallotDurusau WG1BallotGanz WG1BallotGleckler WG1BallotHarvey WG1BallotHsu WG1BallotLucier WG1BallotMedernach WG1BallotRead WG1BallotResults WG1BallotRush WG1BallotRussel WG1BallotShinn WG1BallotShivers WG1BallotSnellPym WG1BallotSussman"

for b in $BALLOTS; do
#  sqlite3 $DB "select text from wiki where name='$b' group by name order by version" | tr -d '\r' > $b
    curl "http://trac.sacrideo.us/wg/wiki/$b?format=txt" | tr -d '\r' > $b
done

