#! /usr/bin/env perl

use strict;
use warnings;

my %aliases;
my %all_aliases =    (none => 'no');
my %ticket_aliases = (37 => {keep => 'yes'},
                      17 => {yes => 'srfi-23'},
                      6 => {r5rs => 'no',
                            r6rs => 'yes'},
                      9 => {yes => 'both'},
                      30 => {yes => 'srfi-6'},
                      11 => {r5rs => 'yes',
                             r6rs => 'no'},
                      38 => {yes => 'both'},
                      15 => {yes => 'both'},
                      26 => {yes => 'undecided', # sorry!
                             module => 'undecided'},
                      51 => {yes => 'equal?+srfi-1',
                             both => 'equal?+srfi-1'},
                      58 => {yes => 'r6rs',
                             s => 'single'},
                      52 => {yes => 'srfi-38'},
                    );

my $ticket;
my $in_proposals;
my $in_rationale;
my $proposal_depth;

my %votes;
my %rationales;

########################################################################
# load user votes

for my $file (glob("WG1Ballot[A-Z]*")) {
  next if $file =~ /^WG1BallotResult/i or $file =~ /~$/i;
  (my $user = $file) =~ s/^WG1Ballot//i;
  undef $ticket;
  open(IN, '<', $file);
  while (<IN>) {
    $in_rationale = 0 if /^[-=]+/;
    if (/^\s*\*\s*'*Preferences:\s*'*\s*([^']\S.*)/i) {
      $in_rationale = 1;
      my @prefs = map {$aliases{$_}
                         || $all_aliases{$_}
                           || $ticket_aliases{$ticket}{$_}
                             || $_}
        split(/\s*,\s*/, lc $1);
      next if $#prefs == 0 and $prefs[0] =~ /\babstain\b/i;
      # handle a|b|... equal votes
      map {$_ = "($_)" if s/\|/ /g} @prefs;
      @prefs = grep /\S/, @prefs;
      $votes{$ticket}{$user} = [@prefs] if @prefs;
    } elsif (/^(\s*)\*\s*'*Proposals:\s*'*/i) {
      $in_proposals = 1;
      $proposal_depth = length($1);
    } elsif ($in_proposals
             and /^(\s*)\*\s*'*([-\w\d]+):\s*'*\s*([-\w\d]+)/) {
      if (length($1) > $proposal_depth) {
        $aliases{lc $3} = lc $2;
      } else {
        $in_proposals = 0;
      }
    } elsif (/^=== #?(\d+) .*===\s*$/) {
      $ticket = int($1);
      $in_proposals = 0;
      %aliases = ();
      $votes{$ticket} = {} unless exists $votes{$ticket};
    } elsif ($ticket
             and $in_rationale
             and /\S/ and not /^\s*\*\s*'+\w*:\s*'+/i) {
      $rationales{$ticket} = {} unless exists $rationales{$ticket};
      $rationales{$ticket}{$user} .= $_;
    }
  }
  close(IN);
}

########################################################################

sub get_voters {
  my $votes = shift;
  my @voters = grep {ref $votes->{$_} and @{$votes->{$_}}} sort keys %$votes;
  return scalar(@voters) . ": "
    . join(", ", map {"[wiki:WG1Ballot$_ $_\]"} @voters);
}

sub get_result {
  my $votes = shift;
  return "" unless %$votes;
  my $input = "(";
  for my $user (keys %$votes) {
    $input .= "($user " . join(" ", @{$votes->{$user}}) . ")";
  }
  $input .= ")";
  my $result = `echo "$input" | ./ranked-pairs.scm`;
  #print "result: $result\n";
  (my $ratios = $result) =~ s/.*?\)\s*\(//;
  $result =~ s/\)\s*\(.*//;
  $result =~ s/[()]//g;
  my @ratios;
  push @ratios, "$1:$2"
    while ($ratios =~ /\((\d+)\s+(\d+)\)/g);
  return "  * '''Results:''' ".join(", ", split(/\s+/, $result))."\n"
    . "  * '''Ratios:''' ".join(", ", @ratios)."\n";
}

sub format_rationale {
  my $str = shift;
  $str =~ s/[\t\n]/ /g;
  $str =~ s/  +/ /g;
  $str =~ s/ +$//;
  $str =~ s/^ */ /;
  return $str;
}

########################################################################
# Run through the official ballot, inserting results.

while (<>) {
  if (/^\s*\*\s*'*Preferences:\s*'*/i) {
    my $votes = $votes{$ticket};
    if (ref $votes) {
      print "  * '''Voters:''' ".get_voters($votes)."\n";
      print get_result($votes);
      if (ref $rationales{$ticket}) {
        print "  * '''Rationales:'''\n\n";
        for my $user (sort keys %{$rationales{$ticket}}) {
          print " `${user}`::\n  "
            . format_rationale($rationales{$ticket}{$user})
            . "\n";
        }
      }
    }
  } else {
    print;
    $ticket = int($1) if /^=== #?(\d+) .*===\s*$/;
  }
}
