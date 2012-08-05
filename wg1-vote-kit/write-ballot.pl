#! /usr/bin/env perl

# http://xkcd.com/224/

# Usage: write-ballot.pl < WG1Ballot > WG1BallotResults
# (must have extracted WG1BallotName files with extract_ballots.sh)

use strict;
use warnings;

use constant MAX_RATIONALE_LENGTH => 768;

# Per-ticket aliases found on the fly.
my %aliases;

# Fixed aliases for English synonyms.
my %all_aliases =    (neither => 'no',
		      none => 'no');

# Fixed aliases added as the options changed.  Mostly mapping a naive
# 'yes' option to a sensible default as more options are added.
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
                      460 => {samebits => 'same-bits'},
                    );

my $ticket;
my $in_proposals;
my $in_rationale;
my $proposal_depth;

my %votes;
my %rationales;
my %options;
my %defaults;

########################################################################
# load user votes

sub normalize_option {
  my $option = lc shift;
  return $aliases{$ticket}{$option}
    || $ticket_aliases{$ticket}{$option}
      || $all_aliases{$option}
	|| $option;
}

sub load_votes {
  my ($user, $file) = @_;
  undef $ticket;
  open(IN, '<', $file);
  while (<IN>) {
    $in_rationale = 0 if /^[-=]+/;
    if (/^\s*\*\s*'*Preferences:\s*'*\s*([^']\S.*)/i) {
      $in_rationale = 1;
      my @prefs = map {normalize_option($_)} split(/\s*,\s*/, lc $1);
      next if $#prefs == 0 and $prefs[0] =~ /\babstain\b/i;
      # handle a|b|... equal votes
      map {$_ = "($_)" if s/\|/ /g} @prefs;
      @prefs = grep /\S/, @prefs;
      # handle foo/bar variations with no foo
      my @prefs2;
      foreach (@prefs) {
        push @prefs2, $_;
        if (m{([^()|/]*)/([^()|/]*)}) {
          my ($base, $variation) = ($1, $2);
          if (exists $options{$ticket}{$base}
              and not grep(/(^|,)\s*\Q$base\E\s*(,|$)/, (@prefs, @prefs2))) {
            push @prefs2, $base;
          }
        }
      }
      $votes{$ticket}{$user} = [@prefs2] if @prefs2;
    } elsif (/^\s*\*\s*'*Options:\s*'*\s*([^']\S.*)/i) {
      $options{$ticket}{$_}++ for split(/\s*,\s*/, lc $1);
    } elsif (/^(\s*)\*\s*'*Proposals:\s*'*/i) {
      $in_proposals = 1;
      $proposal_depth = length($1);
    } elsif ($in_proposals
             and /^(\s*)\*\s*'*([-\w\d]+):\s*'*\s*([-\w\d]+)/) {
      if (length($1) > $proposal_depth) {
        $aliases{$ticket}{lc $3} = lc $2;
      } else {
        $in_proposals = 0;
      }
    } elsif (/^=== #?(\d+) .*?(=*)\s*$/) {
      $ticket = int($1);
      $in_proposals = 0;
      warn "missing closing === in header: $_" unless $2 eq "===";
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

# load initial ballots
for my $file (glob("WG1Ballot[A-Z]*")) {
  next if $file =~ /~$/i;
  (my $user = $file) =~ s/^WG1Ballot//i;
  next if not $user or $user =~ /Result/;
  load_votes($user, $file);
}

# load update ballots
for my $file (glob("WG1ReBallot[A-Z]*")) {
  next if $file =~ /~$/i;
  (my $user = $file) =~ s/^WG1ReBallot//i;
  next if not $user or $user =~ /Result/;
  load_votes($user, $file);
}

########################################################################

sub get_index {
  my ($array, $elt) = @_;
  my @array = @$array;
  for (my $i=0; $i <= $#array; $i++) {
    return $i if $array[$i] eq $elt;
  }
  return -1;
}

sub get_voters {
  my $votes = shift;
  my @voters = grep {ref $votes->{$_} and @{$votes->{$_}}} sort keys %$votes;
  return join("\n", map {"    * [wiki:WG1Ballot$_ $_]: "
                           . join(", ", @{$votes->{$_}})}
              @voters)
    . "\n";
}

sub get_result {
  my ($votes, $default) = @_;
  return "" unless %$votes;
  my $input = "(";
  for my $user (keys %$votes) {
    $input .= "($user " . join(" ", @{$votes->{$user}}) . ")";
  }
  $input .= ")";
  my $result = `echo "$input" | ./ranked-pairs.scm`;
  (my $ratios = $result) =~ s/.*?\)\s*\(//;
  my @ratios;
  push @ratios, "$1:$2"
    while ($ratios =~ /\((\d+)\s+(\d+)\)/g);
  $result =~ s/\)\s*\(.*//;
  $result =~ s/[()]//g;
  my @results = split(/\s+/, $result);
  my $default_index = defined $default ? get_index(\@results, $default) : -1;
  # print STDERR "#$ticket  \nresults: ".join(", ", @results)."\n  ratios: ".
  #   join(", ", @ratios)."\n  default: $default_index".
  #   (($default_index>=0)?" => $results[$default_index]":"")."\n";
  if (($default_index > 0) and ($ratios[$default_index-1] =~ /(\d+):(\d+)/)) {
    if ($results[0] eq "undecided" or $results[0] eq "wg2") {
      # for an undecided winner, choose the default
      # print STDERR "  undecided/wg2 won\n";
      $results[0] = "''$results[0]''";
      $results[$default_index] = "'''$results[$default_index]'''";
    } elsif ($1*2 > scalar keys %$votes) {
      # we have a majority of all voters over the default, bold the result
      # print STDERR "  strong majority: $1*2 > ".scalar(keys %$votes)."\n";
      $results[0] = "'''$results[0]'''";
    } elsif ($1 > $2) {
      # we won pairwise over the default, italicize the result
      # print STDERR "  weak majority: $1 > $2\n";
      $results[0] = "''$results[0]''";
    } else {
      # otherwise use italics for the default
      # print STDERR "  use default\n";
      $results[$default_index] = "''$results[$default_index]''";
    }
  } else {
    # no default or the default won
    # print STDERR "  ".(($default_index == 0)?"default won":"no default")."\n";
    $results[0] = "'''$results[0]'''";
  }
  return "  * '''Results:''' ".join(", ", @results)."\n"
    . "  * '''Ratios:''' ".join(", ", @ratios)."\n";
}

sub format_rationale {
  my $str = shift;
  $str =~ s/[\t\n]/ /g;
  $str =~ s/  +/ /g;
  $str =~ s/ +$//;
  $str =~ s/^\s*//;
  $str =~ s/^('{2,3})?Rationale:\s*('{2,3})?\s*//;
  if (length($str) > MAX_RATIONALE_LENGTH) {
    my $voter = shift;
    warn "truncating long rational for $voter";
    $str = substr($str, 0, MAX_RATIONALE_LENGTH) .
      "[wiki:WG1Ballot$voter ...]";
  }
  return $str;
}

########################################################################
# Run through the official ballot, inserting results.

my $section = "";
my $result_notes = <<EOF;
= Notes about Results =

See [wiki:WG1BallotExplanation WG1BallotExplanation].

EOF

while (<>) {
  if (/^\s*\*\s*'*Preferences:\s*'*/i) {
    my $votes = $votes{$ticket};
    if (ref $votes) {
      print "  * '''Voters:''' \n".get_voters($votes);
      print get_result($votes, $defaults{$ticket});
      if (ref $rationales{$ticket}) {
        print "  * '''Rationales:'''\n\n";
        for my $voter (sort keys %{$rationales{$ticket}}) {
          print " `${voter}`::\n  "
            . format_rationale($rationales{$ticket}{$voter}, $voter)
            . "\n";
        }
      }
    }
  } elsif (/^\s*\*\s*'*Default:\s*'*\s*(\S+)/i) {
    print;
    $defaults{$ticket} ||= normalize_option($1);
  } else {
    if (/^=\s+(.*?)\s*=\s*$/) {
      # Update section, replacing instructions with result notes.
      print $result_notes if lc $section eq "instructions";
      $section = $1;
    }
    print unless lc $section eq "instructions";
    if (/^===\s+#?(\d+) .*?(=*)\s*$/) {
      $ticket = int($1);
      warn "missing closing === in header: $_" unless $2 eq "===";
    }
  }
}
