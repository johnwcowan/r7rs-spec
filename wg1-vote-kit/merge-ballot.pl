#! /usr/bin/env perl

use strict;
use warnings;

my $ticket;
my $in_rationale;
my %votes;
my %rationales;

# To merge from multiple vote files:
#   merge-ballot.pl <( merge-ballot.pl <template> <votes1> ) <votes2>
# etc.

die "usage: $0 <template> <votes>" unless $#ARGV == 1;

open(IN, $ARGV[1]) or die "couldn't open $ARGV[1]: $!";
while (<IN>) {
  $in_rationale = 0 if /^[-=]+/;
  if (/^=== #?(\d+) .*===\s*$/) {
    $ticket = int($1);
  } elsif (/^\s*\*\s*'*Preferences:\s*'*([^'].*\S.*)/i) {
    $in_rationale = 1;
    $votes{$ticket} = $1;
  } elsif ($in_rationale and not /^\s*\*\s*'+\w*:\s*'+/i) {
    $rationales{$ticket} .= $_;
  }
}
close(IN);

undef $ticket;

open(IN, $ARGV[0]) or die "couldn't open $ARGV[0]: $!";
while (<IN>) {
  if (/^=+/ and $ticket and exists $rationales{$ticket}) {
    $rationales{$ticket} =~ s/^\s*//;
    print $rationales{$ticket};
    undef $ticket;
  }
  if (/^=== #?(\d+) .*===\s*$/) {
    $ticket = int($1);
  } else {
    s{(^\s*\*\s*'*Preferences:\s*'*)(.*)}{$1.(exists $votes{$ticket} ? $votes{$ticket} : $2)}ei;
  }
  print;
}
close(IN);

if ($ticket and exists $rationales{$ticket}) {
  print $rationales{$ticket};
}
