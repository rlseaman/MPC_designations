#!/usr/bin/env perl
#
# mpc_designation.pl - CLI for MPC designation converter
#
# Usage: mpc_designation.pl [-v|--verbose] <designation> [designation ...]
#

use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin";
use MPC::Designation qw(convert);

sub print_usage {
    print STDERR <<'EOF';
Usage: mpc_designation.pl [-v|--verbose] <designation> [designation ...]

Convert between packed and unpacked MPC designations.
Auto-detects the input format and converts to the other.

Options:
  -v, --verbose   Show detailed information about the conversion
  --version       Show version information

Examples:
  mpc_designation.pl 00001             -> 1
  mpc_designation.pl 1                 -> 00001
  mpc_designation.pl J95X00A           -> 1995 XA
  mpc_designation.pl '1995 XA'         -> J95X00A
  mpc_designation.pl 'C/1995 O1'       -> CJ95O010
  mpc_designation.pl 1P                -> 0001P
EOF
}

my $verbose = 0;
my @designations;

for my $arg (@ARGV) {
    if ($arg eq '-v' || $arg eq '--verbose') {
        $verbose = 1;
    } elsif ($arg eq '-h' || $arg eq '--help') {
        print_usage();
        exit 0;
    } elsif ($arg eq '--version') {
        print "mpc_designation $MPC::Designation::VERSION\n";
        exit 0;
    } else {
        push @designations, $arg;
    }
}

if (!@designations) {
    print_usage();
    exit 1;
}

my $multiple = @designations > 1;

for my $des (@designations) {
    eval {
        my $result = convert($des);
        my $info = $result->{info};
        my $output = $result->{output};

        if ($verbose) {
            print "  Input:    $des\n";
            print "  Detected: $info->{format} format, $info->{subtype}\n";
            my $action = $info->{format} eq 'packed'
                ? 'unpacking to human-readable form'
                : 'packing to MPC compact form';
            print "  Action:   $action\n";
            print "  Output:   $output\n";
            print "\n" if $multiple;
        } elsif ($multiple) {
            print "$des -> $output\n";
        } else {
            print "$output\n";
        }
    };
    if ($@) {
        my $err = $@;
        $err =~ s/\n$//;
        print STDERR "Error: $err\n";
        exit 1;
    }
}
