#!/usr/bin/env perl
#
# test_csv.pl - Test MPC designation conversion against CSV test data
#
# Usage: perl test_csv.pl <csv_file>
#

use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../src";
use MPC::Designation qw(convert_simple);
use Time::HiRes qw(time);

my $total_tests = 0;
my $passed_tests = 0;
my $failed_tests = 0;
my @errors;
my $max_errors = 100;

sub run_tests {
    my ($csv_file) = @_;

    open my $fh, '<', $csv_file or die "Cannot open file: $csv_file\n";

    my $start_time = time();

    # Skip header
    <$fh>;

    while (my $line = <$fh>) {
        chomp $line;
        next unless $line;

        my @parts = split /,/, $line;
        next unless @parts >= 2;

        my $unpacked = $parts[0];
        my $expected_packed = $parts[1];

        $total_tests++;

        eval {
            my $got_packed = convert_simple($unpacked);
            if ($got_packed ne $expected_packed) {
                $failed_tests++;
                if (@errors < $max_errors) {
                    push @errors, [$unpacked, $got_packed, $expected_packed];
                }
            } else {
                $passed_tests++;
            }
        };
        if ($@) {
            $failed_tests++;
            my $err = $@;
            $err =~ s/\n$//;
            if (@errors < $max_errors) {
                push @errors, [$unpacked, "ERROR: $err", $expected_packed];
            }
        }

        if ($total_tests % 100000 == 0) {
            print "Processed $total_tests entries...\n";
        }
    }

    close $fh;

    my $elapsed = time() - $start_time;
    my $elapsed_ms = int($elapsed * 1000);
    my $rate = $total_tests > 0 ? $total_tests / $elapsed : 0;

    print "\n";
    print "=== Test Results ===\n";
    print "Total:  $total_tests\n";
    print "Passed: $passed_tests\n";
    print "Failed: $failed_tests\n";
    printf "Time:   %dms (%.1f entries/sec)\n", $elapsed_ms, $rate;

    if ($failed_tests > 0 && @errors) {
        print "\n=== First " . scalar(@errors) . " failures ===\n";
        for my $err (@errors) {
            printf "%-25s %-15s %-15s\n", @$err;
        }
    }
}

# Main
if (@ARGV < 1) {
    print STDERR "Usage: test_csv.pl <csv_file>\n";
    exit 1;
}

run_tests($ARGV[0]);
exit($failed_tests > 0 ? 1 : 0);
