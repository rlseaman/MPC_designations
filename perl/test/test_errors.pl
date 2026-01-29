#!/usr/bin/env perl
#
# test_errors.pl - Test MPC designation error handling
#
# Usage: perl test_errors.pl <error_test_cases.csv>
#

use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../src";
use MPC::Designation qw(convert);

my $total_tests = 0;
my $passed_tests = 0;
my $failed_tests = 0;

# Unescape special characters in test input
sub unescape_input {
    my ($input) = @_;
    $input =~ s/\\t/\t/g;
    $input =~ s/\\n/\n/g;
    $input =~ s/\\r/\r/g;
    $input =~ s/\\f/\f/g;
    $input =~ s/\\v/\x0B/g;
    $input =~ s/\\x([0-9A-Fa-f]{2})/chr(hex($1))/ge;
    return $input;
}

sub run_tests {
    my ($csv_file) = @_;

    open my $fh, '<', $csv_file or die "Cannot open file: $csv_file\n";

    print "=== MPC Designation Error Tests ===\n\n";

    while (my $line = <$fh>) {
        chomp $line;

        # Skip empty lines, comments, and header
        next if $line =~ /^\s*$/;
        next if $line =~ /^#/;
        next if $line =~ /^category,/;

        # Parse CSV (simple split, handles most cases)
        my @parts = split /,/, $line, 5;
        next unless @parts >= 5;

        my ($category, $subcategory, $raw_input, $expected_error, $description) = @parts;

        # Unescape the input
        my $input = unescape_input($raw_input);

        $total_tests++;

        # Try to convert
        my $got_error = 0;
        my $error_type = '';
        my $output = '';

        eval {
            my $result = convert($input);
            $output = $result->{output};
        };
        if ($@) {
            $got_error = 1;
            my $err = $@;
            if ($err =~ /out of range/i) {
                $error_type = 'range';
            } else {
                $error_type = 'format';
            }
        }

        # Check result
        my $test_id = "$category/$subcategory";

        if ($expected_error eq 'valid') {
            # Should succeed
            if ($got_error) {
                $failed_tests++;
                print "FAIL [$test_id]: '$description'\n";
                print "      Expected: valid conversion\n";
                print "      Got:      error ($error_type)\n";
            } else {
                $passed_tests++;
            }
        } else {
            # Should fail
            if (!$got_error) {
                $failed_tests++;
                print "FAIL [$test_id]: '$description'\n";
                print "      Expected: error ($expected_error)\n";
                print "      Got:      '$output' (success)\n";
            } else {
                $passed_tests++;
            }
        }
    }

    close $fh;

    print "\n=== Error Test Results ===\n";
    print "Total:  $total_tests\n";
    print "Passed: $passed_tests\n";
    print "Failed: $failed_tests\n";
}

# Main
if (@ARGV < 1) {
    print STDERR "Usage: test_errors.pl <error_test_cases.csv>\n";
    exit 1;
}

run_tests($ARGV[0]);
exit($failed_tests > 0 ? 1 : 0);
