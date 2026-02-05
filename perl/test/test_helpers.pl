#!/usr/bin/env perl
# Test helper functions in MPC designation converter.
# Tests format conversion (minimal <-> 12-char report format),
# fragment extraction, and designation comparison functions.

use strict;
use warnings;
use FindBin qw($Bin);
use lib "$Bin/../src";
use MPC::Designation qw(
    to_report_format from_report_format
    has_fragment get_fragment get_parent designations_equal
);

my $passed = 0;
my $failed = 0;

sub test_to_report {
    my ($input, $expected, $desc) = @_;
    eval {
        my $result = to_report_format($input);
        if ($result eq $expected) {
            print "  PASS: to_report_format(\"$input\") -> \"$result\"\n";
            $passed++;
        } else {
            print "  FAIL: to_report_format(\"$input\"): expected \"$expected\", got \"$result\"\n";
            $failed++;
        }
    };
    if ($@) {
        print "  FAIL: to_report_format(\"$input\"): error - $@";
        $failed++;
    }
}

sub test_from_report {
    my ($input, $expected, $desc) = @_;
    eval {
        my $result = from_report_format($input);
        if ($result eq $expected) {
            print "  PASS: from_report_format(\"$input\") -> \"$result\"\n";
            $passed++;
        } else {
            print "  FAIL: from_report_format(\"$input\"): expected \"$expected\", got \"$result\"\n";
            $failed++;
        }
    };
    if ($@) {
        print "  FAIL: from_report_format(\"$input\"): error - $@";
        $failed++;
    }
}

sub test_has_fragment {
    my ($input, $expected, $desc) = @_;
    my $result = has_fragment($input);
    my $expected_int = $expected ? 1 : 0;
    if ($result == $expected_int) {
        print "  PASS: has_fragment(\"$input\") -> $result\n";
        $passed++;
    } else {
        print "  FAIL: has_fragment(\"$input\"): expected $expected_int, got $result\n";
        $failed++;
    }
}

sub test_get_fragment {
    my ($input, $expected, $desc) = @_;
    eval {
        my $result = get_fragment($input);
        if ($result eq $expected) {
            print "  PASS: get_fragment(\"$input\") -> \"$result\"\n";
            $passed++;
        } else {
            print "  FAIL: get_fragment(\"$input\"): expected \"$expected\", got \"$result\"\n";
            $failed++;
        }
    };
    if ($@) {
        print "  FAIL: get_fragment(\"$input\"): error - $@";
        $failed++;
    }
}

sub test_get_parent {
    my ($input, $expected, $desc) = @_;
    eval {
        my $result = get_parent($input);
        if ($result eq $expected) {
            print "  PASS: get_parent(\"$input\") -> \"$result\"\n";
            $passed++;
        } else {
            print "  FAIL: get_parent(\"$input\"): expected \"$expected\", got \"$result\"\n";
            $failed++;
        }
    };
    if ($@) {
        print "  FAIL: get_parent(\"$input\"): error - $@";
        $failed++;
    }
}

sub test_equal {
    my ($d1, $d2, $expected, $desc) = @_;
    my $result = designations_equal($d1, $d2);
    my $expected_int = $expected ? 1 : 0;
    if ($result == $expected_int) {
        print "  PASS: designations_equal(\"$d1\", \"$d2\") -> $result\n";
        $passed++;
    } else {
        print "  FAIL: designations_equal(\"$d1\", \"$d2\"): expected $expected_int, got $result\n";
        $failed++;
    }
}

print "=== MPC Designation Helper Function Tests (Perl) ===\n\n";

# Test to_report_format
print "--- to_report_format ---\n";

# Numbered asteroids
test_to_report('00001', '       00001', 'Numbered asteroid 1');
test_to_report('00433', '       00433', 'Numbered asteroid 433');
test_to_report('99999', '       99999', 'Numbered asteroid 99999');
test_to_report('A0000', '       A0000', 'Numbered asteroid 100000');
test_to_report('~0000', '       ~0000', 'Numbered asteroid 620000');

# Provisional asteroids
test_to_report('J95X00A', '     J95X00A', 'Provisional 1995 XA');
test_to_report('K24A12B', '     K24A12B', 'Provisional 2024 AB12');

# Survey designations
test_to_report('PLS2040', '     PLS2040', 'Survey P-L');
test_to_report('T3S3141', '     T3S3141', 'Survey T-3');

# Numbered comets
test_to_report('0001P', '0001P       ', 'Comet 1P');
test_to_report('0073P', '0073P       ', 'Comet 73P');

# Numbered comets with fragments
test_to_report('0073Pa', '0073P      a', 'Comet 73P-A');
test_to_report('0073Pb', '0073P      b', 'Comet 73P-B');
test_to_report('0073Paa', '0073P     aa', 'Comet 73P-AA');
test_to_report('0073Paz', '0073P     az', 'Comet 73P-AZ');
test_to_report('0073Pzz', '0073P     zz', 'Comet 73P-ZZ');

# Provisional comets
test_to_report('CJ95O010', '    CJ95O010', 'Comet C/1995 O1');
test_to_report('DJ93F020', '    DJ93F020', 'Comet D/1993 F2');
test_to_report('DJ93F02a', '    DJ93F02a', 'Comet D/1993 F2-A');

# Test from_report_format
print "\n--- from_report_format ---\n";

# Numbered asteroids
test_from_report('       00001', '00001', 'Numbered asteroid 1');
test_from_report('       00433', '00433', 'Numbered asteroid 433');
test_from_report('       A0000', 'A0000', 'Numbered asteroid 100000');

# Provisional asteroids
test_from_report('     J95X00A', 'J95X00A', 'Provisional 1995 XA');

# Numbered comets
test_from_report('0073P       ', '0073P', 'Comet 73P');

# Numbered comets with fragments
test_from_report('0073P      a', '0073Pa', 'Comet 73P-A');
test_from_report('0073P     aa', '0073Paa', 'Comet 73P-AA');
test_from_report('0073P     az', '0073Paz', 'Comet 73P-AZ');

# Provisional comets
test_from_report('    CJ95O010', 'CJ95O010', 'Comet C/1995 O1');

# Test has_fragment
print "\n--- has_fragment ---\n";

# Unpacked with fragments
test_has_fragment('73P-A', 1, 'Unpacked numbered comet with fragment');
test_has_fragment('73P-AA', 1, 'Unpacked numbered comet with 2-letter fragment');
test_has_fragment('D/1993 F2-A', 1, 'Unpacked provisional comet with fragment');
test_has_fragment('P/1930 J1-AA', 1, 'Unpacked provisional comet with 2-letter fragment');

# Unpacked without fragments
test_has_fragment('73P', 0, 'Unpacked numbered comet no fragment');
test_has_fragment('C/1995 O1', 0, 'Unpacked provisional comet no fragment');

# Packed with fragments
test_has_fragment('0073Pa', 1, 'Packed numbered comet with fragment');
test_has_fragment('0073Paa', 1, 'Packed numbered comet with 2-letter fragment');
test_has_fragment('DJ93F02a', 1, 'Packed provisional comet with fragment');

# Packed without fragments
test_has_fragment('0073P', 0, 'Packed numbered comet no fragment');
test_has_fragment('CJ95O010', 0, 'Packed provisional comet no fragment');

# Non-comets
test_has_fragment('1995 XA', 0, 'Asteroid no fragment');
test_has_fragment('00001', 0, 'Numbered asteroid');

# Test get_fragment
print "\n--- get_fragment ---\n";

# Unpacked with fragments
test_get_fragment('73P-A', 'A', 'Unpacked single fragment');
test_get_fragment('73P-AA', 'AA', 'Unpacked 2-letter fragment');
test_get_fragment('73P-I', 'I', 'Unpacked fragment I');
test_get_fragment('D/1993 F2-B', 'B', 'Unpacked provisional fragment');
test_get_fragment('P/1930 J1-AZ', 'AZ', 'Unpacked provisional 2-letter');

# Unpacked without fragments
test_get_fragment('73P', '', 'Unpacked no fragment');
test_get_fragment('C/1995 O1', '', 'Unpacked provisional no fragment');

# Packed with fragments
test_get_fragment('0073Pa', 'A', 'Packed single fragment');
test_get_fragment('0073Paa', 'AA', 'Packed 2-letter fragment');
test_get_fragment('0073Pi', 'I', 'Packed fragment I');
test_get_fragment('DJ93F02b', 'B', 'Packed provisional fragment');

# Packed without fragments
test_get_fragment('0073P', '', 'Packed no fragment');
test_get_fragment('CJ95O010', '', 'Packed provisional no fragment');

# Test get_parent
print "\n--- get_parent ---\n";

# Unpacked with fragments
test_get_parent('73P-A', '73P', 'Unpacked single fragment');
test_get_parent('73P-AA', '73P', 'Unpacked 2-letter fragment');
test_get_parent('D/1993 F2-B', 'D/1993 F2', 'Unpacked provisional fragment');
test_get_parent('P/1930 J1-AA', 'P/1930 J1', 'Unpacked provisional 2-letter');

# Unpacked without fragments
test_get_parent('73P', '73P', 'Unpacked no fragment');
test_get_parent('C/1995 O1', 'C/1995 O1', 'Unpacked provisional no fragment');

# Packed with fragments
test_get_parent('0073Pa', '0073P', 'Packed single fragment');
test_get_parent('0073Paa', '0073P', 'Packed 2-letter fragment');

# Packed without fragments
test_get_parent('0073P', '0073P', 'Packed no fragment');

# Non-comets (should return as-is)
test_get_parent('1995 XA', '1995 XA', 'Asteroid');
test_get_parent('00001', '00001', 'Numbered asteroid');

# Test designations_equal
print "\n--- designations_equal ---\n";

# Same designation, different formats
test_equal('1995 XA', 'J95X00A', 1, 'Provisional packed/unpacked');
test_equal('73P', '0073P', 1, 'Numbered comet packed/unpacked');
test_equal('73P-A', '0073Pa', 1, 'Comet with fragment packed/unpacked');
test_equal('73P-AA', '0073Paa', 1, 'Comet with 2-letter fragment');
test_equal('1', '00001', 1, 'Numbered asteroid');
test_equal('C/1995 O1', 'CJ95O010', 1, 'Provisional comet');

# Different designations
test_equal('1995 XA', '1995 XB', 0, 'Different provisional');
test_equal('73P-A', '73P-B', 0, 'Different fragments');
test_equal('73P', '74P', 0, 'Different comet numbers');
test_equal('1', '2', 0, 'Different asteroid numbers');

# Same designation (both packed or both unpacked)
test_equal('1995 XA', '1995 XA', 1, 'Same unpacked');
test_equal('J95X00A', 'J95X00A', 1, 'Same packed');

# Summary
print "\n==================================================\n";
print "Total: ", $passed + $failed, ", Passed: $passed, Failed: $failed\n";

exit($failed > 0 ? 1 : 0);
