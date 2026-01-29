#!/usr/bin/env perl
#
# test_roundtrip.pl - Bidirectional testing with round-trip verification
#
# Tests:
# 1. Pack direction (unpacked -> packed) with timing
# 2. Unpack direction (packed -> unpacked) with timing
# 3. Unpacked round-trip: unpack(pack(x)) = x
# 4. Packed round-trip: pack(unpack(y)) = y
#
# Usage: perl test_roundtrip.pl <csv_file>
#

use strict;
use warnings;
use Time::HiRes qw(time);
use FindBin qw($Bin);
use lib "$Bin/../src";
use MPC::Designation qw(convert_simple);

my $MAX_ERRORS = 20;

sub main {
    die "Usage: $0 <csv_file>\n" unless @ARGV >= 1;
    my $csv_file = $ARGV[0];

    # Load test data
    open my $fh, '<', $csv_file or die "Cannot open $csv_file: $!";
    my $header = <$fh>;  # Skip header

    my @unpacked_list;
    my @packed_list;

    while (my $line = <$fh>) {
        chomp $line;
        next unless $line;
        my @parts = split /,/, $line, 3;
        next unless @parts >= 2;
        push @unpacked_list, $parts[0];
        push @packed_list, $parts[1];
    }
    close $fh;

    my $total = scalar @unpacked_list;
    print "Loaded $total test cases\n\n";

    my @errors;

    # ========== Phase 1: Pack (unpacked -> packed) ==========
    print "=== Phase 1: Pack (unpacked -> packed) ===\n";
    my $pack_passed = 0;
    my $pack_failed = 0;
    my $start = time();

    for my $i (0 .. $total - 1) {
        my $unpacked = $unpacked_list[$i];
        my $expected = $packed_list[$i];

        my $got = eval { convert_simple($unpacked) };
        if ($@) {
            $pack_failed++;
            push @errors, ['pack', $unpacked, "ERROR: $@", $expected] if @errors < $MAX_ERRORS;
        } elsif ($got ne $expected) {
            $pack_failed++;
            push @errors, ['pack', $unpacked, $got, $expected] if @errors < $MAX_ERRORS;
        } else {
            $pack_passed++;
        }
    }

    my $pack_time = int((time() - $start) * 1000);
    my $pack_rate = $total / ($pack_time / 1000);
    print "Passed: $pack_passed\n";
    print "Failed: $pack_failed\n";
    printf "Time:   %dms (%.1f entries/sec)\n\n", $pack_time, $pack_rate;

    # ========== Phase 2: Unpack (packed -> unpacked) ==========
    print "=== Phase 2: Unpack (packed -> unpacked) ===\n";
    my $unpack_passed = 0;
    my $unpack_failed = 0;
    $start = time();

    for my $i (0 .. $total - 1) {
        my $packed = $packed_list[$i];
        my $expected = $unpacked_list[$i];

        my $got = eval { convert_simple($packed) };
        if ($@) {
            $unpack_failed++;
            push @errors, ['unpack', $packed, "ERROR: $@", $expected] if @errors < $MAX_ERRORS;
        } elsif ($got ne $expected) {
            $unpack_failed++;
            push @errors, ['unpack', $packed, $got, $expected] if @errors < $MAX_ERRORS;
        } else {
            $unpack_passed++;
        }
    }

    my $unpack_time = int((time() - $start) * 1000);
    my $unpack_rate = $total / ($unpack_time / 1000);
    print "Passed: $unpack_passed\n";
    print "Failed: $unpack_failed\n";
    printf "Time:   %dms (%.1f entries/sec)\n\n", $unpack_time, $unpack_rate;

    # ========== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ==========
    print "=== Phase 3: Unpacked round-trip: unpack(pack(x)) = x ===\n";
    my $rt_unpacked_passed = 0;
    my $rt_unpacked_failed = 0;
    $start = time();

    for my $i (0 .. $total - 1) {
        my $original = $unpacked_list[$i];

        my ($packed, $unpacked);
        eval {
            $packed = convert_simple($original);
            $unpacked = convert_simple($packed);
        };
        if ($@) {
            $rt_unpacked_failed++;
            next;
        }

        if ($unpacked ne $original) {
            $rt_unpacked_failed++;
            push @errors, ['rt-unp', $original, "$packed -> $unpacked", $original] if @errors < $MAX_ERRORS;
        } else {
            $rt_unpacked_passed++;
        }
    }

    my $rt_unpacked_time = int((time() - $start) * 1000);
    my $rt_unpacked_rate = $total / ($rt_unpacked_time / 1000);
    print "Passed: $rt_unpacked_passed\n";
    print "Failed: $rt_unpacked_failed\n";
    printf "Time:   %dms (%.1f entries/sec)\n\n", $rt_unpacked_time, $rt_unpacked_rate;

    # ========== Phase 4: Packed round-trip: pack(unpack(y)) = y ==========
    print "=== Phase 4: Packed round-trip: pack(unpack(y)) = y ===\n";
    @errors = ();  # Reset to show phase 4 errors
    my $rt_packed_passed = 0;
    my $rt_packed_failed = 0;
    $start = time();

    for my $i (0 .. $total - 1) {
        my $original = $packed_list[$i];

        my ($unpacked, $repacked);
        eval {
            $unpacked = convert_simple($original);
            $repacked = convert_simple($unpacked);
        };
        if ($@) {
            $rt_packed_failed++;
            push @errors, ['rt-pak', $original, "ERROR: $@", $original] if @errors < $MAX_ERRORS;
            next;
        }

        if ($repacked ne $original) {
            $rt_packed_failed++;
            push @errors, ['rt-pak', $original, "$unpacked -> $repacked", $original] if @errors < $MAX_ERRORS;
        } else {
            $rt_packed_passed++;
        }
    }

    my $rt_packed_time = int((time() - $start) * 1000);
    my $rt_packed_rate = $total / ($rt_packed_time / 1000);
    print "Passed: $rt_packed_passed\n";
    print "Failed: $rt_packed_failed\n";
    printf "Time:   %dms (%.1f entries/sec)\n\n", $rt_packed_time, $rt_packed_rate;

    # ========== Summary ==========
    print "=== Summary ===\n";
    printf "%-30s %10s %18s %12s\n", "Phase", "Time (ms)", "Rate (entries/sec)", "Status";
    printf "%-30s %10s %18s %12s\n", "-" x 30, "-" x 10, "-" x 18, "-" x 12;

    sub status {
        my $failed = shift;
        return "PASS" if $failed == 0;
        return "FAIL ($failed)";
    }

    printf "%-30s %10d %18.1f %12s\n", "Pack", $pack_time, $pack_rate, status($pack_failed);
    printf "%-30s %10d %18.1f %12s\n", "Unpack", $unpack_time, $unpack_rate, status($unpack_failed);
    printf "%-30s %10d %18.1f %12s\n", "Unpacked RT: unpack(pack(x))=x", $rt_unpacked_time, $rt_unpacked_rate, status($rt_unpacked_failed);
    printf "%-30s %10d %18.1f %12s\n", "Packed RT: pack(unpack(y))=y", $rt_packed_time, $rt_packed_rate, status($rt_packed_failed);
    print "\n";

    # Show errors
    if (@errors) {
        print "=== First " . scalar(@errors) . " errors ===\n";
        printf "%-8s %-25s %-20s %-20s\n", "Phase", "Input", "Got", "Expected";
        printf "%-8s %-25s %-20s %-20s\n", "-" x 8, "-" x 25, "-" x 20, "-" x 20;
        for my $e (@errors) {
            printf "%-8s %-25s %-20s %-20s\n", @$e;
        }
    }

    # Exit with error only if pack or packed RT failed
    my $total_failed = $pack_failed + $rt_packed_failed;
    exit($total_failed > 0 ? 1 : 0);
}

main();
