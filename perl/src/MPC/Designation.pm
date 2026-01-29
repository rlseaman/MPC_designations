package MPC::Designation;

use strict;
use warnings;
use Exporter 'import';

our @EXPORT_OK = qw(
    pack unpack convert convert_simple detect_format
    pack_permanent unpack_permanent
    pack_provisional unpack_provisional
    pack_comet_numbered unpack_comet_numbered
    pack_comet_full unpack_comet_full
    pack_satellite unpack_satellite
);

our $VERSION = '1.0.0';

# Try to read version from file
BEGIN {
    for my $path ('VERSION', '../VERSION', '../../VERSION') {
        if (open my $fh, '<', $path) {
            my $v = <$fh>;
            close $fh;
            if ($v) {
                chomp $v;
                $VERSION = $v if $v =~ /^\d+\.\d+/;
            }
            last;
        }
    }
}

# =============================================================================
# Constants
# =============================================================================

# Base-62 character set
my $BASE62_CHARS = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';

# Century codes for provisional designations
my %CENTURY_CODES = (
    A => 10, B => 11, C => 12, D => 13, E => 14, F => 15,
    G => 16, H => 17, I => 18, J => 19, K => 20, L => 21
);
my %REVERSE_CENTURY_CODES = reverse %CENTURY_CODES;

# Survey codes
my %SURVEY_PACKED_TO_UNPACKED = (
    PLS => 'P-L',
    T1S => 'T-1',
    T2S => 'T-2',
    T3S => 'T-3'
);
my %SURVEY_UNPACKED_TO_PACKED = reverse %SURVEY_PACKED_TO_UNPACKED;

# Comet types
my %COMET_TYPES = map { $_ => 1 } qw(P C D X A I);

my %COMET_TYPE_DESCRIPTIONS = (
    P => 'periodic',
    C => 'non-periodic',
    D => 'defunct',
    X => 'uncertain orbit',
    A => 'asteroid with comet designation',
    I => 'interstellar'
);

# Planet codes for satellites
my %SATELLITE_PLANETS = map { $_ => 1 } qw(J S U N);
my %SATELLITE_PLANET_NAMES = (
    J => 'Jupiter',
    S => 'Saturn',
    U => 'Uranus',
    N => 'Neptune'
);

# Maximum asteroid number
my $MAX_ASTEROID_NUMBER = 15396335;

# =============================================================================
# Validation
# =============================================================================

sub validate_raw_input {
    my ($s) = @_;
    for my $c (split //, $s) {
        my $code = ord($c);
        if ($code < 32 || $code > 126) {
            die "Invalid character in designation: '$c'\n";
        }
    }
}

sub validate_whitespace {
    my ($s) = @_;
    my $prev_space = 0;
    for my $c (split //, $s) {
        my $code = ord($c);
        if ($code < 32 || $code > 126) {
            die "Invalid character in designation: '$c'\n";
        }
        if ($c eq ' ') {
            die "Consecutive spaces in designation\n" if $prev_space;
            $prev_space = 1;
        } else {
            $prev_space = 0;
        }
    }
}

sub is_valid_half_month {
    my ($letter) = @_;
    return $letter =~ /^[A-HJ-Y]$/;
}

# =============================================================================
# Base-62 encoding
# =============================================================================

sub base62_to_num {
    my ($char) = @_;
    my $idx = index($BASE62_CHARS, $char);
    die "Invalid base-62 character: $char\n" if $idx < 0;
    return $idx;
}

sub num_to_base62 {
    my ($num) = @_;
    die "Number out of base-62 range: $num\n" if $num < 0 || $num > 61;
    return substr($BASE62_CHARS, $num, 1);
}

sub base62_string_to_num {
    my ($s) = @_;
    my $result = 0;
    for my $char (split //, $s) {
        $result = $result * 62 + base62_to_num($char);
    }
    return $result;
}

sub num_to_base62_string {
    my ($num, $width) = @_;
    $width //= 4;
    my @result;
    for (1..$width) {
        unshift @result, num_to_base62($num % 62);
        $num = int($num / 62);
    }
    return join('', @result);
}

# =============================================================================
# Cycle count encoding
# =============================================================================

sub decode_cycle_count {
    my ($encoded) = @_;
    my $first = substr($encoded, 0, 1);
    my $second = substr($encoded, 1, 1);

    my $tens;
    if ($first =~ /\d/) {
        $tens = $first;
    } elsif ($first =~ /[A-Z]/) {
        $tens = ord($first) - 55;
    } elsif ($first =~ /[a-z]/) {
        $tens = ord($first) - 61;
    } else {
        die "Invalid cycle count encoding: $encoded\n";
    }

    die "Invalid cycle count encoding: $encoded\n" unless $second =~ /\d/;

    return $tens * 10 + $second;
}

sub encode_cycle_count {
    my ($count) = @_;
    die "Cycle count out of range (0-619): $count\n" if $count < 0 || $count >= 620;

    my $tens = int($count / 10);
    my $ones = $count % 10;

    my $first;
    if ($tens < 10) {
        $first = $tens;
    } elsif ($tens < 36) {
        $first = chr($tens + 55);
    } else {
        $first = chr($tens + 61);
    }

    return "$first$ones";
}

# =============================================================================
# Letter/position utilities
# =============================================================================

sub letter_to_position {
    my ($letter) = @_;
    my $pos = ord(uc $letter) - ord('A') + 1;
    $pos-- if uc($letter) gt 'I';
    return $pos;
}

sub position_to_letter {
    my ($pos) = @_;
    # Second letters A-Z excluding I = 25 positions (1-25)
    # A-H = positions 1-8, J-Z = positions 9-25 (I is skipped)
    die "Invalid letter position: $pos\n" if $pos < 1 || $pos > 25;
    $pos++ if $pos >= 9;
    return chr(ord('A') + $pos - 1);
}

# =============================================================================
# Permanent (numbered) asteroid designations
# =============================================================================

sub unpack_permanent {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;
    my $first = substr($packed, 0, 1);

    # Tilde format (>= 620,000)
    if ($first eq '~' && length($packed) == 5) {
        my $base62_part = substr($packed, 1, 4);
        return 620000 + base62_string_to_num($base62_part);
    }

    die "Invalid packed permanent designation length: $packed\n" unless length($packed) == 5;

    my $rest = substr($packed, 1, 4);

    if ($first =~ /\d/) {
        return int($packed);
    } elsif ($first =~ /[A-Z]/) {
        my $val = ord($first) - 55;
        return $val * 10000 + int($rest);
    } elsif ($first =~ /[a-z]/) {
        my $val = ord($first) - 61;
        return $val * 10000 + int($rest);
    } else {
        die "Invalid packed permanent designation: $packed\n";
    }
}

sub pack_permanent {
    my ($number) = @_;
    die "Invalid asteroid number: $number\n" if $number < 1 || $number > $MAX_ASTEROID_NUMBER;

    if ($number < 100000) {
        return sprintf("%05d", $number);
    } elsif ($number < 620000) {
        my $div = int($number / 10000);
        my $mod = $number % 10000;
        my $letter;
        if ($div < 36) {
            $letter = chr($div + 55);
        } else {
            $letter = chr($div + 61);
        }
        return sprintf("%s%04d", $letter, $mod);
    } else {
        my $offset = $number - 620000;
        return '~' . num_to_base62_string($offset);
    }
}

# =============================================================================
# Provisional asteroid designations
# =============================================================================

sub unpack_provisional {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;

    # Survey designations
    if (length($packed) == 7) {
        my $prefix = substr($packed, 0, 3);
        if (exists $SURVEY_PACKED_TO_UNPACKED{$prefix}) {
            my $number = int(substr($packed, 3, 4));
            return "$number $SURVEY_PACKED_TO_UNPACKED{$prefix}";
        }
    }

    die "Invalid packed provisional designation length: $packed\n" unless length($packed) == 7;

    my $century = substr($packed, 0, 1);
    my $year = substr($packed, 1, 2);
    my $half_month = substr($packed, 3, 1);
    my $order_encoded = substr($packed, 4, 2);
    my $second_letter = substr($packed, 6, 1);

    die "Invalid century code: $century\n" unless exists $CENTURY_CODES{$century};

    my $full_year = $CENTURY_CODES{$century} . $year;
    my $order_num = decode_cycle_count($order_encoded);

    if ($order_num == 0) {
        return "$full_year $half_month$second_letter";
    } else {
        return "$full_year $half_month$second_letter$order_num";
    }
}

sub pack_provisional {
    my ($unpacked) = @_;
    $unpacked =~ s/^\s+|\s+$//g;

    # Survey designations
    if ($unpacked =~ /^(\d+) (P-L|T-[123])$/) {
        my $number = $1;
        my $survey = $2;
        die "Survey number must be positive: $number\n" if $number < 1;
        return sprintf("%s%04d", $SURVEY_UNPACKED_TO_PACKED{$survey}, $number);
    }

    # Old-style designation: "A908 CJ" or "B842 FA"
    if ($unpacked =~ /^([AB])(\d)(\d{2}) ([A-Z])([A-Z])$/) {
        my ($prefix, $century_digit, $year_short, $half_month, $second_letter) = ($1, $2, $3, $4, $5);

        my $century_code;
        if ($century_digit eq '8') {
            $century_code = 'I';
        } elsif ($century_digit eq '9') {
            $century_code = 'J';
        } elsif ($century_digit eq '0') {
            $century_code = 'K';
        } else {
            die "Invalid century digit in old-style designation: $century_digit\n";
        }

        return "${century_code}${year_short}${half_month}00${second_letter}";
    }

    # Standard provisional: "1995 XA" or "1995 XA12"
    if ($unpacked =~ /^(\d{4}) ([A-Z])([A-Z])(\d*)$/) {
        my ($year, $half_month, $second_letter, $order_str) = ($1, $2, $3, $4);

        die "Invalid half-month letter: $half_month\n" unless is_valid_half_month($half_month);

        my $century = int(substr($year, 0, 2));
        my $year_short = substr($year, 2, 2);

        die "Invalid century in year: $year\n" unless exists $REVERSE_CENTURY_CODES{$century};

        my $century_code = $REVERSE_CENTURY_CODES{$century};
        my $order_num = length($order_str) ? int($order_str) : 0;

        # Extended format for cycle >= 620
        if ($order_num >= 620) {
            my $year_char = num_to_base62($year % 100);
            my $base_sequence = ($order_num - 620) * 25 + letter_to_position($second_letter) - 1;
            my $seq_encoded = num_to_base62_string($base_sequence, 4);
            return "_${year_char}${half_month}${seq_encoded}";
        }

        my $order_encoded = encode_cycle_count($order_num);
        return "${century_code}${year_short}${half_month}${order_encoded}${second_letter}";
    }

    die "Invalid unpacked provisional designation: $unpacked\n";
}

# =============================================================================
# Comet designations
# =============================================================================

sub unpack_comet_provisional {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;

    my $length = length($packed);
    die "Invalid packed comet provisional designation length: $packed\n" unless $length == 7 || $length == 8;

    my $century = substr($packed, 0, 1);
    my $year = substr($packed, 1, 2);
    my $half_month = substr($packed, 3, 1);
    my $order_encoded = substr($packed, 4, 2);
    my $fragment = $length == 7 ? substr($packed, 6, 1) : substr($packed, 6, 2);

    die "Invalid century code: $century\n" unless exists $CENTURY_CODES{$century};

    my $full_year = $CENTURY_CODES{$century} . $year;
    my $order_num = decode_cycle_count($order_encoded);

    my $result = "$full_year $half_month$order_num";
    $result .= "-" . uc($fragment) if $fragment ne '0';

    return $result;
}

sub pack_comet_provisional {
    my ($unpacked) = @_;
    $unpacked =~ s/^\s+|\s+$//g;

    if ($unpacked =~ /^(\d{4}) ([A-Z])(\d+)(?:-([A-Z]{1,2}))?$/) {
        my ($year, $half_month, $order_num, $fragment) = ($1, $2, $3, $4);

        die "Comet order number must be positive: $order_num\n" if $order_num < 1;

        my $century = int(substr($year, 0, 2));
        my $year_short = substr($year, 2, 2);

        die "Invalid century in year: $year\n" unless exists $REVERSE_CENTURY_CODES{$century};

        my $century_code = $REVERSE_CENTURY_CODES{$century};
        my $order_encoded = encode_cycle_count($order_num);
        my $fragment_code = defined($fragment) ? lc($fragment) : '0';

        return "${century_code}${year_short}${half_month}${order_encoded}${fragment_code}";
    }

    die "Invalid unpacked comet provisional designation: $unpacked\n";
}

sub unpack_comet_numbered {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;

    if ($packed =~ /^(\d{4})([PD])$/) {
        my $number = int($1);
        my $comet_type = $2;
        return "$number$comet_type";
    }

    die "Invalid packed numbered comet designation: $packed\n";
}

sub pack_comet_numbered {
    my ($unpacked) = @_;
    $unpacked =~ s/^\s+|\s+$//g;

    if ($unpacked =~ /^(\d+)([PD])(?:\/[A-Za-z].*)?$/) {
        my $number = int($1);
        my $comet_type = $2;

        die "Comet number out of range (1-9999): $number\n" if $number < 1 || $number > 9999;

        return sprintf("%04d%s", $number, $comet_type);
    }

    die "Invalid unpacked numbered comet designation: $unpacked\n";
}

# =============================================================================
# Satellite designations
# =============================================================================

sub unpack_satellite {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;

    die "Invalid packed satellite designation: $packed\n"
        unless length($packed) == 8 && substr($packed, 0, 1) eq 'S';

    my $century = substr($packed, 1, 1);
    my $year = substr($packed, 2, 2);
    my $planet = substr($packed, 4, 1);
    my $number_encoded = substr($packed, 5, 2);

    die "Invalid century code: $century\n" unless exists $CENTURY_CODES{$century};
    die "Invalid planet code: $planet\n" unless exists $SATELLITE_PLANETS{$planet};

    my $full_year = $CENTURY_CODES{$century} . $year;
    my $number = decode_cycle_count($number_encoded);

    return "S/$full_year $planet $number";
}

sub pack_satellite {
    my ($unpacked) = @_;
    $unpacked =~ s/^\s+|\s+$//g;

    if ($unpacked =~ /^S\/(\d{4}) ([JSUN]) (\d+)$/) {
        my ($year, $planet, $number) = ($1, $2, $3);

        die "Satellite number must be positive: $number\n" if $number < 1;

        my $century = int(substr($year, 0, 2));
        my $year_short = substr($year, 2, 2);

        die "Invalid century in year: $year\n" unless exists $REVERSE_CENTURY_CODES{$century};

        my $century_code = $REVERSE_CENTURY_CODES{$century};
        my $number_encoded = encode_cycle_count($number);

        return "S${century_code}${year_short}${planet}${number_encoded}0";
    }

    die "Invalid unpacked satellite designation: $unpacked\n";
}

# =============================================================================
# Ancient/BCE comet designations
# =============================================================================

sub encode_bce_year {
    my ($year) = @_;
    die "Not a BCE year: $year\n" if $year >= 0;

    my $abs_year = abs($year);
    my $code = 99 - ($abs_year % 100);

    if ($abs_year < 100) {
        return ('/', sprintf("%02d", $code));
    } elsif ($abs_year < 200) {
        return ('.', sprintf("%02d", $code));
    } elsif ($abs_year < 300) {
        return ('-', sprintf("%02d", $code));
    } else {
        die "BCE year out of supported range: $year\n";
    }
}

sub decode_bce_year {
    my ($prefix, $code) = @_;
    my $year_part = 99 - int($code);

    return -$year_part if $prefix eq '/';
    return -($year_part + 100) if $prefix eq '.';
    return -($year_part + 200) if $prefix eq '-';

    die "Invalid BCE prefix: $prefix\n";
}

sub pack_ancient_comet_provisional {
    my ($comet_type, $year, $half_month, $order_num, $fragment) = @_;
    $fragment //= '';

    my $order_encoded = encode_cycle_count($order_num);
    my $fragment_code = $fragment eq '' ? '0' : lc($fragment);

    if ($year < 0) {
        my ($prefix, $code) = encode_bce_year($year);
        return "${comet_type}${prefix}${code}${half_month}${order_encoded}${fragment_code}";
    } else {
        return sprintf("%s%03d%s%s%s", $comet_type, $year, $half_month, $order_encoded, $fragment_code);
    }
}

sub unpack_ancient_comet_provisional {
    my ($packed) = @_;
    $packed =~ s/^\s+|\s+$//g;

    die "Invalid ancient comet designation length: $packed\n" unless length($packed) == 8;

    my $comet_type = substr($packed, 0, 1);
    die "Invalid comet type: $comet_type\n" unless exists $COMET_TYPES{$comet_type};

    my ($year, $half_month, $order_encoded, $fragment);

    if (substr($packed, 1, 1) =~ /[\/.\-]/) {
        my $prefix = substr($packed, 1, 1);
        my $year_code = substr($packed, 2, 2);
        $year = decode_bce_year($prefix, $year_code);
        $half_month = substr($packed, 4, 1);
        $order_encoded = substr($packed, 5, 2);
        $fragment = substr($packed, 7, 1);
    } else {
        $year = int(substr($packed, 1, 3));
        $half_month = substr($packed, 4, 1);
        $order_encoded = substr($packed, 5, 2);
        $fragment = substr($packed, 7, 1);
    }

    my $order_num = decode_cycle_count($order_encoded);
    my $result = "$comet_type/$year $half_month$order_num";
    $result .= "-" . uc($fragment) if $fragment ne '0';

    return $result;
}

# =============================================================================
# Full comet designations
# =============================================================================

sub is_asteroid_style_packed {
    my ($prov) = @_;
    return 0 unless length($prov) == 7;
    return substr($prov, 6, 1) =~ /[A-Z]/;
}

sub is_asteroid_style_unpacked {
    my ($prov) = @_;
    return $prov =~ /^\d{4} [A-Z][A-Z]/;
}

sub unpack_comet_full {
    my ($packed) = @_;
    my $length = length($packed);

    my ($comet_type, $provisional_part, $num_str);

    if ($length == 8) {
        $comet_type = substr($packed, 0, 1);
        $provisional_part = substr($packed, 1, 7);
        $num_str = '';
    } elsif ($length == 9) {
        $comet_type = substr($packed, 0, 1);
        $provisional_part = substr($packed, 1, 8);
        $num_str = '';
    } elsif ($length == 12 || ($length < 12 && substr($packed, 0, 1) eq ' ')) {
        $packed = ' ' x (12 - $length) . $packed while length($packed) < 12;
        $num_str = substr($packed, 0, 4);
        $num_str =~ s/^\s+//;
        $comet_type = substr($packed, 4, 1);
        $provisional_part = substr($packed, 5, 7);
    } else {
        die "Invalid packed full comet designation length: $packed\n";
    }

    die "Invalid comet type: $comet_type\n" unless exists $COMET_TYPES{$comet_type};

    my $provisional;
    if (is_asteroid_style_packed($provisional_part)) {
        $provisional = unpack_provisional($provisional_part);
    } else {
        $provisional = unpack_comet_provisional($provisional_part);
    }

    if ($num_str eq '') {
        return "$comet_type/$provisional";
    } else {
        my $number = int($num_str);
        return "$number$comet_type/$provisional";
    }
}

sub pack_comet_full {
    my ($unpacked) = @_;
    $unpacked =~ s/^\s+|\s+$//g;

    if ($unpacked =~ /^(\d*)([PCDXAI])\/(-?\d+) (.+)$/) {
        my ($number, $comet_type, $year, $prov_part) = ($1, $2, $3, $4);

        die "Invalid comet type: $comet_type\n" unless exists $COMET_TYPES{$comet_type};

        # Ancient or BCE year
        if ($year < 1000) {
            if ($prov_part =~ /^([A-Z])(\d+)(?:-([A-Z]))?$/) {
                my ($half_month, $order_num, $fragment) = ($1, $2, $3);
                return pack_ancient_comet_provisional($comet_type, $year, $half_month, $order_num, $fragment);
            } else {
                die "Invalid ancient comet provisional: $prov_part\n";
            }
        }

        my $provisional = "$year $prov_part";
        my $provisional_packed;

        if (is_asteroid_style_unpacked($provisional)) {
            $provisional_packed = pack_provisional($provisional);
        } else {
            $provisional_packed = pack_comet_provisional($provisional);
        }

        if ($number eq '') {
            return "$comet_type$provisional_packed";
        } else {
            my $num = int($number);
            die "Comet number out of range (1-9999): $num\n" if $num < 1 || $num > 9999;
            return sprintf("%04d%s%s", $num, $comet_type, $provisional_packed);
        }
    }

    die "Invalid unpacked comet designation: $unpacked\n";
}

# =============================================================================
# Format detection
# =============================================================================

sub detect_format {
    my ($designation) = @_;

    my %result = (format => '', type => '', subtype => '');

    validate_raw_input($designation);

    # Check for packed full comet (12 chars)
    if (length($designation) == 12) {
        if ($designation =~ /^([ 0-9]{4})([PCDXAI])([IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z])$/) {
            return { format => 'packed', type => 'comet_full', subtype => 'comet with provisional (12-char)' };
        }
    }

    # Check for packed comet (8 chars)
    if (length($designation) == 8) {
        if ($designation =~ /^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9A-Za-z])$/) {
            return { format => 'packed', type => 'comet_full', subtype => 'comet with provisional (8-char)' };
        }
        if ($designation =~ /^([PCDXAI])([0-9]{3})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/) {
            return { format => 'packed', type => 'comet_ancient', subtype => 'comet ancient provisional' };
        }
        if ($designation =~ /^([PCDXAI])([\/.\-])([0-9]{2})([A-Z][0-9A-Za-z]{2}[0-9a-z])$/) {
            return { format => 'packed', type => 'comet_bce', subtype => 'comet BCE provisional' };
        }
    }

    # Check for packed comet with 2-letter fragment (9 chars)
    if (length($designation) == 9) {
        if ($designation =~ /^([PCDXAI])([A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[a-z]{2})$/) {
            return { format => 'packed', type => 'comet_full', subtype => 'comet with provisional (9-char)' };
        }
    }

    my $des = $designation;
    $des =~ s/^\s+|\s+$//g;

    validate_whitespace($des);

    # Packed satellite (8 chars starting with S)
    if (length($des) == 8 && substr($des, 0, 1) eq 'S') {
        if ($des =~ /^S[A-L][0-9]{2}[JSUN][0-9A-Za-z]{2}0$/) {
            my $planet = substr($des, 4, 1);
            my $name = $SATELLITE_PLANET_NAMES{$planet} // $planet;
            return { format => 'packed', type => 'satellite', subtype => "natural satellite ($name)" };
        }
    }

    # Packed permanent (5 chars)
    if (length($des) == 5) {
        if (substr($des, 0, 1) eq '~') {
            if ($des =~ /^~[0-9A-Za-z]{4}$/) {
                return { format => 'packed', type => 'permanent', subtype => 'permanent numbered (tilde/base-62)' };
            }
        } elsif ($des =~ /^\d{5}$/) {
            return { format => 'packed', type => 'permanent', subtype => 'permanent numbered (5-digit)' };
        } elsif ($des =~ /^[A-Za-z][0-9]{4}$/) {
            return { format => 'packed', type => 'permanent', subtype => 'permanent numbered (letter-prefix)' };
        }
        if ($des =~ /^[0-9]{4}[PD]$/) {
            return { format => 'packed', type => 'comet_numbered', subtype => 'comet numbered' };
        }
    }

    # Packed provisional (7 chars)
    if (length($des) == 7) {
        if (substr($des, 0, 1) eq '_') {
            # Year code: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
            if ($des =~ /^_[0-9A-Za-z][A-Z][0-9A-Za-z]{4}$/) {
                return { format => 'packed', type => 'provisional_extended', subtype => 'provisional extended' };
            }
        }
        if ($des =~ /^[A-L][0-9]{2}[A-Z][0-9A-Za-z]{2}[A-Z]$/) {
            return { format => 'packed', type => 'provisional', subtype => 'provisional' };
        }
        if ($des =~ /^PLS\d{4}$/) {
            return { format => 'packed', type => 'survey', subtype => 'survey (Palomar-Leiden)' };
        }
        if ($des =~ /^T[123]S\d{4}$/) {
            return { format => 'packed', type => 'survey', subtype => 'survey (Trojan)' };
        }
        if ($des =~ /^[IJKL][0-9]{2}[A-Z][0-9A-Za-z]{2}[0-9a-z]$/) {
            return { format => 'packed', type => 'comet_provisional', subtype => 'comet provisional' };
        }
    }

    # --- UNPACKED FORMATS ---

    # Unpacked satellite
    if ($des =~ /^S\/\d{4} ([JSUN]) \d+$/) {
        my $planet = $1;
        my $name = $SATELLITE_PLANET_NAMES{$planet} // $planet;
        return { format => 'unpacked', type => 'satellite', subtype => "natural satellite ($name)" };
    }

    # Unpacked permanent (all digits)
    if ($des =~ /^\d+$/) {
        return { format => 'unpacked', type => 'permanent', subtype => 'permanent numbered' };
    }

    # Unpacked survey
    if ($des =~ /^\d+ (P-L|T-[123])$/) {
        return { format => 'unpacked', type => 'survey', subtype => 'survey' };
    }

    # Old-style provisional
    if ($des =~ /^[AB]\d{3} [A-Z][A-Z]$/) {
        return { format => 'unpacked', type => 'provisional', subtype => 'provisional (old-style)' };
    }

    # Unpacked provisional
    if ($des =~ /^\d{4} [A-Z][A-Z]\d*$/) {
        return { format => 'unpacked', type => 'provisional', subtype => 'provisional' };
    }

    # Unpacked comet with type prefix
    if ($des =~ /^(\d*)([PCDXAI])\/(-?\d+) ([A-Z][A-Z0-9]+)(?:-([A-Z]{1,2}))?$/) {
        return { format => 'unpacked', type => 'comet_full', subtype => 'comet provisional' };
    }

    # Unpacked numbered comet
    if ($des =~ /^(\d+)([PD])(?:\/[A-Za-z].*)?$/) {
        return { format => 'unpacked', type => 'comet_numbered', subtype => 'comet numbered' };
    }

    die "Unable to detect designation format: $designation\n";
}

# =============================================================================
# Main conversion functions
# =============================================================================

sub convert {
    my ($designation) = @_;

    my $info = detect_format($designation);
    my $result = '';

    if ($info->{format} eq 'packed') {
        if ($info->{type} eq 'permanent') {
            $result = unpack_permanent($designation);
        } elsif ($info->{type} =~ /^(provisional|survey)$/) {
            $result = unpack_provisional($designation);
        } elsif ($info->{type} eq 'provisional_extended') {
            # Extended provisional unpacking
            my $des = $designation;
            $des =~ s/^\s+|\s+$//g;
            my $year_char = substr($des, 1, 1);
            my $half_month = substr($des, 2, 1);
            my $seq_encoded = substr($des, 3, 4);
            my $base_sequence = base62_string_to_num($seq_encoded);
            my $cycle = 620 + int($base_sequence / 25);
            my $letter_pos = ($base_sequence % 25) + 1;
            my $second_letter = position_to_letter($letter_pos);
            # Year code is base-62: digit (0-9 for 2000-2009) or letter (A=10 for 2010, etc.)
            # Extended format is for years 2000-2099, year code is year % 100
            my $year = 2000 + base62_to_num($year_char);
            $result = "$year $half_month$second_letter$cycle";
        } elsif ($info->{type} eq 'comet_numbered') {
            $result = unpack_comet_numbered($designation);
        } elsif ($info->{type} eq 'comet_provisional') {
            $result = unpack_comet_provisional($designation);
        } elsif ($info->{type} eq 'comet_full') {
            $result = unpack_comet_full($designation);
        } elsif ($info->{type} =~ /^comet_(ancient|bce)$/) {
            $result = unpack_ancient_comet_provisional($designation);
        } elsif ($info->{type} eq 'satellite') {
            $result = unpack_satellite($designation);
        }
    } else {
        if ($info->{type} eq 'permanent') {
            my $num = $designation;
            $num =~ s/^\s+|\s+$//g;
            $result = pack_permanent(int($num));
        } elsif ($info->{type} =~ /^(provisional|survey)$/) {
            $result = pack_provisional($designation);
        } elsif ($info->{type} eq 'comet_numbered') {
            $result = pack_comet_numbered($designation);
        } elsif ($info->{type} eq 'comet_full') {
            $result = pack_comet_full($designation);
        } elsif ($info->{type} eq 'satellite') {
            $result = pack_satellite($designation);
        }
    }

    return {
        input => $designation,
        output => $result,
        info => $info
    };
}

sub convert_simple {
    my ($designation) = @_;
    return convert($designation)->{output};
}

sub pack {
    my ($designation) = @_;
    my $info = detect_format($designation);
    return $designation =~ s/^\s+|\s+$//gr if $info->{format} eq 'packed';
    return convert($designation)->{output};
}

sub unpack {
    my ($designation) = @_;
    my $info = detect_format($designation);
    return $designation =~ s/^\s+|\s+$//gr if $info->{format} eq 'unpacked';
    return convert($designation)->{output};
}

1;

__END__

=head1 NAME

MPC::Designation - Convert between packed and unpacked MPC designations

=head1 SYNOPSIS

    use MPC::Designation qw(pack unpack convert_simple);

    my $packed = pack("1995 XA");      # "J95X00A"
    my $unpacked = unpack("J95X00A");  # "1995 XA"
    my $converted = convert_simple("1"); # "00001"

=head1 DESCRIPTION

This module provides functions for converting between packed and unpacked
Minor Planet Center (MPC) designations for asteroids, comets, and satellites.

=head1 AUTHOR

MPC Designations Project

=head1 LICENSE

CC0 1.0 Universal

=cut
