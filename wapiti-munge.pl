#!/usr/bin/env perl

use strict;
use warnings;

my $file = $ARGV[0];
open my $model, '<', $file or die "Couldn't open $file for reading: $!\n";

my $active_features = 0;
my $patterns;
my $labels;
my $observations;

my $header = <$model>;
chomp $header;
if($header =~ m/\A \#mdl\#(\d+) \z/msx) {
    $active_features = int $1;
}
else { die "Invalid model header: $header\n" }

# Read patterns (if any)
$patterns = read_patterns($model);

# Read label quark DB
$labels = read_quarks($model);

# Read observation quark DB
$observations = read_quarks($model);

# Read feature weights
my $weights = {};
while(my $line = <$model>) {
    chomp $line;
    if($line =~ m/\A (\d+) = (-?) 0x(1\.[0-9a-f]+) p ([+-]\d+) \z/msx) {
        my $num = $1;
        my $sign = $2;
        my $mantissa = $3;
        my $exponent = int $4;

        #$weights->[$num] = [$mantissa, $exponent];
        $weights->{$num} = {sign     => $sign,
                            mantissa => $mantissa,
                            exponent => $exponent,
                            number   => $num};
    }
    else {
        die "Bad feature weight at line $.\n";
    }
}

#dump_wapiti($patterns, $labels, $observations, $weights, $active_features);
dump_sexpr($patterns, $labels, $observations, $weights, $active_features);

sub read_patterns {
    my ($model) = @_;

    my $patterns;
    my $header = <$model>;
    chomp $header;
    if($header =~ m{\A \#rdr\# (\d+)/(\d+) \z}msx) {
        # Field 2 appears non-sensical
        $patterns = int $1;
    }
    else { die "Invalid reader header: $header\n" }

    my $pats = [];
    for(my $i = 0; $i < $patterns; $i++) {
        push @$pats, read_string($model);
    }

    return $pats;
}

sub read_quarks {
    my ($model) = @_;

    my $strings;
    my $header = <$model>;
    chomp $header;
    if($header =~ m/\A \#qrk\# (\d+) \z/msx) {
        $strings = int $1;
    }

    my $db = [];
    for(my $i = 0; $i < $strings; $i++) {
        push @$db, read_string($model);
    }

    return $db;
}

sub read_string {
    my ($model) = @_;
    my $line = <$model>;
    chomp $line;

    # $. is line number of last filehandle accessed.
    if($line =~ m/\A (\d+) : (.+) , \z/msx) {
        my ($length, $string) = ($1, $2);
        die "Bad length string at line $.: `$string' not $length long\n"
            if $length != length $string;

        return $2;
    }
    else {
        die "Bad input on line $.: $line\n";
    }
}

sub dump_wapiti {
    my ($templates, $labels, $observations, $features, $active) = @_;

    print "#mdl#$active\n";
    print "#rdr#", scalar @$templates, "/?\n";
    for(my $i = 0; $i < scalar @$templates; $i++) {
        print length $templates->[$i], ":$templates->[$i],\n"
    }

    print "#qrk#", scalar @$labels, "\n";
    for(my $i = 0; $i < scalar @$labels; $i++) {
        print length $labels->[$i], ":$labels->[$i],\n"
    }

    print "#qrk#", scalar @$observations, "\n";
    for(my $i = 0; $i < scalar @$observations; $i++) {
        print length $observations->[$i], ":$observations->[$i],\n"
    }

    for my $weight (sort {int $a <=> int $b} keys %$features) {
        $weight = $features->{$weight};
        print "$weight->{number}=$weight->{sign}0x$weight->{mantissa}p",
            ($weight->{exponent} >= 0? '+' : ''), "$weight->{exponent}\n";
    }
}

sub dump_sexpr {
    my ($templates, $labels, $observations, $features, $active) = @_;

    print "(", join(" ", map {"\"$_\""} @$templates), ")\n";
    print "#1A(", join(" ", map {"\"$_\""} @$labels), ")\n";
    print "#1A(", join(" ", map {"\"$_\""} @$observations), ")\n";
    #print $active, "\n";
    print "#1A(";
    for my $weight (sort {int $a <=> int $b} keys %$features) {
        $weight = $features->{$weight};
        print "($weight->{number} \"$weight->{sign}\" \"$weight->{mantissa}\" \"$weight->{exponent}\") ";
    }
    print ")\n";
}
