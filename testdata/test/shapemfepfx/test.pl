#!/usr/bin/env perl

use strict;
use warnings;

my ($bin_window, $bin_nowindow, $inputSequence) = @ARGV;
die "usage: perl $0 <binary of window version> <binary of non-window version> <input RNA sequence>\n" if (@ARGV != 3);

#run and parse results of window version
	my %window_shapes = ();
	foreach my $line (split(m/\r?\n/, qx($bin_window -w 2 -i 1 $inputSequence))) {
		my ($i_all, $j_all, $i_window, $j_window, $shape, $prob, $mfe, $db) = split(m/;/, $line);
		if ((defined $j_window) && ($j_window == length($inputSequence))) {
			$window_shapes{$shape} = {prob => $prob, mfe => $mfe, db => $db};
		}
	}

#run and parse results of window version
	my %nonwindow_shapes = ();
	foreach my $line (split(m/\r?\n/, qx($bin_nowindow $inputSequence))) {
		my ($shape, $mfe, $pfunc, $db) = ($line =~ m/^\( \( (\S+) , \( \((\S+), <\d+, \d+>, <\d+, \d+>\) , (\S+) \) \) , (\S+) \)$/);
		if (defined $shape) {
			$nonwindow_shapes{$shape} = {prob => undef, pfunc => $pfunc, mfe => $mfe, db => $db};
		}
	}
	my $pfuncSum = 0;
	foreach my $shape (keys(%nonwindow_shapes)) {
		$pfuncSum += $nonwindow_shapes{$shape}->{pfunc};
	}
	if ($pfuncSum > 0) {
		foreach my $shape (keys(%nonwindow_shapes)) {
			$nonwindow_shapes{$shape}->{prob} = $nonwindow_shapes{$shape}->{pfunc} / $pfuncSum;
		}
	}
	
#compare results
	foreach my $shape (keys(%nonwindow_shapes)) {
		if (not exists $window_shapes{$shape}) {
			print STDERR "Subseq 0 ".(length $inputSequence)." ".$inputSequence."\n";
			print STDERR "Could not find shape: ".$shape."\n";
		} else {
			if (abs($nonwindow_shapes{$shape}->{prob} - $window_shapes{$shape}->{prob}) >= 0.00001) {
				print STDERR "Subseq 0 ".(length $inputSequence)." ".$inputSequence."\n";
				print STDERR "Shape: ".$shape."\n";
				print STDERR "Probs: ".$nonwindow_shapes{$shape}->{prob}." ".$window_shapes{$shape}->{prob}."\n";
			}
			if (abs($nonwindow_shapes{$shape}->{mfe} - $window_shapes{$shape}->{mfe}) >= 0.00001) {
				print STDERR "Subseq 0 ".(length $inputSequence)." ".$inputSequence."\n";
				print STDERR "Shape: ".$shape."\n";
				print STDERR "Mfe: ".$nonwindow_shapes{$shape}->{mfe}." ".$window_shapes{$shape}->{mfe}."\n";
			}
		}
	}