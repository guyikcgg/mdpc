#!/usr/bin/perl

open(MYINPUTFILE, "<memoria.lyx"); # open for input
my(@lines) = <MYINPUTFILE>; # read file into list

my $allpaths = "";

foreach(@lines) {
	my $img = $_;
	if ($img =~ /img/) {
		if ($img =~ m/filename.(.*)/) {
			my $path = $1;
#			print "$path ";
			$allpaths = $allpaths . " " . $path;
		}
	}
}
print "$allpaths";

close(MYINPUTFILE);
