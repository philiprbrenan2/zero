#!/usr/bin/perl -I/home/phil/perl/cpan/DataTableText/lib/ -I/home/phil/perl/cpan/GitHubCrud/lib/
#-------------------------------------------------------------------------------
# Push 0 code to GitHub
# Philip R Brenan at gmail dot com, Appa Apps Ltd Inc., 2023
#-------------------------------------------------------------------------------
use warnings FATAL => qw(all);
use strict;
use Carp;
use Data::Dump qw(dump);
use Data::Table::Text qw(:all);
use GitHub::Crud qw(:all);
use feature qw(say current_sub);

my $home = q(/home/phil/perl/zero/);                                            # Local files
my $user = q(philiprbrenan);                                                    # User
my $repo = q(zero);                                                             # Store code here so it can be referenced from a browser
my $wf   = q(.github/workflows/main.yml);                                       # Work flow on Ubuntu

expandWellKnownWordsInMarkDownFile                                              # Documentation
  fpe($home, qw(README md2)), fpe $home, qw(README md);

push my @files, searchDirectoryTreesForMatchingFiles($home, qw(.pl .md));        # Files

for my $s(@files)                                                               # Upload each selected file
 {my $c = readFile($s);                                                         # Load file
  my $t = swapFilePrefix $s, $home;
  my $w = writeFileUsingSavedToken($user, $repo, $t, $c);
  lll "$w $s $t";
 }

my $d = dateTimeStamp;

my $y = <<'END';
# Test $d

name: Test

on:
  push

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
    - uses: actions/checkout@v2
      with:
        ref: 'main'

    - name: Download other repos
      run: |
        curl -sL https://github.com/philiprbrenan/DataTableText/archive/refs/heads/main.zip > DataTableText.zip
        unzip DataTableText
        cp -R DataTableText-main/* .

    - name: Install Data::Dump
      run: |
        sudo cpan install -T Data::Dump

    - name: Zero
      run: |
        perl emulator.pl
END

lll "Ubuntu work flow for $repo ", writeFileUsingSavedToken($user, $repo, $wf, $y);
