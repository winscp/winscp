#!/usr/bin/env perl
#
# Makefile generator for PuTTY.
#
# Reads the file `Recipe' to determine the list of generated
# executables and their component objects. Then reads the source
# files to compute #include dependencies. Finally, writes out the
# various target Makefiles.

open IN, "Recipe" or die "unable to open Recipe file\n";

$help = ""; # list of newline-free lines of help text
%programs = (); # maps program name to listref of objects/resources
%types = (); # maps program name to "G" or "C"
%groups = (); # maps group name to listref of objects/resources

while (<IN>) {
  # Skip comments (unless the comments belong, for example because
  # they're part of the help text).
  next if /^\s*#/ and !$in_help;

  chomp;
  split;
  if ($_[0] eq "!begin" and $_[1] eq "help") { $in_help = 1; next; }
  if ($_[0] eq "!end" and $in_help) { $in_help = 0; next; }
  # If we're gathering help text, keep doing so.
  if ($in_help) { $help .= "$_\n"; next; }
  # Ignore blank lines.
  next if scalar @_ == 0;

  # Now we have an ordinary line. See if it's an = line, a : line
  # or a + line.
  @objs = @_;

  if ($_[0] eq "+") {
    $listref = $lastlistref;
    $prog = undef;
    die "$.: unexpected + line\n" if !defined $lastlistref;
  } elsif ($_[1] eq "=") {
    $groups{$_[0]} = [] if !defined $groups{$_[0]};
    $listref = $groups{$_[0]};
    $prog = undef;
    shift @objs; # eat the group name
  } elsif ($_[1] eq ":") {
    $programs{$_[0]} = [] if !defined $programs{$_[0]};
    $listref = $programs{$_[0]};
    $prog = $_[0];
    shift @objs; # eat the program name
  } else {
    die "$.: unrecognised line type\n";
  }
  shift @objs; # eat the +, the = or the :

  while (scalar @objs > 0) {
    $i = shift @objs;
    if ($groups{$i}) {
      foreach $j (@{$groups{$i}}) { unshift @objs, $j; }
    } elsif (($i eq "[G]" or $i eq "[C]") and defined $prog) {
      $types{$prog} = substr($i,1,1);
    } else {
      push @$listref, $i;
    }
  }
  $lastlistref = $listref;
}

close IN;

# Now retrieve the complete list of objects and resource files, and
# construct dependency data for them. While we're here, expand the
# object list for each program, and complain if its type isn't set.
@prognames = sort keys %programs;
%depends = ();
@scanlist = ();
foreach $i (@prognames) {
  if (!defined $types{$i}) { die "type not set for program $i\n"; }
  # Strip duplicate object names.
  $prev = undef;
  @list = grep { $status = ($prev ne $_); $prev=$_; $status }
          sort @{$programs{$i}};
  $programs{$i} = [@list];
  foreach $j (@list) {
    # Dependencies for "x" start with "x.c".
    # Dependencies for "x.res" start with "x.rc".
    # Both types of file are pushed on the list of files to scan.
    # Libraries (.lib) don't have dependencies at all.
    if ($j =~ /^(.*)\.res$/) {
      $file = "$1.rc";
      $depends{$j} = [$file];
      push @scanlist, $file;
    } elsif ($j =~ /\.lib$/) {
      # libraries don't have dependencies
    } else {
      $file = "$j.c";
      $depends{$j} = [$file];
      push @scanlist, $file;
    }
  }
}

# Scan each file on @scanlist and find further inclusions.
# Inclusions are given by lines of the form `#include "otherfile"'
# (system headers are automatically ignored by this because they'll
# be given in angle brackets). Files included by this method are
# added back on to @scanlist to be scanned in turn (if not already
# done).
#
# Resource scripts (.rc) can also include a file by means of a line
# ending `ICON "filename"'. Files included by this method are not
# added to @scanlist because they can never include further files.
#
# In this pass we write out a hash %further which maps a source
# file name into a listref containing further source file names.

%further = ();
while (scalar @scanlist > 0) {
  $file = shift @scanlist;
  next if defined $further{$file}; # skip if we've already done it
  $resource = ($file =~ /\.rc$/ ? 1 : 0);
  $further{$file} = [];
  open IN, $file or die "unable to open source file $file\n";
  while (<IN>) {
    chomp;
    /^\s*#include\s+\"([^\"]+)\"/ and do {
      push @{$further{$file}}, $1;
      push @scanlist, $1;
      next;
    };
    /ICON\s+\"([^\"]+)\"\s*$/ and do {
      push @{$further{$file}}, $1;
      next;
    }
  }
  close IN;
}

# Now we're ready to generate the final dependencies section. For
# each key in %depends, we must expand the dependencies list by
# iteratively adding entries from %further.
foreach $i (keys %depends) {
  %dep = ();
  @scanlist = @{$depends{$i}};
  foreach $i (@scanlist) { $dep{$i} = 1; }
  while (scalar @scanlist > 0) {
    $file = shift @scanlist;
    foreach $j (@{$further{$file}}) {
      if ($dep{$j} != 1) {
        $dep{$j} = 1;
	push @{$depends{$i}}, $j;
	push @scanlist, $j;
      }
    }
  }
#  printf "%s: %s\n", $i, join ' ',@{$depends{$i}};
}

# Utility routines while writing out the Makefiles.

sub objects {
  my ($prog, $otmpl, $rtmpl, $ltmpl) = @_;
  my @ret;
  my ($i, $x, $y);
  @ret = ();
  foreach $i (@{$programs{$prog}}) {
    if ($i =~ /^(.*)\.res/) {
      $y = $1;
      ($x = $rtmpl) =~ s/X/$y/;
      push @ret, $x if $x ne "";
    } elsif ($i =~ /^(.*)\.lib/) {
      $y = $1;
      ($x = $ltmpl) =~ s/X/$y/;
      push @ret, $x if $x ne "";
    } else {
      ($x = $otmpl) =~ s/X/$i/;
      push @ret, $x if $x ne "";
    }
  }
  return join " ", @ret;
}

sub splitline {
  my ($line, $width) = @_;
  my ($result, $len);
  $len = (defined $width ? $width : 76);
  while (length $line > $len) {
    $line =~ /^(.{0,$len})\s(.*)$/ or $line =~ /^(.{$len,}?\s(.*)$/;
    $result .= $1 . " \\\n\t\t";
    $line = $2;
    $len = 60;
  }
  return $result . $line;
}

sub deps {
  my ($otmpl, $rtmpl) = @_;
  my ($i, $x, $y);
  foreach $i (sort keys %depends) {
    if ($i =~ /^(.*)\.res/) {
      $y = $1;
      ($x = $rtmpl) =~ s/X/$y/;
    } else {
      ($x = $otmpl) =~ s/X/$i/;
    }
    print &splitline(sprintf "%s: %s", $x, join " ", @{$depends{$i}}), "\n";
  }
}

# Now we're ready to output the actual Makefiles.

##-- CygWin makefile
open OUT, ">Makefile.cyg"; select OUT;
print
"# Makefile for PuTTY under cygwin.\n".
"#\n# This file was created by `mkfiles.pl' from the `Recipe' file.\n".
"# DO NOT EDIT THIS FILE DIRECTLY; edit Recipe or mkfiles.pl instead.\n";
# gcc command line option is -D not /D
($_ = $help) =~ s/=\/D/=-D/gs;
print $_;
print
"\n".
"# You can define this path to point at your tools if you need to\n".
"# TOOLPATH = c:\\cygwin\\bin\\ # or similar, if you're running Windows\n".
"# TOOLPATH = /pkg/mingw32msvc/i386-mingw32msvc/bin/\n".
"CC = \$(TOOLPATH)gcc\n".
"RC = \$(TOOLPATH)windres\n".
"# You may also need to tell windres where to find include files:\n".
"# RCINC = --include-dir c:\\cygwin\\include\\\n".
"\n".
&splitline("CFLAGS = -mno-cygwin -Wall -O2 -D_WINDOWS -DDEBUG -DWIN32S_COMPAT".
  " -DNO_SECURITY -D_NO_OLDNAMES -DNO_MULTIMON -I.")."\n".
"LDFLAGS = -mno-cygwin -s\n".
&splitline("RCFLAGS = \$(RCINC) --define WIN32=1 --define _WIN32=1".
  " --define WINVER=0x0400 --define MINGW32_FIX=1")."\n".
"\n".
".SUFFIXES:\n".
"\n".
"%.o: %.c\n".
"\t\$(CC) \$(COMPAT) \$(FWHACK) \$(XFLAGS) \$(CFLAGS) -c \$<\n".
"\n".
"%.res.o: %.rc\n".
"\t\$(RC) \$(FWHACK) \$(RCFL) \$(RCFLAGS) \$< \$\@\n".
"\n";
print &splitline("all:" . join "", map { " $_.exe" } @prognames);
print "\n\n";
foreach $p (@prognames) {
  $objstr = &objects($p, "X.o", "X.res.o", undef);
  print &splitline($p . ".exe: " . $objstr), "\n";
  my $mw = $types{$p} eq "G" ? " -mwindows" : "";
  $libstr = &objects($p, undef, undef, "-lX");
  print &splitline("\t\$(CC)" . $mw . " \$(LDFLAGS) -o \$@ " .
                   $objstr . " $libstr", 69), "\n\n";
}
&deps("X.o", "X.res.o");
print
"\n".
"version.o: FORCE;\n".
"# Hack to force version.o to be rebuilt always\n".
"FORCE:\n".
"\t\$(CC) \$(COMPAT) \$(FWHACK) \$(XFLAGS) \$(CFLAGS) \$(VER) -c version.c\n".
"clean:\n".
"\trm -f *.o *.exe *.res.o\n".
"\n";
select STDOUT; close OUT;

##-- Borland makefile
%stdlibs = (  # Borland provides many Win32 API libraries intrinsically
  "advapi32" => 1,
  "comctl32" => 1,
  "comdlg32" => 1,
  "gdi32" => 1,
  "imm32" => 1,
  "shell32" => 1,
  "user32" => 1,
  "winmm" => 1,
  "winspool" => 1,
  "wsock32" => 1,
);	    
open OUT, ">Makefile.bor"; select OUT;
print
"# Makefile for PuTTY under Borland C.\n".
"#\n# This file was created by `mkfiles.pl' from the `Recipe' file.\n".
"# DO NOT EDIT THIS FILE DIRECTLY; edit Recipe or mkfiles.pl instead.\n";
# bcc32 command line option is -D not /D
($_ = $help) =~ s/=\/D/=-D/gs;
print $_;
print
"\n".
"# If you rename this file to `Makefile', you should change this line,\n".
"# so that the .rsp files still depend on the correct makefile.\n".
"MAKEFILE = Makefile.bor\n".
"\n".
"# C compilation flags\n".
"CFLAGS = -DWINVER=0x0401\n".
"\n".
"# Get include directory for resource compiler\n".
"!if !\$d(BCB)\n".
"BCB = \$(MAKEDIR)\\..\n".
"!endif\n".
"\n".
".c.obj:\n".
&splitline("\tbcc32 -w-aus -w-ccc -w-par -w-pia \$(COMPAT) \$(FWHACK)".
  " \$(XFLAGS) \$(CFLAGS) /c \$*.c",69)."\n".
".rc.res:\n".
&splitline("\tbrcc32 \$(FWHACK) \$(RCFL) -i \$(BCB)\\include -r".
  " -DNO_WINRESRC_H -DWIN32 -D_WIN32 -DWINVER=0x0401 \$*.rc",69)."\n".
"\n";
print &splitline("all:" . join "", map { " $_.exe" } @prognames);
print "\n\n";
foreach $p (@prognames) {
  $objstr = &objects($p, "X.obj", "X.res", undef);
  print &splitline("$p.exe: " . $objstr . " $p.rsp"), "\n";
  my $ap = ($types{$p} eq "G") ? "-aa" : "-ap";
  print "\tilink32 $ap -Gn -L\$(BCB)\\lib \@$p.rsp\n\n";
}
foreach $p (@prognames) {
  print $p, ".rsp: \$(MAKEFILE)\n";
  $objstr = &objects($p, "X.obj", undef, undef);
  @objlist = split " ", $objstr;
  @objlines = ("");
  foreach $i (@objlist) {
    if (length($objlines[$#objlines] . " $i") > 50) {
      push @objlines, "";
    }
    $objlines[$#objlines] .= " $i";
  }
  $c0w = ($types{$p} eq "G") ? "c0w32" : "c0x32";
  print "\techo $c0w + > $p.rsp\n";
  for ($i=0; $i<=$#objlines; $i++) {
    $plus = ($i < $#objlines ? " +" : "");
    print "\techo$objlines[$i]$plus >> $p.rsp\n";
  }
  print "\techo $p.exe >> $p.rsp\n";
  $objstr = &objects($p, "X.obj", "X.res", undef);
  @libs = split " ", &objects($p, undef, undef, "X");
  @libs = grep { !$stdlibs{$_} } @libs;
  unshift @libs, "cw32", "import32";
  $libstr = join ' ', @libs;
  print "\techo nul,$libstr, >> $p.rsp\n";
  print "\techo " . &objects($p, undef, "X.res", undef) . " >> $p.rsp\n";
  print "\n";
}
&deps("X.obj", "X.res");
print
"\n".
"version.o: FORCE\n".
"# Hack to force version.o to be rebuilt always\n".
"FORCE:\n".
"\tbcc32 \$(FWHACK) \$(VER) \$(CFLAGS) /c version.c\n\n".
"clean:\n".
"\t-del *.obj\n".
"\t-del *.exe\n".
"\t-del *.res\n".
"\t-del *.pch\n".
"\t-del *.aps\n".
"\t-del *.il*\n".
"\t-del *.pdb\n".
"\t-del *.rsp\n".
"\t-del *.tds\n".
"\t-del *.\$\$\$\$\$\$\n";
select STDOUT; close OUT;

##-- Visual C++ makefile
open OUT, ">Makefile.vc"; select OUT;
print
"# Makefile for PuTTY under Visual C.\n".
"#\n# This file was created by `mkfiles.pl' from the `Recipe' file.\n".
"# DO NOT EDIT THIS FILE DIRECTLY; edit Recipe or mkfiles.pl instead.\n";
print $help;
print
"\n".
"# If you rename this file to `Makefile', you should change this line,\n".
"# so that the .rsp files still depend on the correct makefile.\n".
"MAKEFILE = Makefile.vc\n".
"\n".
"# C compilation flags\n".
"CFLAGS = /nologo /W3 /O1 /D_WINDOWS /D_WIN32_WINDOWS=0x401 /DWINVER=0x401\n".
"LFLAGS = /incremental:no /fixed\n".
"\n".
".c.obj:\n".
"\tcl \$(COMPAT) \$(FWHACK) \$(XFLAGS) \$(CFLAGS) /c \$*.c\n".
".rc.res:\n".
"\trc \$(FWHACK) \$(RCFL) -r -DWIN32 -D_WIN32 -DWINVER=0x0400 \$*.rc\n".
"\n";
print &splitline("all:" . join "", map { " $_.exe" } @prognames);
print "\n\n";
foreach $p (@prognames) {
  $objstr = &objects($p, "X.obj", "X.res", undef);
  print &splitline("$p.exe: " . $objstr . " $p.rsp"), "\n";
  print "\tlink \$(LFLAGS) -out:$p.exe -map:$p.map \@$p.rsp\n\n";
}
foreach $p (@prognames) {
  print $p, ".rsp: \$(MAKEFILE)\n";
  $objstr = &objects($p, "X.obj", "X.res", "X.lib");
  @objlist = split " ", $objstr;
  @objlines = ("");
  foreach $i (@objlist) {
    if (length($objlines[$#objlines] . " $i") > 50) {
      push @objlines, "";
    }
    $objlines[$#objlines] .= " $i";
  }
  $subsys = ($types{$p} eq "G") ? "windows" : "console";
  print "\techo /nologo /subsystem:$subsys > $p.rsp\n";
  for ($i=0; $i<=$#objlines; $i++) {
    print "\techo$objlines[$i] >> $p.rsp\n";
  }
  print "\n";
}
&deps("X.obj", "X.res");
print
"\n".
"# Hack to force version.o to be rebuilt always\n".
"version.obj: *.c *.h *.rc\n".
"\tcl \$(FWHACK) \$(VER) \$(CFLAGS) /c version.c\n\n".
"clean: tidy\n".
"\t-del *.exe\n\n".
"tidy:\n".
"\t-del *.obj\n".
"\t-del *.res\n".
"\t-del *.pch\n".
"\t-del *.aps\n".
"\t-del *.ilk\n".
"\t-del *.pdb\n".
"\t-del *.rsp\n".
"\t-del *.dsp\n".
"\t-del *.dsw\n".
"\t-del *.ncb\n".
"\t-del *.opt\n".
"\t-del *.plg\n".
"\t-del *.map\n".
"\t-del *.idb\n".
"\t-del debug.log\n";
select STDOUT; close OUT;
