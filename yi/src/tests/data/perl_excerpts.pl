# Extracted from the perldata, perlop and perlsyn man pages plus some random bits.
# This variable identifier should not cause an error.
$};

$days;               # the simple scalar value "days"
$days[28];           # the 29th element of array @days
$days{'Feb'};        # the 'Feb' value from hash %days
$#days;              # the last index of array @days

@days;               # ($days[0], $days[1],... $days[n])
@days[3,4,5];        # same as ($days[3],$days[4],$days[5])
@days{'a','c'};      # same as ($days{'a'},$days{'c'})

%days;               # (key1, val1, key2, val2 ...)

# The $ should not be recognized as a variable.
    /asdf$/;
/asdf$/;
    m#asdf$#;
m#asdf$#;
    s/asdf$/asdf/;
s/asdf$/asdf/;
    s#asdf$#asdf#;
s#asdf$#asdf#;

# The ${$1} should be recognized as a variable. The end of the regex quote 
# should be recognized.
m#${$1}#;
s#alskdjf#${$1}#;

# ${} style variables can use any BLOCK expression for the identifier.
m#${$foo->[1 + 2]}#;
s#${$foo->[1 + 2 + rand]}#bacon#;

$foo{bar}->{${$baz}->{zip}};

my $bar = <<ffff;
ffff

int( <STDIN> );

sort( <STDIN> );

if ($str == 0 && $str ne "0")  {
   warn "That doesn't look like a number";
}

warn "has nondigits"        if     /\D/;
warn "not a natural number" unless /^\d+$/;             # rejects -3
warn "not an integer"       unless /^-?\d+$/;           # rejects +3
warn "not an integer"       unless /^[+-]?\d+$/;
warn "not a decimal number" unless /^-?\d+\.?\d*$/;     # rejects .2
warn "not a decimal number" unless /^-?(?:\d+(?:\.\d*)?|\.\d+)$/;
warn "not a C float" unless /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/;

@whatever = ();
$#whatever = -1;

scalar(@whatever) == $#whatever - $[ + 1;

scalar(@whatever) == $#whatever + 1;

$element_count = scalar(@whatever);

$Price = '$100';    # not interpolated
print "The price is $Price.\n";     # interpolated

$who = "Larry";
print PASSWD "${who}::0:0:Superuser:/:/bin/perl\n";
print "We use ${who}speak when ${who}'s here.\n";

print v9786;              # prints UTF-8 encoded SMILEY, "\x{263a}"
print v102.111.111;       # prints "foo"
print 102.111.111;        # same

$temp = join($", @ARGV);
system "echo $temp";

system "echo @ARGV";

@foo = ('cc', '-E', $bar);

$foo = ('cc', '-E', $bar);

@foo = ('cc', '-E', $bar);
$foo = @foo;                # $foo gets 3


@foo = (
   1,
   2,
   3,
);


@sauces = <<End_Lines =~ m/(\S.*\S)/g;
   normal tomato
   spicy tomato
   green chile
   pesto
   white wine
End_Lines

(@foo,@bar,&SomeSub,%glarch)

# Stat returns list value.
$time = (stat($file))[8];

# SYNTAX ERROR HERE.
$time = stat($file)[8];  # OOPS, FORGOT PARENTHESES

# Find a hex digit.
$hexdigit = ('a','b','c','d','e','f')[$digit-10];

# A "reverse comma operator".
return (pop(@foo),pop(@foo))[0];

($a, $b, $c) = (1, 2, 3);

($map{'red'}, $map{'blue'}, $map{'green'}) = (0x00f, 0x0f0, 0xf00);

($dev, $ino, undef, undef, $uid, $gid) = stat($file);

$x = (($foo,$bar) = (3,2,1));       # set $x to 3, not 2
$x = (($foo,$bar) = f());           # set $x to f()'s return count

$count = () = $string =~ /\d+/g;
$count = $string =~ /\d+/g;

($a, $b, @rest) = split;
my($a, $b, %rest) = @_;

# same as map assignment above
%map = ('red',0x00f,'blue',0x0f0,'green',0xf00);

%map = (
            red   => 0x00f,
            blue  => 0x0f0,
            green => 0xf00,
);

$rec = {
           witch => 'Mable the Merciless',
           cat   => 'Fluffy the Ferocious',
           date  => '10/31/1776',
};

$field = $query->radio_group(
          name      => 'group_name',
          values    => ['eenie','meenie','minie'],
          default   => 'meenie',
          linebreak => 'true',
          labels    => \%labels
);

%scientists =
(
   "Newton" => "Isaac",
   "Einstein" => "Albert",
   "Darwin" => "Charles",
   "Feynman" => "Richard",
);

print "Darwin's First Name is ", $scientists{"Darwin"}, "\n";

$whoami = $ENV{"USER"};             # one element from the hash
$parent = $ISA[0];                  # one element from the array
$dir    = (getpwnam("daemon"))[7];  # likewise, but with list

($him, $her)   = @folks[0,-1];              # array slice
@them          = @folks[0 .. 3];            # array slice
($who, $home)  = @ENV{"USER", "HOME"};      # hash slice
($uid, $dir)   = (getpwnam("daemon"))[2,7]; # list slice

@days[3..5]    = qw/Wed Thu Fri/;
@colors{'red','blue','green'}
              = (0xff0000, 0x0000ff, 0x00ff00);
@folks[0, -1]  = @folks[-1, 0];

($days[3], $days[4], $days[5]) = qw/Wed Thu Fri/;
($colors{'red'}, $colors{'blue'}, $colors{'green'})
              = (0xff0000, 0x0000ff, 0x00ff00);
($folks[0], $folks[-1]) = ($folks[-1], $folks[0]);

foreach (@array[ 4 .. 10 ]) { s/peter/paul/ }

foreach (@hash{qw[key1 key2]}) {
   s/^\s+//;           # trim leading whitespace
   s/\s+$//;           # trim trailing whitespace
   s/(\w+)/\u\L$1/g;   # "titlecase" words
}

@a = ()[1,0];           # @a has no elements
@b = (@a)[0,1];         # @b has no elements
@c = (0,1)[2,3];        # @c has no elements

@a = (1)[1,0];          # @a has two elements
@b = (1,undef)[1,0,2];  # @b has three elements

while ( ($home, $user) = (getpwent)[7,0]) {
   printf "%-8s %s\n", $user, $home;
}


$fh = *STDOUT;

$fh = \*STDOUT;

sub newopen {
   my $path = shift;
   local  *FH;  # not my!
   open   (FH, $path)          or  return undef;
   return *FH;
}
$fh = newopen('/etc/passwd');

sub myopen {
   open my $fh, "@_"
        or die "Can't open '@_': $!";
   return $fh;
}

{
   my $f = myopen("</etc/motd");
   print <$f>;
   # $f implicitly closed here
}

my $a;
if ($a) {}
my $a;
$a++;

sub myname;
$me = myname $0             or die "can't get myname";


print "Basset hounds got long ears" if length $ear >= 10;
go_outside() and play() unless $is_raining;

print "Hello $_!\n" foreach qw(world Dolly nurse);

# Both of these count from 0 to 10.
print $i++ while $i <= 10;
print $j++ until $j >  10;

do {
   $line = <STDIN>;
} until $line  eq ".\n";

do {{
   next if $x == $y;
   # do something here
}} until $x++ > $z;

LOOP: {
           last if $x = $y**2;
           # do something here
}

if ($x) { print "foo"; }
if ($x) { print "foo"; } else { print "foo"; }
if ($x) { print "foo"; } elsif ($y) { print "foo"; } else { print "foo"; }
label: while ($x) { print "foo"; }
label: while ($x) { print "foo"; } continue { print "foo"; }
label: until ($x) { print "foo"; }
label: until ($x) { print "foo"; } continue { print "foo"; }
label: for ($x; $x; $x) { print "foo"; }
label: foreach x (1..2) { print "foo"; }
label: foreach x (1..2) { print "foo"; } continue { print "foo"; }
label: { print "foo"; } continue { print "foo"; }

if (!open(FOO)) { die "Can't open $FOO: $!"; }
die "Can't open $FOO: $!" unless open(FOO);
open(FOO) or die "Can't open $FOO: $!";     # FOO or bust!
open(FOO) ? 'hi mom' : die "Can't open $FOO: $!";
                   # a bit exotic, that last one


LINE: while (<STDIN>) {
   next LINE if /^#/;      # discard comments
}

LINE: while (<STDIN>) {
   last LINE if /^$/;      # exit when done with header
}

while (<>) {
   chomp;
   if (s/\\$//) {
   }
   # now process $_
}

LINE: while (defined($line = <ARGV>)) {
   chomp($line);
   if ($line =~ s/\\$//) {
   }
   # now process $line
}

# inspired by :1,$g/fred/s//WILMA/
while (<>) {
   ?(fred)?    && s//WILMA $1 WILMA/;
   ?(barney)?  && s//BETTY $1 BETTY/;
   ?(homer)?   && s//MARGE $1 MARGE/;
} continue {
   print "$ARGV $.: $_";
   close ARGV  if eof();           # reset $.
   reset       if eof();           # reset ?pat?
}

if (/pattern/) {{
   last if /fred/;
   next if /barney/; # same effect as "last", but doesn't document as well
   # do something here
}}

my ${%bag};

for ($i = 1; $i < 10; $i++) {
}

$i = 1;
while ($i < 10) {
} continue {
   $i++;
}

$on_a_tty = -t STDIN && -t STDOUT;
sub prompt { print "yes? " if $on_a_tty }
for ( prompt(); <STDIN>; prompt() ) {
   # do something
}

for ( prompt(); defined( $_ = <STDIN> ); prompt() ) {
   # do something
}

for (@ary) { s/foo/bar/ }

for my $elem (@elements) {
   $elem *= 2;
}

for $count (10,9,8,7,6,5,4,3,2,1,'BOOM') {
   print $count, "\n"; sleep(1);
}

for (1..15) { print "Merry Christmas\n"; }

foreach $item (split(/:[\\\n:]*/, $ENV{TERMCAP})) {
   print "Item: $item\n";
}

for (my $i = 0; $i < @ary1; $i++) {
   for (my $j = 0; $j < @ary2; $j++) {
           last; # can't go to outer :-(
   }
   # this is where that last takes me
}


OUTER: for my $wid (@ary1) {
INNER:   for my $jet (@ary2) {
           next OUTER if $wid > $jet;
           $wid += $jet;
        }
     }

SWITCH: {
   if (/^abc/) { $abc = 1; last SWITCH; }
   if (/^def/) { $def = 1; last SWITCH; }
   if (/^xyz/) { $xyz = 1; last SWITCH; }
   $nothing = 1;
}

SWITCH: {
   $abc = 1, last SWITCH  if /^abc/;
   $def = 1, last SWITCH  if /^def/;
   $xyz = 1, last SWITCH  if /^xyz/;
   $nothing = 1;
}

SWITCH: {
   /^abc/ && do { $abc = 1; last SWITCH; };
   /^def/ && do { $def = 1; last SWITCH; };
   /^xyz/ && do { $xyz = 1; last SWITCH; };
   $nothing = 1;
}

SWITCH: {
   /^abc/      && do {
                       $abc = 1;
                       last SWITCH;
                  };

   /^def/      && do {
                       $def = 1;
                       last SWITCH;
                  };

   /^xyz/      && do {
                       $xyz = 1;
                       last SWITCH;
                   };
   $nothing = 1;
}

SWITCH: {
   /^abc/ and $abc = 1, last SWITCH;
   /^def/ and $def = 1, last SWITCH;
   /^xyz/ and $xyz = 1, last SWITCH;
   $nothing = 1;
}

if (/^abc/)
   { $abc = 1 }
elsif (/^def/)
   { $def = 1 }
elsif (/^xyz/)
   { $xyz = 1 }
else
   { $nothing = 1 }

SWITCH: for ($where) {
           /In Card Names/     && do { push @flags, '-e'; last; };
           /Anywhere/          && do { push @flags, '-h'; last; };
           /In Rulings/        && do {                    last; };
           die "unknown value for form variable where: `$where'";

$amode = do {
   if     ($flag & O_RDONLY) { "r" }       # XXX: isn't this 0?
   elsif  ($flag & O_WRONLY) { ($flag & O_APPEND) ? "a" : "w" }
   elsif  ($flag & O_RDWR)   {
   }
};

   print do {
                             "read-only";
   };

# pick out jargon file page based on browser
$dir = 'http://www.wins.uva.nl/~mes/jargon';
for ($ENV{HTTP_USER_AGENT}) {
   $page  =    /Mac/            && 'm/Macintrash.html'
            || /Win(dows )?NT/  && 'e/evilandrude.html'
            || /Win|MSIE|WebTV/ && 'm/MicroslothWindows.html'
            || /Linux/          && 'l/Linux.html'
            || /HP-UX/          && 'h/HP-SUX.html'
            || /SunOS/          && 's/ScumOS.html'
            ||                     'a/AppendixB.html';
}
print "Location: $dir/$page\015\012\015\012";

goto(("FOO", "BAR", "GLARCH")[$i]);

=head1 Here There Be Pods!



=item snazzle($)

The snazzle() function will behave in the most spectacular
form that you can possibly imagine, not even excepting
cybernetic pyrotechnics.

=cut     

sub snazzle($) {
   my $thingie = shift;
}


$a=3;
=secret stuff
warn "Neither POD nor CODE!?"
=cut 
print "got $a\n";





# example: '# line 42 "new_filename.plx"'
/^\#   \s*
 line \s+ (\d+)   \s*
 (?:\s("?)([^"]+)\2)? \s*
$/x



die 'foo';

@ary = (1, 3, sort 4, 2);
print @ary;         # prints 1324


# These evaluate exit before doing the print:
print($foo, exit);  # Obviously not what you want.
print $foo, exit;   # Nor is this.

# These do the print before evaluating exit:
(print $foo), exit; # This is what you want.
print($foo), exit;  # Or this.
print ($foo), exit; # Or even this.


print ($foo & 255) + 1, "\n";


1 + 1, "\n";    # Obviously not what you meant.


print(($foo & 255) + 1, "\n");


print $i++;  # prints 0
print ++$j;  # prints 1


$i = $i ++;
print ++ $i + $i ++;

print ++($foo = '99');      # prints '100'
print ++($foo = 'a0');      # prints 'a1'
print ++($foo = 'Az');      # prints 'Ba'
print ++($foo = 'zz');      # prints 'aaa'


print '-' x 80;             # print row of dashes

print "\t" x ($tab/8), ' ' x ($tab%8);      # tab over

@ones = (1) x 80;           # a list of 80 1's
@ones = (5) x @ones;        # set all elements to 5

chdir $foo    || die;       # (chdir $foo) || die
chdir($foo)   || die;       # (chdir $foo) || die
chdir ($foo)  || die;       # (chdir $foo) || die
chdir +($foo) || die;       # (chdir $foo) || die


chdir $foo * 20;    # chdir ($foo * 20)
chdir($foo) * 20;   # (chdir $foo) * 20
chdir ($foo) * 20;  # (chdir $foo) * 20
chdir +($foo) * 20; # chdir ($foo * 20)

rand 10 * 20;       # rand (10 * 20)
rand(10) * 20;      # (rand 10) * 20
rand (10) * 20;     # (rand 10) * 20
rand +(10) * 20;    # rand (10 * 20)
   print "Even\n" if ($x & 1) == 0;
   print "false\n" if (8 | 2) != 10;

$home = $ENV{'HOME'} || $ENV{'LOGDIR'} ||
   (getpwuid($<))[7] || die "You're homeless!\n";


@a = scalar(@b) || @c;      # really meant this
@a = @b ? @b : @c;          # this works fine, though


unlink "alpha", "beta", "gamma"
       or gripe(), next LINE;


unlink("alpha", "beta", "gamma")
       || (gripe(), next LINE);

for (1 .. 1_000_000) {
   # code
}


if (101 .. 200) { print; } # print 2nd hundred lines, short for
                          #   if ($. == 101 .. $. == 200) ...

next LINE if (1 .. /^$/);  # skip header lines, short for
                          #   ... if ($. == 1 .. /^$/);
                          # (typically in a loop labeled LINE)

s/^/> / if (/^$/ .. eof());  # quote body

# parse mail messages
while (<>) {
   $in_header =   1  .. /^$/;
   $in_body   = /^$/ .. eof;
   if ($in_header) {
       # ...
   } else { # in body
       # ...
   }
} continue {
   close ARGV if eof;             # reset $. each file
}

@lines = ("   - Foo",
         "01 - Bar",
         "1  - Baz",
         "   - Quux");

foreach (@lines) {
   if (/0/ .. /1/) {
       print "$_\n";
   }
}

for (101 .. 200) { print; } # print $_ 100 times
@foo = @foo[0 .. $#foo];    # an expensive no-op
@foo = @foo[$#foo-4 .. $#foo];      # slice last 5 items

@alphabet = ('A' .. 'Z');

$hexdigit = (0 .. 9, 'a' .. 'f')[$num & 15];

@z2 = ('01' .. '31');  print $z2[$mday];

@list = (2.18 .. 3.14); # same as @list = (2 .. 3);

printf "I have %d dog%s.\n", $n,
       ($n == 1) ? '' : "s";

$a = $ok ? $b : $c;  # get a scalar
@a = $ok ? @b : @c;  # get an array
$a = $ok ? @b : @c;  # oops, that's just a count!


($a_or_b ? $a : $b) = $c;


$a += ($a % 2) ? 10 : 2;

$a += 2;

$a = $a + 2;


($tmp = $global) =~ tr [A-Z] [a-z];

($a += 2) *= 3;

$a += 2;
$a *= 3;

use constant FOO => "something";

my %h = ( FOO => 23 );

my %h = ("FOO", 23);

my %h = ("something", 23);

%hash = ( $key => $value );
login( $username => $password );

open HANDLE, "filename"
   or die "Can't open: $!\n";


print FH $data              or die "Can't write to FH: $!";


($a = $b) or $c;            # really means this
$a = $b || $c;              # better written this way


@info = stat($file) || die;     # oops, scalar sense of stat!
@info = stat($file) or die;     # better, now @info gets its due

'$not_var';
q{$var};

"$var";      
qq{$var};
`$var`;
qx{$var};
qw{$not var};
/$var/;
m{$var};

qr{$var};          
s{$var}{$var};
tr{}{};

<<EOF
$is_var
EOF

<<'EOF'
$not_var
EOF

while (<>) {
   if (?^$?) {
                       # blank line between header and body
   }
} continue {
   reset if eof;       # clear ?? status for next file
}

open(TTY, '/dev/tty');
<TTY> =~ /^y/i && foo();    # do foo if desired

if (/Version: *([0-9.]*)/) { $version = $1; }

next if m#^/usr/spool/uucp#;

# poor man's grep
$arg = shift;
while (<>) {
   print if /$arg/o;       # compile only once
}

if (($F1, $F2, $Etc) = ($foo =~ /^(\S+)\s+(\S+)\s*(.*)/))


# list context
($one,$five,$fifteen) = (`uptime` =~ /(\d+\.\d+)/g);

# scalar context
$/ = "";
while (defined($paragraph = <>)) {
   while ($paragraph =~ /[a-z]['")]*[.!?]+['")]*\s/g) {
       $sentences++;
   }
}
print "$sentences\n";

# using m//gc with \G
$_ = "ppooqppqq";
while ($i++ < 2) {
   print "1: '";
   print $1 while /(o)/gc; print "', pos=", pos, "\n";
   print "2: '";
   print $1 if /\G(q)/gc;  print "', pos=", pos, "\n";
   print "3: '";
   print $1 while /(p)/gc; print "', pos=", pos, "\n";
}
print "Final: '$1', pos=",pos,"\n" if /\G(.)/;

$_ = <<'EOL';
     $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL

LOOP:
   {
     print(" digits"),         redo LOOP if /\G\d+\b[,.;]?\s*/gc;
     print(" lowercase"),      redo LOOP if /\G[a-z]+\b[,.;]?\s*/gc;
     print(" UPPERCASE"),      redo LOOP if /\G[A-Z]+\b[,.;]?\s*/gc;
     print(" Capitalized"),    redo LOOP if /\G[A-Z][a-z]+\b[,.;]?\s*/gc;
     print(" MiXeD"),          redo LOOP if /\G[A-Za-z]+\b[,.;]?\s*/gc;
     print(" alphanumeric"),   redo LOOP if /\G[A-Za-z0-9]+\b[,.;]?\s*/gc;
     print(" line-noise"),     redo LOOP if /\G[^A-Za-z0-9]+/gc;
     print ". That's all!\n";
   }

$foo = q!I said, "You said, 'She said it.'"!;
$bar = q('This is it.');
$baz = '\n';                # a two-character string

$_ .= qq
(*** The previous line contains the naughty word "$1".\n)
           if /\b(tcl|java|python)\b/i;      # :-)
$baz = "\n";                # a one-character string


$rex = qr/my.STRING/is;
s/$rex/foo/;

s/my.STRING/foo/is;

$re = qr/$pattern/;
$string =~ /foo${re}bar/;   # can be interpolated in other patterns
$string =~ $re;             # or used standalone
$string =~ /$re/;           # or this way

sub match {
   my $patterns = shift;
   my @compiled = map qr/$_/i, @$patterns;
   grep {
       my $success = 0;
       foreach my $pat (@compiled) {
           $success = 1, last if /$pat/;
       }
       $success;
   } @_;
}

$output = `cmd 2>&1`;

$output = `cmd 2>/dev/null`;

$output = `cmd 2>&1 1>/dev/null`;

$output = `cmd 3>&1 1>&2 2>&3 3>&-`;

system("program args 1>program.stdout 2>program.stderr");

$perl_info  = qx(ps $$);            # that's Perl's $$
$shell_info = qx'ps $$';            # that's the new shell's $$

qw(foo bar baz)

use POSIX qw( setlocale localeconv )
@EXPORT = qw( foo bar baz );

s/\bgreen\b/mauve/g;                # don't change wintergreen

$path =~ s|/usr/bin|/usr/local/bin|;

s/Login: $foo/Login: $bar/; # run-time pattern

($foo = $bar) =~ s/this/that/;      # copy first, then change

$count = ($paragraph =~ s/Mister\b/Mr./g);  # get change-count

$_ = 'abc123xyz';
s/\d+/$&*2/e;               # yields 'abc246xyz'
s/\d+/sprintf("%5d",$&)/e;  # yields 'abc  246xyz'
s/\w/$& x 2/eg;             # yields 'aabbcc  224466xxyyzz'

s/%(.)/$percent{$1}/g;      # change percent escapes; no /e
s/%(.)/$percent{$1} || $&/ge;       # expr now, so /e
s/^=(\w+)/&pod($1)/ge;      # use function call

# expand variables in $_, but dynamics only, using
# symbolic dereferencing
s/\$(\w+)/${$1}/g;

# Add one to the value of any numbers in the string
s/(\d+)/1 + $1/eg;

# This will expand any embedded scalar variable
# (including lexicals) in $_ : First $1 is interpolated
# to the variable name, and then evaluated
s/(\$\w+)/$1/eeg;

# Delete (most) C comments.
$program =~ s {
   /\*     # Match the opening delimiter.
   .*?     # Match a minimal number of characters.
   \*/     # Match the closing delimiter.
} []gsx;

s/^\s*(.*?)\s*$/$1/;        # trim whitespace in $_, expensively

for ($variable) {           # trim whitespace in $variable, cheap
   s/^\s+//;
   s/\s+$//;
}

s/([^ ]*) *([^ ]*)/$2 $1/;  # reverse 1st two fields

# put commas in the right places in an integer
1 while s/(\d)(\d\d\d)(?!\d)/$1,$2/g;

# expand tabs to 8-column spacing
1 while s/\t+/' ' x (length($&)*8 - length($`)%8)/e;

$ARGV[1] =~ tr/A-Z/a-z/;    # canonicalize to lower case

$cnt = tr/*/*/;             # count the stars in $_

$cnt = $sky =~ tr/*/*/;     # count the stars in $sky

$cnt = tr/0-9//;            # count the digits in $_

tr/a-zA-Z//s;               # bookkeeper -> bokeper

($HOST = $host) =~ tr/a-z/A-Z/;

tr/a-zA-Z/ /cs;             # change non-alphas to single space

tr [\200-\377]
  [\000-\177];             # delete 8th bit

tr/AAA/XYZ/

eval "tr/$oldlist/$newlist/";
die $@ if $@;

eval "tr/$oldlist/$newlist/, 1" or die $@;

  print <<EOF;
The price is $Price.
EOF

  print << "EOF"; # same as above
The price is $Price.
EOF

  print << `EOC`; # execute commands
echo hi there
echo lo there
EOC

  print <<"foo", <<"bar"; # you can stack them
I said foo.
foo
I said bar.
bar

  myfunc(<< "THIS", 23, <<'THAT');
Here's a line
or two.
THIS
and here's another.
THAT

($quote = <<'FINIS') =~ s/^\s+//gm;
  The Road goes ever on and on,
  down from the door where it began.
FINIS


s/this/<<E . 'that'
. 'more '/eg;
the other
E


while (defined($_ = <STDIN>)) { print; }
while ($_ = <STDIN>) { print; }
while (<STDIN>) { print; }
for (;<STDIN>;) { print; }
print while defined($_ = <STDIN>);
print while ($_ = <STDIN>);
print while <STDIN>;


while (my $line = <STDIN>) { print $line }


while (($_ = <STDIN>) ne '0') { ... }
while (<STDIN>) { last unless $_; ... }



while (<>) {
}


unshift(@ARGV, '-') unless @ARGV;
while ($ARGV = shift) {
   open(ARGV, $ARGV);
   while (<ARGV>) {
   }
}

@ARGV = grep { -f && -T } glob('*') unless @ARGV;


@ARGV = map { /\.(gz|Z)$/ ? "gzip -dc < $_ |" : $_ } @ARGV;


while ($_ = $ARGV[0], /^-/) {
   shift;
   last if /^--$/;
   if (/^-D(.*)/) { $debug = $1 }
   if (/^-v/)     { $verbose++  }
}

while (<>) {
}



$fh = \*STDIN;
$line = <$fh>;

while (<*.c>) {
   chmod 0644, $_;
}
open(FOO, "echo *.c | tr -s ' \t\r\f' '\\012\\012\\012\\012'|");
while (<FOO>) {
   chomp;
   chmod 0644, $_;
}


chmod 0644, <*.c>;


($file) = <blurch*>;


$file = <blurch*>;



@files = glob("$dir/*.[ch]");
@files = glob($files[$i]);


foreach $file (@filenames) {
   if (-s $file > 5 + 100 * 2**16) {  }
}

1 while foo();

# ASCII-based examples
print "j p \n" ^ " a h";            # prints "JAPH\n"
print "JA" | "  ph\n";              # prints "japh\n"
print "japh\nJunk" & '_____';       # prints "JAPH\n";
print 'p N$' ^ " E<H\n";            # prints "Perl\n";


$foo =  150  |  105;        # yields 255  (0x96 | 0x69 is 0xFF)
$foo = '150' |  105;        # yields 255
$foo =  150  | '105';       # yields 255
$foo = '150' | '105';       # yields string '155' (under ASCII)

$baz = 0+$foo & 0+$bar;     # both ops explicitly numeric
$biz = "$foo" ^ "$bar";     # both ops explicitly stringy


use integer;


no integer;


printf "%.20g\n", 123456789123456789;
#        produces 123456789123456784


sub fp_equal {
   my ($X, $Y, $POINTS) = @_;
   my ($tX, $tY);
   $tX = sprintf("%.${POINTS}g", $X);
   $tY = sprintf("%.${POINTS}g", $Y);
   return $tX eq $tY;
}


use Math::BigInt;
$x = Math::BigInt->new('123456789123456789');
print $x * $x;

# prints +15241578780673678515622620750190521

__END__
foo at bzzzt line 201.

# line 200 "bzzzt"
eval qq[\n#line 2001 ""\ndie 'foo']; print $@;
__END__
foo at - line 2001.

eval qq[\n#line 200 "foo bar"\ndie 'foo']; print $@;
__END__
foo at foo bar line 200.

# line 345 "goop"
eval "\n#line " . __LINE__ . ' "' . __FILE__ ."\"\ndie 'foo'";
print $@;
__END__
foo at goop line 345.


