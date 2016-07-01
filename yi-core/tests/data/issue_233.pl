# The following should be recognized as a HEREDOC but no variable interpolation should be performed.
my $foo = <<'FOO';
bacon bacon $bacon
FOO

# The following should be recognized as a HEREDOC with variable interpolation:
my $bar = <<"FOO";
bacon bacon $bacon
FOO

# The following should be recognized as a HEREDOC with variable interpolation:
my $zab = <<FOO;
bacon bacon $bacon
FOO

# The following should be recognized as a HEREDOC with variable interpolation that ends on the first empty line.
my $fib = << ;
alskjdflaksjdf $bacon
This line should be included in the heredoc.
As should this line, but the next line should end the heredoc.

$fib = 'nothing'; # Should not be recognized as part of the heredoc.

# Should also be recognized as a heredoc even though it'll likely cause a syntax error on compile.
my $bad = <<FOO
Inside HEREDOC
FOO
