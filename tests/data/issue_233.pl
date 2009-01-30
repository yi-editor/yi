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
