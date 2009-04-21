use MooseX::Declare;

class App::Persistent::StartupInfo {
    use namespace::autoclean;
    use MooseX::Types -declare => ['Environment'];
    use MooseX::Types::Path::Class qw(Dir);
    use MooseX::Types::Moose qw(ArrayRef HashRef Str);

    subtype Environment, as HashRef[Str];

    # haskell's JSON implementation sends us weird shit, so
    # this cleans it up
    coerce Environment, from ArrayRef[ArrayRef[Str]], via {
        my @pairs = @$_;
        my $hash;
        for my $pair (@pairs){
            my ($k, $v) = @$pair;
            $hash->{$k} = $v;
        }
        return $hash;
    };

    has 'environment' => (
        init_arg => 'Environment',
        is       => 'ro',
        isa      => Environment,
        required => 1,
        coerce   => 1,
    );

    has 'program_name' => (
        init_arg => 'ProgramName',
        is       => 'ro',
        isa      => Str,
        required => 1,
    );

    has 'working_directory' => (
        init_arg => 'WorkingDirectory',
        is       => 'ro',
        isa      => Dir,
        required => 1,
        coerce   => 1,
    );

    has 'cmdline_args' => (
        init_arg => 'CommandLineArgs',
        is       => 'ro',
        isa      => ArrayRef[Str],
        required => 1,
    );
}

1;
