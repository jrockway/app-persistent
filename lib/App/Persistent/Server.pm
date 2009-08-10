use MooseX::Declare;
use feature 'switch';

class App::Persistent::Server {
    use MooseX::Types::Moose qw(CodeRef Str);
    use MooseX::Types::Path::Class qw(Dir File);
    use MooseX::Types::Set::Object;
    use namespace::autoclean;

    use App::Persistent::Server::Connection;
    use AnyEvent;
    use AnyEvent::Socket qw(tcp_server);

    has 'name' => (
        is       => 'ro',
        isa      => Str,
        required => 1,
        default  => 'pserver',
    );

    has 'namespace' => (
        is       => 'ro',
        isa      => Str,
        required => 1,
        default  => sub { $ENV{USER} },
    );

    has 'socket_directory' => (
        is      => 'ro',
        isa     => Dir,
        lazy    => 1,
        coerce  => 1,
        default => sub {
            my $self = shift;
            return Path::Class::dir('', 'tmp', 'pserver-'.$self->namespace );
        },
    );

    has 'socket_file' => (
        is      => 'ro',
        isa     => File,
        lazy    => 1,
        coerce  => 1,
        default => sub {
            my $self = shift;
            return $self->socket_directory->file($self->name);
        },
    );

    has 'code' => (
        is       => 'ro',
        isa      => CodeRef,
        required => 1,
    );

    has 'control_server_guard' => (
        is         => 'ro',
        lazy_build => 1,
    );

    has 'server_guard' => (
        is         => 'ro',
        lazy_build => 1,
    );

    has 'completion_condvar' => (
        is      => 'ro',
        default => sub {
            AnyEvent->condvar,
        },
    );

    has 'client_set' => (
        is       => 'ro',
        isa      => 'Set::Object',
        required => 1,
        default  => sub { Set::Object->new },
        handles  => {
            add_connection => 'insert',
        },
    );

    method _build_control_server_guard(){
        return tcp_server undef, 1235, sub {
            my ($fh, $host, $port) =  @_;

            my $handle = AnyEvent::Handle->new(
                fh => $fh,
            );

            my $reader; $reader = sub {
                my ($handle, $msg) = @_;
                given($msg->{type}){
                    when('Exit'){
                        $self->completion_condvar->send();
                    }
                    # TODO: status, forcible kill, etc.
                    default {
                        $handle->push_read( json => $reader );
                    }
                }
            };
            $handle->push_read( json => $reader );
        };
    }

    method _build_server_guard() {
        mkdir $self->socket_directory;
        chmod 0700, $self->socket_directory;
        confess 'Socket directory '. $self->socket_directory.
          ' does not exist and could not be created.'
            unless -d $self->socket_directory;

        warn $self->socket_file->stringify;
        return tcp_server 'unix/', $self->socket_file->stringify, sub {
            my ($fh, $host, $port) =  @_;

            print "Got connection from $host:$port\n";

            my $connection = App::Persistent::Server::Connection->new(
                socket => $fh,
                server => $self,
            );
            $self->add_connection($connection);
            $connection->run;
        };
    }

    method start() {
        print "Starting server\n";
        return ($self->server_guard, $self->control_server_guard);
    }
}
