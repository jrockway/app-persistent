use MooseX::Declare;
use feature 'switch';

class App::Persistent::Server {
    use MooseX::Types::Moose qw(CodeRef);
    use MooseX::Types::Set::Object;
    use namespace::autoclean;

    use App::Persistent::Server::Connection;
    use AnyEvent;
    use AnyEvent::Socket qw(tcp_server);

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
        return tcp_server undef, 1234, sub {
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
