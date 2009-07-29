use MooseX::Declare;
use feature 'switch';

class App::Persistent::Server::Connection {
    use namespace::autoclean;

    use AnyEvent;
    use AnyEvent::Handle;
    use AnyEvent::Subprocess;

    use App::Persistent::StartupInfo;

    use MooseX::Types -declare => ['Handle','Condvar'];
    use MooseX::Types::Moose qw(GlobRef);

    class_type 'App::Persistent::Server';
    class_type 'AnyEvent::Handle';
    class_type 'AnyEvent::CondVar';

    subtype Handle, as 'AnyEvent::Handle';

    coerce Handle, from GlobRef, via {
        AnyEvent::Handle->new( fh => $_ );
    };

    subtype Condvar, as 'AnyEvent::CondVar';

    has 'server' => (
        is       => 'ro',
        isa      => 'App::Persistent::Server',
        required => 1,
        weak_ref => 1,
    );

    has 'socket' => (
        is       => 'ro',
        isa      => Handle,
        required => 1,
        coerce   => 1,
    );

    has 'headers_ready' => (
        is       => 'ro',
        isa      => Condvar,
        required => 1,
        default  => sub {
            return AnyEvent->condvar;
        },
    );

    has 'headers' => (
        is         => 'ro',
        isa        => 'App::Persistent::StartupInfo',
        lazy_build => 1,
    );

    has 'running_app' => (
        is         => 'ro',
        isa        => 'AnyEvent::Subprocess::Running',
        lazy_build => 1,
    );

    method write_json(HashRef $obj) {
        $self->socket->push_write(json => $obj);
        $self->socket->push_write("\n");
    }

    method _mk_printer($read_handle, $type) {
        $read_handle->on_read(sub {
            my $handle = shift;
            my $data = delete $handle->{rbuf};
            $self->write_json({ $type => "$data" });
        });

        # add support for relaying end of stdout/stderr?
    }

    method _build_running_app(){
        my $headers = $self->headers;
        my $subprocess = AnyEvent::Subprocess->new(
            code             => sub { $self->server->code->($headers) },
            before_fork_hook => sub {
                my $run = shift;
                $self->_mk_printer($run->stdout_handle, 'NormalOutput');
                $self->_mk_printer($run->stderr_handle, 'ErrorOutput');

                $run->completion_condvar->cb(
                    sub {
                        my ($cv) = @_;
                        my $done = $cv->recv;
                        $self->write_json({ 'Exit' => $done->exit_value });
                    },
                ),

            }
        );
        return $subprocess->run;
    }

    method _build_headers(){
        return $self->headers_ready->recv;
    }

    method run(){
        my $read; $read = sub {
            my ($handle, $msg) = @_;
            my ($type, $value) = %$msg;

            given($type){
                when('KeyPress'){
                    $self->running_app->stdin_handle->push_write($value);
                }
                when('EndOfFile'){
                    $self->running_app->close_stdin_handle;
                }
            }

            $handle->push_read( json => $read );
        };

        my $read_headers;
        {
            my $headers;
            $read_headers = sub {
                my ($handle, $msg) = @_;
                my ($k, $v) = %$msg;
                $headers->{$k} = $v;

                if (keys %$headers < 5) { # XXX: XXX
                    $handle->push_read( json => $read_headers );
                } else {
                    $self->headers_ready->send(
                        do {
                            App::Persistent::StartupInfo->new($headers);
                        },
                    );
                    $handle->push_read( json => $read );
                }

            };
        }

        $self->socket->push_read( json => $read_headers );
        return $self->running_app;
    }
}

1;
