use MooseX::Declare;
use feature 'switch';

class App::Persistent::Server::Connection {
    use AnyEvent;
    use AnyEvent::Handle;
    use AnyEvent::Subprocess;

    use App::Persistent::StartupInfo;

    use MooseX::Types -declare => ['Handle'];
    use MooseX::Types::Moose qw(GlobRef);

    class_type 'App::Persistent::Server';
    class_type 'AnyEvent::Handle';
    class_type 'AnyEvent::CondVar';

    subtype Handle, as 'AnyEvent::Handle';

    coerce Handle, from GlobRef, via {
        AnyEvent::Handle->new( fh => $_ );
    };

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

    method run(){
        # XXX: twisty maze of spaghetti!

        my $ready = AnyEvent->condvar;
        my $read_headers;

        my $headers;
        $read_headers = sub {
            my ($handle, $msg) = @_;
            my ($k, $v) = %$msg;
            $headers->{$k} = $v;

            my $info = eval {
                App::Persistent::StartupInfo->new($headers);
            };

            if (!$info) {
                $handle->push_read( json => $read_headers );
            }
            else {
                my $subprocess = AnyEvent::Subprocess->new_with_traits(
                    traits           => ['Pty'],
                    code             => sub { $self->server->code->($info) },
                    before_fork_hook => sub {
                        my $run = shift;
                        $self->_mk_printer($run->pty_handle, 'NormalOutput');

                        # TODO: clone the pclient's winsize, not the parent's
                        $run->pty_handle->fh->slave->clone_winsize_from(\*STDIN);

                        $run->completion_condvar->cb(
                            sub {
                                my ($cv) = @_;
                                my $done = $cv->recv;

                                # the JSON serializer confuses Haskell if we
                                # don't 0+ the exit code.  (not sure why it
                                # thinks it's a string)
                                $self->write_json({ 'Exit' => 0+$done->exit_value });
                                $self->socket->on_error( sub { } );
                            },
                        ),

                    }
                );

                my $running_app = $subprocess->run;

                my $read; $read = sub {
                    my ($handle, $msg) = @_;
                    my ($type, $value) = %$msg;

                    given($type){
                        when('KeyPress'){
                            $running_app->pty_handle->push_write($value);
                        }
                        when('EndOfFile'){
                            close $running_app->pty_handle->fh;
                        }
                    }

                    $handle->push_read( json => $read );
                };

                $self->socket->on_error( sub { $running_app->kill } );
                $self->socket->push_read( json => $read );
            }
        };

        $self->socket->push_read( json => $read_headers );

        return; # leak nothing
    }
}

1;
