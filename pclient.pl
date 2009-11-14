use v5.10;
use strict;
use warnings;
use Socket;
use JSON::XS;
use Term::ReadKey;
use Data::Dumper;
socket(SERVER,PF_UNIX,SOCK_STREAM,0);
connect(SERVER, sockaddr_un('/tmp/pserver-pawel/pserver'));
sub input {
    my ($rin,$win,$ein);
    $rin = $win = $ein = '';
    vec($rin,fileno(SERVER),1) = 1;
    $ein = $rin | $win;
    my $select = select($rin,$win,$ein,0);
    $select;
}
sub command {
    my ($command) = shift;
    print SERVER encode_json($command)."\n";
}

my $oldfh = select(SERVER);
$| = 1;
select($oldfh);

print SERVER <<'END';
{"ProgramName":"pserver"}
{"CommandLineArgs":[]}
{"Environment":[["SHELL","/bin/bash"],["TERM","screen"],["WINDOWID","10485809"],["HUSHLOGIN","FALSE"],["OLDPWD","/home/pawel/app-persistent-orig"],["USER","pawel"],["LS_COLORS","no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.svgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.lzma=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.rar=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:"],["STD_RED_CACHEDIR","/home/pawel/std_red_cache"],["TERMCAP","SC|screen|VT 100/ANSI X3.64 virtual terminal:\\\n\t:DO=\\E[%dB:LE=\\E[%dD:RI=\\E[%dC:UP=\\E[%dA:bs:bt=\\E[Z:\\\n\t:cd=\\E[J:ce=\\E[K:cl=\\E[H\\E[J:cm=\\E[%i%d;%dH:ct=\\E[3g:\\\n\t:do=^J:nd=\\E[C:pt:rc=\\E8:rs=\\Ec:sc=\\E7:st=\\EH:up=\\EM:\\\n\t:le=^H:bl=^G:cr=^M:it#8:ho=\\E[H:nw=\\EE:ta=^I:is=\\E)0:\\\n\t:li#24:co#80:am:xn:xv:LP:sr=\\EM:al=\\E[L:AL=\\E[%dL:\\\n\t:cs=\\E[%i%d;%dr:dl=\\E[M:DL=\\E[%dM:dc=\\E[P:DC=\\E[%dP:\\\n\t:im=\\E[4h:ei=\\E[4l:mi:IC=\\E[%d@:ks=\\E[?1h\\E=:\\\n\t:ke=\\E[?1l\\E>:vi=\\E[?25l:ve=\\E[34h\\E[?25h:vs=\\E[34l:\\\n\t:ti=\\E[?1049h:te=\\E[?1049l:us=\\E[4m:ue=\\E[24m:so=\\E[3m:\\\n\t:se=\\E[23m:mb=\\E[5m:md=\\E[1m:mr=\\E[7m:me=\\E[m:ms:\\\n\t:Co#8:pa#64:AF=\\E[3%dm:AB=\\E[4%dm:op=\\E[39;49m:AX:\\\n\t:vb=\\Eg:G0:as=\\E(0:ae=\\E(B:\\\n\t:ac=\\140\\140aaffggjjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~..--++,,hhII00:\\\n\t:po=\\E[5i:pf=\\E[4i:k0=\\E[10~:k1=\\EOP:k2=\\EOQ:k3=\\EOR:\\\n\t:k4=\\EOS:k5=\\E[15~:k6=\\E[17~:k7=\\E[18~:k8=\\E[19~:\\\n\t:k9=\\E[20~:k;=\\E[21~:F1=\\E[23~:F2=\\E[24~:F3=\\EO2P:\\\n\t:F4=\\EO2Q:F5=\\EO2R:F6=\\EO2S:F7=\\E[15;2~:F8=\\E[17;2~:\\\n\t:F9=\\E[18;2~:FA=\\E[19;2~:kb=:K2=\\EOE:kB=\\E[Z:\\\n\t:*4=\\E[3;2~:*7=\\E[1;2F:#2=\\E[1;2H:#3=\\E[2;2~:#4=\\E[1;2D:\\\n\t:%c=\\E[6;2~:%e=\\E[5;2~:%i=\\E[1;2C:kh=\\E[1~:@1=\\E[1~:\\\n\t:kH=\\E[4~:@7=\\E[4~:kN=\\E[6~:kP=\\E[5~:kI=\\E[2~:kD=\\E[3~:\\\n\t:ku=\\EOA:kd=\\EOB:kr=\\EOC:kl=\\EOD:km:"],["PATH","/home/pawel/.cabal/bin:/usr/local/bin:/usr/bin:/bin:/usr/games"],["MAIL","/var/mail/pawel"],["STY","8923.pts-4.takielunek"],["PWD","/home/pawel/fake_server"],["EDITOR","vim"],["LANG","en_US.UTF-8"],["HISTCONTROL","ignoredups"],["HOME","/home/pawel"],["SHLVL","4"],["LOGNAME","pawel"],["WINDOW","0"],["LESSOPEN","| /usr/bin/lesspipe %s"],["WINDOWPATH","7"],["DISPLAY",":0.0"],["LESSCLOSE","/usr/bin/lesspipe %s %s"],["COLORTERM","gnome-terminal"],["XAUTHORITY","/home/pawel/.Xauthority"],["_","/home/pawel/.cabal/bin/pclient"]]}
{"WorkingDirectory":"/home/pawel/fake_server"}
{"Capabilities":["Dumb"]}
END
ReadMode 4;
while (1) {
    while (input()) {
        my $input = decode_json(scalar <SERVER>);
        if (defined $input->{NormalOutput}) {
            print $input->{NormalOutput};
        } elsif (defined $input->{Exit}) {
            exit $input->{Exit};
        } elsif (defined $input->{ErrorOutput}) {
            print STDERR $input->{ErrorOutput};
        } else {
            print Dumper($input);
        }
    }
    my $key = ReadKey -1;
    if (defined $key) {
        command {KeyPress=>$key};
    }
}
END { ReadMode 0 }
