use strict;
use warnings;
use List::Util qw(max);

my %arith = (   # imm, r   imm, r/m      r, r/m       r/m, r
    add     =>  [ undef,   [0x81, 0],    0x01,        0x03   ],
    and     =>  [ undef,   [0x81, 4],    0x21,        0x23   ],
    cmp     =>  [ undef,   [0x81, 7],    0x39,        0x3B   ],
    lea     =>  [ undef,   undef,        undef,       0x8D   ],
    mov     =>  [ 0xB8,    [0xC7, 0],    0x89,        0x8B   ],
    or      =>  [ undef,   [0x81, 1],    0x09,        0x0B   ],
    sub     =>  [ undef,   [0x81, 5],    0x29,        0x2B   ],
    test    =>  [ undef,   [0xF7, 0],    0x85,        undef  ],
    xor     =>  [ undef,   [0x81, 6],    0x31,        0x33   ]
   );

sub h($) {sprintf "0x%02x", shift}

while (my ($mnm, $opc) = each %arith) {
    print "/* " . uc $mnm . " */\n\n";
    
    my $imm_r = shift @$opc;
    if(defined $imm_r) {
        opcode($mnm, "imm32", "r32", ["dst"] => sub {
                   byte(h($imm_r) . "+dst");
               });
    }
    
    my $imm_rm = shift @$opc;
    if(defined $imm_rm) {
        my ($base, $ext) = @$imm_rm;
        opcode($mnm, "imm32", "rm32", [qw(mod reg)] => sub {
                   byte(h $base);
                   modrm('mod', h $ext, 'reg');
               });
    }
    
    my $r_rm = shift @$opc;
    if(defined $r_rm) {
        opcode($mnm, "r32", "rm32", [qw(src mod reg)] => sub {
                   byte(h $r_rm);
                   modrm('mod', 'src', 'reg');
               });
    }
    my $rm_r = shift @$opc;
    if(defined $rm_r) {
        opcode($mnm, "rm32", "r32", [qw(mod reg dst)] => sub {
                   byte(h $rm_r);
                   modrm('mod', 'dst', 'reg');
               });
    }
}

our @LINES;

sub opcode {
    my $mnm = shift;
    my @asmargs = ("x86", "$mnm");
    my ($arg, $margs, $sub);
    while(!ref($arg = shift)) {
        push @asmargs, $arg;
    }
    $margs = $arg;
    $sub = shift;
  {
      local @LINES = ();
      $sub->();
      unshift @LINES, ("#define " .
                       join("_", map {uc} @asmargs) .
                       "(" . join (",", 'buf', @$margs) . ")");
      my $mlen = max map{length} @LINES;
      my $macro = join("\\\n",
                       map{$_ . (" " x ($mlen + 2 - length $_)) } @LINES);
      $macro =~ s/\s*$//;
      print "$macro\n\n";
  }   
}

sub byte {
    my $byte = shift;
    push @LINES, " " x 8 . "X86_BYTE(buf, $byte);";
}

sub modrm {
    my ($mod, $regop, $rm) = @_;
    byte("MODRM($mod, $regop, $rm)");
}