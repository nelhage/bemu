env = Environment(CFLAGS = '-g -DDEBUG', CCPATH = ',')

env.Program('bemu', ['bemu.c', 'bcpu.c', 'bdecode.c'])
env.Program('x86-test.c')
env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

