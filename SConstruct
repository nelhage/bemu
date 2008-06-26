env = Environment(CFLAGS = '-g -DDEBUG')

env.Program('bemu', ['bemu.c', 'bcpu.c', 'bdecode.c'], CCPATH = '.')
env.Program('x86-test.c')
env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')


