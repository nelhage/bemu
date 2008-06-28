env = Environment(CFLAGS = '-g -DDEBUG', CCPATH = '.')

env.Program('x86-test.c')
env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

bemu = env.Program('bemu', ['bemu.c', 'bcpu.c', 'bdecode.c', 'bt.c'])
Default(bemu)

tags = env.Command('TAGS', '', 'etags *.[ch]')
AlwaysBuild(tags)

