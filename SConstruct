env = Environment(CFLAGS = '-O0 -g', CCPATH = '.', LIBS = '-lrt')
debug = ARGUMENTS.get('debug', 0)
if(debug):
    env.Append(CFLAGS = ' -DDEBUG')

env.Program('x86-test.c')
env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

bemu = env.Program('bemu', ['bemu.c', 'bcpu.c',
                            'bdecode.c', 'bt.c',
                            'bt_helper.S', 'bclock.c',
                            'bconsole.c'])
Default(bemu)

tags = env.Command('TAGS', '', 'etags *.[ch]')
AlwaysBuild(tags)

