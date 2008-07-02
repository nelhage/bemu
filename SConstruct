env = Environment(CFLAGS = '-O0 -g', CCPATH = '.')
debug = ARGUMENTS.get('debug', 0)
if(debug):
    env.Append(CFLAGS = ' -DDEBUG')

env.Program('uasm/uasm.c')

env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

bemu = env.Program('bemu', ['bemu.c', 'bcpu.c',
                            'bdecode.c', 'bt.c',
                            'bt_helper.S', 'bclock.c',
                            'bconsole.c'], LIBS = '-lrt')
Default(bemu)

tags = env.Command('TAGS', '', 'etags *.[ch]')
AlwaysBuild(tags)

