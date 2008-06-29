env = Environment(CFLAGS = '-O2', CCPATH = '.')
debug = ARGUMENTS.get('debug', 0)
if(debug):
    env.Append(CFLAGS = ' -g -DDEBUG')

env.Program('x86-test.c')
env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

bemu = env.Program('bemu', ['bemu.c', 'bcpu.c', 'bdecode.c', 'bt.c', 'bt_helper.S'])
Default(bemu)

tags = env.Command('TAGS', '', 'etags *.[ch]')
AlwaysBuild(tags)

