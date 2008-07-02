def dep_uasm(target, source, env):
    source.append('uasm/uasm')
    return target, source
uasm = Builder(action = 'uasm/uasm $SOURCE',
               suffix = '.bin',
               src_suffix = '.uasm',
               emitter = dep_uasm)

env = Environment(CFLAGS = '-O0 -g -m32',
                  CCPATH = '.',
                  ASPPFLAGS='-m32',
                  LINKFLAGS='-m32')
env.Append(BUILDERS = {'UAsm': uasm})

debug = ARGUMENTS.get('debug', 0)
if(debug):
    env.Append(CFLAGS = ' -DDEBUG')

env.Command('opcodes.h','insts.pl', 'perl $SOURCE > $TARGET')

bemu = env.Program('bemu', ['bemu.c', 'bcpu.c',
                            'bdecode.c', 'bt.c',
                            'bt_helper.S', 'bclock.c',
                            'bconsole.c'], LIBS = '-lrt')
Default(bemu)

AlwaysBuild(env.Command('TAGS', '', 'etags *.[ch]'))

env.Program('uasm/uasm.c')

tests = ['align', 'bench1', 'bench2', 'bench3', 'bench4',
         'sancheck', 'supervisor', 'timer', 'litmus', 'qsort']
test_targets = []
for t in tests:
    test_targets.append(env.UAsm('tests/' + t + '.uasm'))

AlwaysBuild(env.Command('test', [bemu, test_targets], './run-tests.sh'))
