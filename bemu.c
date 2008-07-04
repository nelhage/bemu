#include "bemu.h"

#include <unistd.h>
#include <getopt.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>

/* Subtract the `struct timeval' values X and Y,
   storing the result in RESULT.
   Return 1 if the difference is negative, otherwise 0.  */
int
timeval_subtract (result, x, y)
     struct timeval *result, *x, *y;
{
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }

  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;

  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

void usage() {
    printf("Usage: bemu [OPTIONS] file.bin\n");
    printf("   Options:\n");
    printf("     -e         Emulate; Do not perform binary translation\n");
    printf("     -t         Time program execution\n");
    printf("     -d         Dump CPU state at HALT()\n");
    printf("     -o OPTS    Set CPU options\n");
    printf("\n");
    printf(" Valid options for -o are: clock, tty\n");
    exit(1);
}

struct {
    bool emulate;
    bool do_time;
    bool do_dump;
    bool enable_clock;
    bool kbd_interrupt;
    char *filename;
} cpu_options;

void handle_flags(char *optval) {
    int len;
    char *comma;

    while(1) {
        comma = (char*)strchr(optval, ',');
        if(comma) {
            len = comma - optval;
        } else {
            len = strlen(optval);
        }
        if(!strncmp(optval, "clock", len)) {
            cpu_options.enable_clock = 1;
        } else if(!strncmp(optval, "tty", len)) {
            cpu_options.kbd_interrupt = 1;
        } else {
            fprintf(stderr, "Bad option spec: %s\n", optval);
            usage();
        }
        if(!comma) {
            break;
        }
        optval = comma + 1;
    }
}

void handle_options(int argc, char **argv) {
    int arg;
    while((arg = getopt(argc, argv, "deto:")) > 0) {
        switch(arg) {
        case 'd':
            cpu_options.do_dump = 1;
            break;
        case 't':
            cpu_options.do_time = 1;
            break;
        case 'e':
            cpu_options.emulate = 1;
            break;
        case 'o':
            handle_flags(optarg);
            break;
        default:
            usage();
        }
    }
    if(optind < argc) {
        cpu_options.filename = argv[optind];
    } else {
        usage();
    }
}

int main(int argc, char **argv)
{
    int fd;
    struct stat stat;
    struct timeval start, end, delta;

    handle_options(argc, argv);

    fd = open(cpu_options.filename, O_RDWR);
    if(fd < 0) {
        perror("open");
        exit(-1);
    }

    if(fstat(fd, &stat) < 0) {
        perror("stat");
        exit(-1);
    }

    if((beta_mem = mmap(NULL, stat.st_size, PROT_READ|PROT_WRITE,
                        MAP_PRIVATE, fd, 0)) == MAP_FAILED) {
        perror("mmap");
        exit(-1);
    }
    close(fd);

    bcpu_reset();

    if(cpu_options.enable_clock) {
        start_clock();
    }

    console_open(cpu_options.kbd_interrupt);

    gettimeofday(&start, NULL);
    if(cpu_options.emulate) {
        while(!CPU.halt) {
            bcpu_step_one();
        }
    } else {
        bt_run();
    }
    gettimeofday(&end, NULL);

    if(cpu_options.do_time) {
        timeval_subtract(&delta, &end, &start);
        printf("Executed %d insts in %ds.%dus\n", CPU.inst_count,
               (int)delta.tv_sec, (int)delta.tv_usec);
    }

    if(cpu_options.do_dump) {
        int i;
        printf("[%08x] Done\n", CPU.PC);
        for(i = 0; i < 32; i++) {
            printf("[%02d] %08x ", i, CPU.regs[i]);
            if(i % 4 == 3) {
                printf("\n");
            }
        }
    }

    console_close();

    munmap(beta_mem, stat.st_size);

    return 0;
}

void __panic(char *file, int line, char *fmt, ...)
{
    va_list ap;

    fprintf(stderr, "PANIC[%s:%d]: ", file, line);

    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);

    fprintf(stderr, "\n");

    exit(-1);
}
