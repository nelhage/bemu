#include "bemu.h"
#include <unistd.h>
#include <getopt.h>

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


int main(int argc, char **argv)
{
    int fd;
    int arg;
    struct stat stat;
    struct timeval start, end, delta;
    char *filename;
    bool emulate = 0;
    bool do_time = 0;
    bool dump = 0;

    if(argc < 2) {
        printf("Usage: %s <file.bin>\n", argv[0]);
        exit(-1);
    }

    while((arg = getopt(argc, argv, "det")) > 0) {
        switch(arg) {
        case 'd':
            dump = 1;
            break;
        case 't':
            do_time = 1;
            break;
        case 'e':
            emulate = 1;
            break;
        }
    }

    filename = argv[optind];

    fd = open(filename, O_RDWR);
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

    gettimeofday(&start, NULL);
    if(emulate) {
        while(!CPU.halt) {
            bcpu_step_one();
        }
    } else {
        bt_run();
    }
    gettimeofday(&end, NULL);

    if(do_time) {
        timeval_subtract(&delta, &end, &start);
        printf("Executed in %ds.%dus\n", delta.tv_sec, delta.tv_usec);
    }

    if(dump) {
        int i;
        printf("[%08x] Done\n", CPU.PC);
        for(i = 0; i < 32; i++) {
            printf("[%02x] %08x ", i, CPU.regs[i]);
            if(i % 4 == 3) {
                printf("\n");
            }
        }
    }

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

    exit(-1);
}
