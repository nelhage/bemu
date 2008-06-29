#include "bemu.h"

int main(int argc, char **argv)
{
    int fd;
    struct stat stat;

    if(argc < 2) {
        printf("Usage: %s <file.bin>\n", argv[0]);
        exit(-1);
    }

    fd = open(argv[1], O_RDWR);
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
    bt_run();

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
