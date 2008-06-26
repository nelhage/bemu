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
                        MAP_SHARED, fd, 0)) == MAP_FAILED) {
        perror("mmap");
        exit(-1);
    }

    bcpu_reset();

    while(!CPU.halt) {
        bcpu_step_one();
    }

    munmap(beta_mem, stat.st_size);
    close(fd);

    return 0;
}
