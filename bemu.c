#include "bemu.h"

int main(int argc, char **argv)
{
    int fd;
    int cycles = -1;
    struct stat stat;

    if(argc < 2) {
        printf("Usage: %s <file.bin> [<cycles>]\n", argv[0]);
        exit(-1);
    }

    fd = open(argv[1], O_RDWR);
    if(fd < 0) {
        perror("open");
        exit(-1);
    }

    if(argc > 2) {
        if((cycles = atoi(argv[2])) == 0) {
            printf("cycle count must be nonzero.\n");
            exit(-1);
        }
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
    if(cycles > 0) {
        while(cycles > 0) {
            bcpu_step_one();
            cycles--;
        }
    } else {
        while(1) {
            bcpu_step_one();
        }
    }

    munmap(beta_mem, stat.st_size);
    close(fd);

    return 0;
}
