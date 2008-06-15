#ifndef __BEMU_H__
#define __BEMU_H__

#include <sys/mman.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include "bdecode.h"
#include "bcpu.h"

#ifdef DEBUG
#define LOG(fmt, ...) printf("%s:%d: " fmt "\n", __FILE__, __LINE__, __VA_ARGS__)
#else
#define LOG(fmt, ...)
#endif


#endif
