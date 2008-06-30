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
#include <stdarg.h>

#include "bdecode.h"
#include "bcpu.h"
#include "x86.h"
#include "bt.h"
#include "bclock.h"

#ifdef DEBUG
#define LOG(fmt, ...) printf("%s:%d: " fmt "\n", __FILE__, __LINE__, ## __VA_ARGS__)
#define ASSERT(x)     if(!(x)) {LOG("FAILED ASSERT : %s", #x); exit(1);}
#else
#define LOG(fmt, ...)
#define ASSERT(x)
#endif

#define panic(fmt, ...) __panic(__FILE__, __LINE__, fmt, ## __VA_ARGS__);
void __panic(char *file, int line, char *fmt, ...) __attribute__((noreturn));

#endif
