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

#define offsetof(TYPE, MEMBER)  __builtin_offsetof (TYPE, MEMBER)

#ifdef DEBUG
#define LOG_(lev, fmt, ...) do {                                        \
    if(DEBUG >= lev)                                                    \
        printf("%s:%d: " fmt "\n", __FILE__, __LINE__, ## __VA_ARGS__); \
    } while (0)
#define ASSERT(x)     if(!(x)) {LOG("FAILED ASSERT : %s", #x); abort();}
#else
#define LOG_(lev, fmt, ...)
#define ASSERT(x)
#endif

#define LOG(fmt, ...)   LOG_(1, fmt, ## __VA_ARGS__)
#define LOG0(fmt, ...)   LOG_(0, fmt, ## __VA_ARGS__)
#define LOG1(fmt, ...)   LOG_(1, fmt, ## __VA_ARGS__)

#define panic(fmt, ...) __panic(__FILE__, __LINE__, fmt, ## __VA_ARGS__);
void __panic(const char *file, int line, const char *fmt, ...) __attribute__((noreturn));

#define UNUSED __attribute__((unused))

extern int profile_instructions;

#include "bdecode.h"
#include "bcpu.h"
#include "x86.h"
#include "bt.h"
#include "bclock.h"
#include "bconsole.h"

#endif
