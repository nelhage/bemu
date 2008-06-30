#include "bemu.h"

#include <termios.h>
#include <errno.h>
#include <pthread.h>
#include <poll.h>
#include <signal.h>

static struct termios saved_termios;
static pthread_t console_thread;
static int kbd_char;

void* console_process(void *arg) {
    int err;
    struct pollfd pollfd = {
        .fd   = 0,
        .events = POLLIN
    };
    struct timespec timeout = {-1, 0};
    while(1) {
        err = poll(&pollfd, 1, -1);
        if(err < 0) {
            if(errno = EINTR) continue;
            panic("Poll returned error: %s\n", strerror(errno));
        }
        LOG("Keyboard interrupt!\n");
        set_interrupt(INT_KBD);
        read(0, &kbd_char, 1);
    }
    return NULL;
}

void console_open(bool interrupt) {
    LOG("console_open(%d)", interrupt);
    struct termios termios;
    sigset_t set;
    int flags;
    /* Disable echo */
    if(tcgetattr(0, &saved_termios) < 0) {
        panic("Can't tcgettattr(0): %s", strerror(errno));
    }
    termios = saved_termios;
    termios.c_lflag &= ~(ECHO|ICANON);
    if(tcsetattr(0, TCSANOW, &termios) < 0) {
        panic("Can't tcsettattr(0): %s", strerror(errno));
    }

    sigemptyset(&set);
    sigaddset(&set, SIGIO);
    if(sigprocmask(SIG_BLOCK, &set, NULL) < 0) {
        panic("Can't sigprocmask: %s", strerror(errno));
    }

    flags = fcntl(0, F_GETFL);
    flags |= O_ASYNC;
    fcntl(0, F_SETFL, flags);

    if(interrupt) {
        LOG("Creating keyboard processing thread...");
        if(pthread_create(&console_thread, NULL,
                          console_process, NULL) < 0) {
            panic("Can't pthread_create: %s", strerror(errno));
        }
    }
}

void console_close() {
    tcsetattr(0, TCSANOW, &saved_termios);
}

void beta_wrchr(int chr) {
    putchar(chr);
    fflush(stdout);
}

int beta_rdchr() {
    int c = kbd_char;
    kbd_char = 0;
    return c;
}

