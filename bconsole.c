#include "bemu.h"

#include <termios.h>
#include <errno.h>
#include <pthread.h>
#include <poll.h>
#include <signal.h>

static pthread_cond_t console_cond;
static pthread_mutex_t console_mutex;
static pthread_t console_thread;

static struct termios saved_termios;
static int saved_flags;
static int kbd_char;

void* console_process(void *arg UNUSED) {
    int err;
    struct pollfd pollfd = {
        .fd   = 0,
        .events = POLLIN
    };
    while(1) {
        err = poll(&pollfd, 1, -1);
        if(err < 0) {
            if(errno == EINTR) continue;
            panic("Poll returned error: %s\n", strerror(errno));
        }
        LOG("Keyboard interrupt!\n");
        pthread_mutex_lock(&console_mutex);
        if(read(0, &kbd_char, 1) > 0) {
            set_interrupt(INT_KBD);
        }
        pthread_cond_wait(&console_cond, &console_mutex);
        pthread_mutex_unlock(&console_mutex);
    }
    return NULL;
}

void console_open(bool interrupt) {
    LOG("console_open(%d)", interrupt);
    struct termios termios;
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

    signal(SIGIO, SIG_IGN);

    saved_flags = fcntl(0, F_GETFL);
    flags = saved_flags|O_ASYNC;
    fcntl(0, F_SETFL, flags);

    setvbuf(stdout, NULL, _IONBF, 0);

    if(interrupt) {
        LOG("Creating keyboard processing thread...");
        if(pthread_mutex_init(&console_mutex, NULL) < 0) {
            panic("Can't create console mutex: %s", strerror(errno));
        }
        if(pthread_cond_init(&console_cond, NULL) < 0) {
            panic("Can't create console condition var: %s", strerror(errno));
        }
        if(pthread_create(&console_thread, NULL,
                          console_process, NULL) < 0) {
            panic("Can't pthread_create: %s", strerror(errno));
        }
    }
}

void console_close() {
    tcsetattr(0, TCSANOW, &saved_termios);
    fcntl(0, F_SETFL, saved_flags);
}

void beta_wrchr(int chr) {
    putchar(chr);
}

int beta_rdchr() {
    int c;

    pthread_mutex_lock(&console_mutex);
    c = kbd_char;
    kbd_char = 0;
    clear_interrupt(INT_KBD);
    pthread_mutex_unlock(&console_mutex);
    pthread_cond_signal(&console_cond);
    return c;
}

