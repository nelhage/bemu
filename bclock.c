#include "bemu.h"

#include <time.h>
#include <signal.h>
#include <errno.h>

static timer_t beta_timer;

static void clock_tick(int signal) {
    set_interrupt(INT_CLK);
}

void start_clock(void) {
    struct sigevent evt = {
        .sigev_notify = SIGEV_SIGNAL,
        .sigev_signo  = SIGUSR1
    };
    struct itimerspec spec = {
        .it_value    = {0, 10000000},
        .it_interval = {0, 1000000000/BETA_HZ}
    };

    if(signal(SIGUSR1, clock_tick) < 0) {
        panic("Can't set timer signal handler: %s", strerror(errno));
    }
    if(timer_create(CLOCK_REALTIME, &evt, &beta_timer) < 0) {
        panic("Can't create Beta timer: %s", strerror(errno));
    }
    if(timer_settime(beta_timer, 0, &spec, NULL) < 0) {
        panic("Can't start Beta timer: %s", strerror(errno));
    }
    LOG("Started the clock at %dHz", BETA_HZ);
}
