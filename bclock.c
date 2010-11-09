#include "bemu.h"

#include <sys/time.h>
#include <signal.h>
#include <errno.h>

extern beta_cpu CPU;

static void clock_tick(int signal) {
    set_interrupt(&CPU, INT_CLK);
}

void start_clock(void) {
    struct itimerval timer = {
        { 0, 1000000 / BETA_HZ }, /* Interval */
        { 0, 1000000 / BETA_HZ }, /* Current */
    };

    if(signal(SIGALRM, clock_tick) < 0) {
        perror("signal");
        panic("Can't set timer signal handler");
    }

    if(setitimer(ITIMER_REAL, &timer, NULL) < 0) {
        perror("setitimer");
        panic("Unable to start the Beta clock.");
    }
    LOG("Started the clock at %dHz", BETA_HZ);
}
