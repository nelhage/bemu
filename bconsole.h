#ifndef __BCONSOLE_H__
#define __BCONSOLE_H__

void console_open(bool interrupt);
void console_close();

void beta_wrchr(int chr);
int  beta_rdchr()

#endif
