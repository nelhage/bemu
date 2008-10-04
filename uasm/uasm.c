/* MAYBE microassembler */
/* Changes:
 *  (SAW)	returns 0 on correct assy.
 *		 Accepts char constants, in 'x' syntax.
 *		 Allows multi-line macro bodies, using { ... } syntax.
 *  10/7/84	Made parenthesized expressions work (SAW)
 *  10/11/84	Added .map output file stuff:
 *		 writes binary file name.map, one 16-bit short per
 *		 input file line.  map[i] = uROM address corresponding
 *               to that line.
 *  10/25/87	Kenmac: upped SLINES to 3000
 *  12/8/89	mafetter: Changed the .map output format in order to make it
 *               not be dependent on host machine's byte ordering.
 *  8/27/90	mafetter: Changed the .map output format in order to
 *               accomidate source files which are more than 32,767 bytes
 *               long.  In particular, stemp.uasm is currently >77 Kbytes.
 *               This has been causing problems with the simulator.
 *               Removed SLINES.  Using dynamic allocation instead.
 * 3/93 SAW:	Added "b" modes to fopens, for Mac compatibility.
 *		Changed error printout to be EMACS parsable.
 * 		Made it not bomb on unterminated macro defn.
 *		ADDED CONSTRUCTS
 *		 .align <expr>
 *		    expr-byte aligns dot.  Expr defaults to 4.
 *		 .ascii "Text string"
 *		    assembles characters, no trailing 0 or alignment
 *		    Accepts escapes \r, \n, \\, \", etc
 *		 .text "Text string"
 *		    Assembles text, adds trailing 0, aligns dot.
 */

#ifndef lint
#ifndef saber
static char *rcs_id = "$Id: uasm.c,v 1.5 1993/05/05 15:13:56 chaiken Exp chaiken $";
#endif
#endif

#define _GNU_SOURCE

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define LSIZE	200	/* max size of input line */
#define	MSIZE	2000	/* max size of macro body. */
#define DEFROMSIZE 8 * 1024	/* default size of target rom */
#define HASHSIZE 137	/* number of hash buckets */
#define NARGS	100	/* maximum number of args in a macro call */

#define ISDIGIT(ascii) (('0' <= (ascii)) && ((ascii) <= '9'))

struct macro {
  struct symbol *name;	/* symbol which is name of this macro */
  char *body;		/* body is simply a character string */
  int nparams;		/* number of parameters */
  char called;		/* flag to detect recursive macro calls */
  struct symbol *params[1]; /* array of formal parameters, NULL terminated */
};

struct symbol {
  struct symbol *next;	/* next symbol in hash bucket */
  char *name;		/* symbol's name */
  long value;		/* value */
  char type;		/* tells how symbol got its value */
  char ary;		/* N-ary macro, or -1 for regular symbol */
  struct macro *mdef;	/* if symbol is a macro name, points to defn */
} *hashtbl[HASHSIZE];	/* array of hash buckets */

#define SUNDEF  0	/* symbol has no value */
#define SASSIGN	1	/* symbol value established with "=" */
#define SLABEL  2	/* symbol value established with ":" */
#define	SMACRO	3	/* symbol is name of a macro */

struct symbol Dot = { NULL, ".", 0, SASSIGN, -1 };	/* the symbol "." */
int MaxDotValue = 8192;  /* no smaller than 8Kbytes: default for ROMS */

/* skip to next non-spacing character */
#define skipb(p) while (cinfo[*p] & SPC) p += 1

/* bits found in character info array cinfo[] */
#define SPC	0x01	/* spacing */
#define EOL 	0x02	/* end of line */
#define D	0x04	/* digit */
#define S	0x08	/* can start symbol */
#define T	0x10	/* can be part of symbol */

/* small info table for each input character */
char cinfo[128] = {
	EOL,	0,	0,	0,	0,	0,	0,	0,
	0,	SPC,	EOL,	SPC,	SPC,	SPC,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	SPC,	0,	0,	0,	S+T,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	S+T,	0,
	D+T,	D+T,	D+T,	D+T,	D+T,	D+T,	D+T,	D+T,
	D+T,	D+T,	0,	0,	0,	0,	0,	0,
	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	0,	0,	0,	0,	S+T,
	0,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,	S+T,
	S+T,	S+T,	S+T,	0,	EOL,	0,	0,	S+T
};

char *sassign(),*sdefer(),*smacro(),*sinclude(),*soperand(),*Expr(),*term();
char *salign(), *stext(), *sascii();

char iline[LSIZE];	/* current input line resides here */
unsigned Line_no;	/* current input line number */
char *File_name;	/* current input file name */
int  romfd;
int romsize;
char *rom;              /* where generated code is stored */
int Pass;			/* which pass we're on */
int errline;		/* set when there has been an error on this line */
int errfile;		/* set when there has been an error in the file */

/* Source code map stuff.
 */

struct SrcMap {
  unsigned int offset;	/* Byte offset of line in source file	*/
  unsigned int codeadr;	/* uROM adr of code for this line	*/
} *map = NULL;
int MaxLines;		/* Number of lines in source file.	*/
int IncLevel = 0;	/* .include level.			*/

/* produce simple error message, and set error flags */
#define ERROR(args) {                     \
                      if (Pass != 1) {    \
                        error_handler1(); \
			fprintf args;     \
			error_handler2(); \
		      }                   \
		    }

error_handler1()
{
  errline = 1;
  errfile = 1;
  fflush(stdout);
  fprintf(stderr,"%s:%d: ", File_name, Line_no);
}

error_handler2()
{
  fprintf(stderr,"\n%s\n", iline);
  fflush(stderr);
}

/* return 1 if string p is a prefix of string q */
int prefix(p,q)
  register char *p,*q;
  {	while (*p) if (*p++ != *q++) return(0);
	return(1);
}

extern long ftell();

/* Read a source line:
 */

FILE *infile;		/* input file				*/
char *inmacro = 0;	/* input macro body (takes precedence)	*/

char *rsource(iline)
  char *iline;
 {	char *rval;
	char flag = 'F';

	if (inmacro)
	 { register char *cc;
	   if (!*inmacro) return (inmacro = 0);
	   for (cc=iline; *inmacro && (*inmacro != '\n');) *cc++ = *inmacro++;
	   *cc++ = '\n'; if (*inmacro) inmacro++;
	   *cc=0;
	   rval = iline;
	   flag = 'M';
	   goto rtn;
	 }

	if (IncLevel == 0 && Pass == 2) /* Top-level source map ...	*/
	  {
	    map[Line_no].offset = ftell(infile);
	    map[Line_no].codeadr = Dot.value;
	  }
	if ((rval=fgets(iline,LSIZE,infile)) != iline) return rval;
	Line_no++;
	if (Pass == 1) MaxLines = Line_no;

rtn:	if (0 && Pass == 1)
		printf("%c %d:	|%s", flag, IncLevel, iline);
			
	return rval;
 }

scan()
  {	register int i;
	register char *p;	/* pointer into input line */
	char *token;		/* pointer to beginning of last token */


	for (;;)
	 {if (rsource(iline) != iline) break;
	  p = iline;
	  errline = 0;

	  /* see what we know about next char */
  again:  skipb(p);
	  i = cinfo[*p];
	  if (i & EOL) continue;	/* blank lines okay */

/* Following syntactic restriction seems unnatural, eliminated. -SAW.	*/

	  /* gotta start with a symbol */
/*
	  else if (!(i & S)) {
	    ERROR((stderr,
	          "unexpected character (%c) at beginning of statement", *p));
	    continue;
	  }
 */
	  /* what follows is either label or opcode, gobble it up */
	  token = p;
	  while (cinfo[*p] & T) p += 1;
	  skipb(p);

	  if (*p == '=') {	  /* "=" -> this is label assignment */
	    p = sassign(p += 1,token);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad assignment syntax"));
	    continue;
	  } else if (*p == ':') {  /* ":" -> this is label definition */
	    slabel(token);
	    p += 1;
	    goto again;
	  } else if (prefix(".macro",token)) {
	    p = smacro(p);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad macro syntax"));
	    continue;
	  } else if (prefix(".include",token)) {
	    p = sinclude(p);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad include syntax"));
	    continue;
	  } else if (prefix(".align", token)) {
	    p = salign(p);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad .align syntax"));
	  } else if (prefix(".text", token)) {
	    p = stext(p);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad .text syntax"));
	  } else if (prefix(".ascii", token)) {
	    p = sascii(p);
	    if (!(cinfo[*p] & EOL)) ERROR((stderr, "bad .ascii syntax"));
	  } else p = token;	  /* nothing special, back to beginning */

	  /* read operands until end of line is reached */
	  while (!(cinfo[*p] & EOL) && !errline) {
	    p = soperand(p);
	    skipb(p);
	    if (*p == ',') { p += 1; skipb(p); }
	  }
	}
} 

/* Hashing routine for symbol hash table */
hash(s)
  register char *s;
  {	register int i = 0;

	while (*s) i = i*10 + *s++ - ' ';
	i = i % HASHSIZE;
	return(i<0 ? i+HASHSIZE : i);
}

/* lookup symbol in symbol table
 * Added nparams 10/84 SAW:
 *  Allows n-ary and m-ary macros with same names (ie, separate
 *	name space for each number of arguments).
 *  nparams == -1 for regular symbol (thus making symbol names not
 *	conflict with macro names).
 */
struct symbol *lookup(sym,create,nparams)
  register char *sym;
  char nparams;
  {	register int hashcode;
	register struct symbol *s, *prev, *head;

	/* look through appropriate hash bucket */
	prev = s = hashtbl[hashcode = hash(sym)];
	while (s) {
	  if ((strcmp(s->name,sym) == 0) && (nparams == s->ary)) {
	    if (s != prev) {
	      head = hashtbl[hashcode];
	      hashtbl[hashcode] = s;
	      prev->next = s->next;
	      s->next = head;
	    }
	    return(s);
	  }
	  prev = s;
	  s = s->next;
	}

	if (create) {
	  s = (struct symbol *)malloc(sizeof(struct symbol));
	  s->next = hashtbl[hashcode];
	  hashtbl[hashcode] = s;
	  s->name = (char *)malloc(strlen(sym) + 1);
	  strcpy(s->name,sym);
	  s->type = SUNDEF;
	  s->mdef = NULL;
	  s->ary = nparams;
	  s->value = 0;
	}
	return(s);
}

/* handle definition of label */
slabel(token)
  register char *token;
  {	register char *p;
	register struct symbol *s;
	char lab[50];

	/* make asciz version of label */
	p = lab;
	while (cinfo[*token] & T) *p++ = *token++;
	*p = 0;

	/* find/enter symbol in the symbol table */
	s = lookup(lab, 1, -1);

	/* on pass 1 look for multiply defined symbols.  if ok, label
	 * value is Dot.
	 */
	if (Pass == 1) {
	  if (s->type != SUNDEF) ERROR((stderr,
					"multiply defined symbol (%s)", lab));
	  s->type = SLABEL;
	  s->value = Dot.value;
	} else
	   /* Modified to omit phase error reports following other
	    * errors -- SAW.
	    */
	   if ((s->value != Dot.value) && !errfile)
	   ERROR((stderr, "phase error in symbol definition (%s)", lab));

	/* fprintf(stderr,"slabel(%s) = %d\n",lab,Dot.value); */
}

/* handle assignment to a label, return updated line pointer */
char *sassign(lptr,token)
  register char *token;
  register char *lptr;
  {	register char *p;
	register struct symbol *s;
	unsigned long value;
	char lab[50];

	/* make asciz version of label */
	p = lab;
	while (cinfo[*token] & T) *p++ = *token++;
	*p = 0;

	/* find/enter symbol in the symbol table, and get its new value */
	s = lookup(lab,1,-1);
	lptr = Expr(lptr, &value);

	if (s->type == SLABEL)
	  ERROR((stderr, "illegal redefinition of symbol (%s)", lab));
	s->type = SASSIGN;
	s->value = value;

	/* fprintf(stderr,"sassign(%s) = %d\n",lab,s.value); */

	/* return updated line pointer */
	skipb(lptr);
	return(lptr);
}

/* read macro definition.
 */

char *smacro(lptr)
  register char *lptr;
  {	register int i,j;
	register char *p;
	register struct macro *m;
	register struct symbol *s;
	char lab[LSIZE], bod[MSIZE], bflg = 0;
	struct symbol *params[NARGS];

	/* make asciz version of field name (first arg to .macro) */
	skipb(lptr);
	p = lab;
	while (cinfo[*lptr] & T) *p++ = *lptr++;
	*p = 0;

	/* see if parenthesized parameter list follows.  If it does,
	 * each entry should be a symbol, a pointer to which is stored
	 * in the params array for later processing.
	 */
	skipb(lptr);
	i = 0;
	if (*lptr == '(') {
	  lptr += 1;
	  skipb(lptr);
	  if (*lptr == ')') goto done;
	  while (i < NARGS) {
	    skipb(lptr);
	    if (!(cinfo[*lptr] & S)) {	/* next thing has to be a symbol */
	      ERROR((stderr, "symbol expected in macro parameter list"));
	      goto done;
	    }
	    p = bod;			/* make a copy of the symbol name */
	    while (cinfo[*lptr] & T) *p++ = *lptr++;
	    *p = 0;
	    params[i++] = lookup(bod,1,-1);  /* create corresponding struct */
	    skipb(lptr);
	    if (*lptr == ')') goto done;  /* see if we're done */
	    else if (*lptr == ',') lptr += 1;
	    else {
	      ERROR((stderr, "comma or right parenthesis expected"));
	      goto done;
	    }
	  }
	  ERROR((stderr, "too many arguments in macro definition"));

  done:   lptr += 1;			/* skip RP */
	}

	skipb(lptr);		/* skip comma if there is one */
	if (*lptr == ',') { lptr += 1; skipb(lptr); }
	if (*lptr == '{') { lptr += 1; bflg++; skipb(lptr); }

	p = bod;		/* read body of macro */

	do
	 { while (!(cinfo[*lptr] & EOL))
		{ 
		  if ((p-bod) >= (MSIZE-2))
			{ error_handler1();
			  fprintf(stderr, "Macro %s too big!\n", lab);
			  exit(-1);
			}
		  *p = *lptr++;
		  if (*p != '}') p++;
	          else bflg--;
		}
	   *p++ = '\n';
	   if (bflg)
	    { if (rsource(iline) != iline)
			{ ERROR((stderr, "Unterminated macro '%s'", lab));
			  return NULL;
			}
		  lptr = iline;
		  errline = 0;
		}
	 }
	while (bflg);
	*p = 0;

	/* find/enter symbol in the symbol table. */
	s = lookup(lab,1, i);
	if (s->mdef != NULL) ERROR((stderr, "redefinition of macro %s",lab));

	/* allocate macro structure and fill it in */
	m = (struct macro *)malloc(sizeof(struct macro) +
				   (i - 1)*sizeof(struct symbol *));
	s->mdef = m;
	m->name = s;
	m->body = (char *)malloc(p - bod + 1);
	s->type = SMACRO;
	strcpy(m->body,bod);
	m->called = 0;
	m->nparams = i;
	for (j = 0; j < i; j += 1) m->params[j] = params[j];

	return(lptr);
}

/* .align 4
 */
char *salign(lptr)
 char *lptr;
 { long v;
   skipb(lptr);
   if (*lptr != '\n') lptr = Expr(lptr, &v);
   else v = 4;
   while (Dot.value % v) RomByte(0);
   return lptr;
 }

#if 0
char *RdText(lptr)
 char *lptr;
 { char ch;
   skipb(lptr);
   if (*lptr != '"') return lptr;
   ++lptr;
   while(1)
    { switch (ch = *lptr++)
       { case '\n':	return lptr;
	 case '\\':	switch (ch = *lptr++)
 			 { case 'r':	ch = '\r'; break;
 			   case 'n':	ch = '\n'; break;
 			   case 't':	ch = '\t'; break;
 			   case 'b':	ch = '\b'; break;
 			 }
 			/* Fall thru... */
	 default:	RomByte(ch); continue;
 	 case '"':	return lptr;
       }
    }
 }
#else
char *RdText(lptr)
     char *lptr;
{ 
  char ch;
  
  skipb(lptr);
  if (*lptr != '"') 
    return lptr;
  ++lptr;
  while(1)
    { 
      switch (ch = *lptr++)
	{ 
	case '\n':	return lptr;
	case '\\':	
	  switch (ch = *lptr++)
	    { 
	    case 'r':	ch = '\r'; break;
	    case 'n':	ch = '\n'; break;
	    case 't':	ch = '\t'; break;
	    case 'b':	ch = '\b'; break;
	      
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      /* we have a numeric constant, read it in, and dtrt */
	      {
		char ascii_value[3]; /* we will have a max of 3 values */
		int i;
		
		i = 0;
		lptr--;
		while (ISDIGIT (*lptr) && i < 3)
		  ascii_value[i++] = *lptr++ - '0';
		/* now convert our escape number to its numeric value */
		switch (i)
		  {
		  case 1:
		    ch = ascii_value[0];
		    break;
		  case 2:
		    ch = ascii_value[1] + (ascii_value[0] << 3);
		    break;
		  case 3:
		    ch = ascii_value[2] + (ascii_value[1] << 3) + (ascii_value[0] << 6);
		    break;
		  }
		break;
	      }
	    }
	  /* Fall thru... */
	default:	RomByte(ch); continue;
	case '"':	return lptr;
	}
    }
}
#endif

/* .text "text" - assemble text chars, terminate with 0, align.
 */
char *stext(lptr)
 char *lptr;
 {  char *p;
   skipb(lptr);
   lptr = RdText(lptr);
   RomByte(0);
   while (Dot.value % 4) RomByte(0);
   return lptr;
 }

/* .ascii "text" - assemble text chars.
 */
char *sascii(lptr)
 char *lptr;
 {  char *p;
   skipb(lptr);
   lptr = RdText(lptr);
   return lptr;
 }

/* read and process include directive */
char *sinclude(lptr)
  register char *lptr;
  {	register char *p;
	char lab[LSIZE], *oldinmacro = inmacro;
	FILE *oldin=infile, *in;

	/* make asciz version of file name (first arg to .include) */
	skipb(lptr);
	p = lab;
	while (*lptr > ' ') *p++ = *lptr++;
	*p = 0;

	if ((in = fopen(lab,"r")) == NULL)
	  ERROR((stderr, "cannot open %s for inclusion",lab))
	else {
	  int save_lineno = Line_no;
	  char *save_file = File_name;
	  char tline[LSIZE];

	  File_name = lab;
	  Line_no = 0;
	  strcpy(tline,iline);
	  IncLevel++;
	  infile = in;
	  inmacro = 0;
	  scan();
	  infile = oldin;
	  inmacro = oldinmacro;
	  fclose(in);
	  IncLevel--;
	  Line_no = save_lineno;
	  File_name = save_file;
	  strcpy(iline,tline);
	}

	skipb(lptr);
	return(lptr);
}

/* Gets value of next operand and loads it into rom */
char *soperand(lptr)
  register char *lptr;
  {	register int i;
	register char *p;
	register struct symbol *s;
	char sym[50],*lsave;
	unsigned long v,values[NARGS];

	skipb(lptr);
	lsave = lptr;

	/* if operand doesn't start with a symbol, we must be reading an
	 * ordinary expression.
	 */
	if (!(cinfo[*lptr] & S)) goto readexp;

	/* make asciz version of symbol and look it up in symbol table */
	p = sym;
	while (cinfo[*lptr] & T) *p++ = *lptr++;
	*p = 0;

	/* see if parenthesized argument(s) follow opcode symbol */
	skipb(lptr);
	if (*lptr == '(')
	 {
		i = 0;
		lptr += 1;
		skipb(lptr);
		if (*lptr == ')') goto done;
	 }
	else
	 {
	   if ((s = lookup(sym,0,-1)) == NULL)
		ERROR((stderr, "undefined operand symbol (%s)",sym));
	   goto readexp;
	 }

	/* must be a macro call, evaluate arguments and process macro body */
	while (i < NARGS) {
	  lptr = Expr(lptr,&values[i]);
	  i += 1;
	  skipb(lptr);
	  if (*lptr == ')') goto done;
	  else if (*lptr == ',') lptr += 1;
	  else {
	    ERROR((stderr, "comma or right parenthesis expected"));
	    goto done;
	  }
	}

	ERROR((stderr, "too many arguments in a macro call"));

  done:	lptr += 1;
	if ((s = lookup(sym,0,i)) == NULL)
	 {	ERROR((stderr, "undefined macro (%s)", sym));
		goto sdone;
	 }

	if (s!=NULL && s->mdef!=NULL) {	/* process macro call */
	  register struct macro *m = s->mdef;
	  long temp;
	  char types[NARGS];

	  if (m->called != 0) {
	    ERROR((stderr, "recursive call to macro %s", s->name));
	    goto sdone;
	  } else m->called = 1;

	  /* No longer necessary, since there are now different name spaces */
	  /* for different ary functions - mafetter 8/28/90 */
/*
	  if (i != m->nparams) {
	    ERROR((stderr,
		   "wrong number of arguments to macro %s (expected %d, got %d)",
		   s->name,m->nparams,i));
	    goto mdone;
	  }
*/

	  for(i = 0; i < m->nparams; i += 1) {
	    s = m->params[i];
	    temp = s->value;
	    s->value = values[i];
	    values[i] = temp;
	    types[i] = s->type;
	    s->type = SASSIGN;
	  }

	 /* Scan, recursively, the macro body:
	  */
	  { char *oldinmacro = inmacro;
	    char savebuf[LSIZE];
	    strcpy(savebuf, iline);
	    inmacro = m->body;
	    scan();
	    inmacro = oldinmacro;
	    strcpy(iline, savebuf);
	  }

	  for(i = 0; i < m->nparams; i += 1) {
	    s = m->params[i];
	    s->value = values[i];
	    s->type = types[i];
	  }

  mdone:  m->called = 0;
	} else ERROR((stderr, "operand symbol not name of a macro (%s)",sym));
	goto sdone;

	/* ordinary expression */
 readexp:
	lptr = Expr(lsave,&v);
	if (lptr == lsave)
	 {	ERROR((stderr, "Illegal operand: '%s'", lptr));
		lptr++;
	 }

	else if (!errline)
	  RomByte(v);

	/* return updated line pointer */
 sdone:	return(lptr);
}

void RomGrow() {
    romsize *= 2;
    ftruncate(romfd, romsize);
    rom = realloc(rom, romsize);
    if(rom == NULL) {
        ERROR((stderr, "Can't realloc"));
        Dot.value -= 1;
    }
}

/* Output a byte to ROM:
 */
RomByte(b)
 { rom[Dot.value] = b;
   if (Dot.value > MaxDotValue)
     MaxDotValue = Dot.value;
   if ((Dot.value += 1) >= romsize)
    {
        RomGrow();
    }
 }

/* read expression, sticking value where pointed to by second arg */
char *Expr(lptr,vptr)
  register char *lptr;
  register unsigned long *vptr;
  {	register int i;
	unsigned long v2;

	/* read in a term of an expression */
	lptr = term(lptr,vptr);

	/* parse expression until we reach some non-operator */
	while (1) {
	  skipb(lptr);
	  switch (*lptr) {

	    case '>':	if (lptr[1] == '>')
			 {  lptr = term(lptr+2, &v2); i = v2;
			    *vptr >>= i;
			    continue;
			 }
			else goto notop;

	    case '<':	if (lptr[1] == '<')
			 {  lptr = term(lptr+2, &v2); i = v2;
			    *vptr <<= i;
			    continue;
			 }
			else goto notop;

	    case '+':	lptr = term(lptr+1,&v2);
			*vptr += v2;
			continue;

	    case '-':	lptr = term(lptr+1,&v2);
			*vptr -= v2;
			continue;

	    case '*':	lptr = term(lptr+1,&v2);
			*vptr *= v2;
			continue;

	    case '/':	lptr = term(lptr+1,&v2);
	      		if (v2 == 0) {
			  ERROR((stderr, "division by 0"));
			  continue;
			}
			*vptr = *vptr / v2;
			continue;

	    case '%':	lptr = term(lptr+1, &v2);
			*vptr = *vptr % v2;
			continue;

notop:
	    default:	return(lptr);
	  }
	}
}

/* read term: either
 *	symbol
 *	constant
 *	unary minus
 *	complement
 *	parenthesized expression
 */
char *term(lptr,vptr)
  register char *lptr;
  register unsigned long *vptr;
  {	register int i;
	register struct symbol *s;
	register char *p;
	register int base;
	char token[50];

	*vptr = 0;		/* resonable default value */

	skipb(lptr);		/* see what's in store */
	i = cinfo[*lptr];

	if (i & D) {		/* a number */
	  if (*lptr == '0') {
	    if ((*(lptr+1) == 'b')||(*(lptr+1) == 'B')) { lptr += 2; base = 2; }
	    else if ((*(lptr+1) == 'x')||(*(lptr+1) == 'X')) { lptr += 2; base = 16; }
	    else { lptr += 1; base = 8; }
	  } else base = 10;

	  if (base <= 10) while (*lptr>='0' && *lptr<base+'0') {
	    *vptr = *vptr*base + *lptr - '0';
	    lptr += 1;
	  } else while (1) {
	    if (*lptr>='0' && *lptr<='9')
	      *vptr = *vptr*base + *lptr - '0';
	    else if (*lptr>='a' && *lptr<=base+'a'-10)
	      *vptr = *vptr*base + *lptr - 'a' + 10;
	    else if (*lptr>='A' && *lptr<=base+'A'-10)
	      *vptr = *vptr*base + *lptr - 'A' + 10;
	    else break;
	    lptr += 1;
	  }
	} else if (i & S) {		/* a symbol */
 	  p = token;
	  while (cinfo[*lptr] & T) *p++ = *lptr++;
	  *p = 0;	  
	  s = lookup(token,0,-1);
	  if (s == NULL) ERROR((stderr,
				"undefined symbol (%s) in expression", token))
	  else *vptr = s->value;
	} else if (*lptr == '\'') {		/* character constant */
	  if (*++lptr == '\\') switch (*lptr)
		{ case 'r':	*vptr = '\r'; break;
		  case 'n':	*vptr = '\n'; break;
		  case 'b':	*vptr = '\b'; break;
		  default:	*vptr = *lptr; break;
		}
	  else *vptr = *lptr;
	  lptr++;
	  if (*lptr++ != '\'') ERROR((stderr, "Bad character constant"));
	} else if (*lptr == '-') {		/* unary minus */
	  lptr = term(lptr+1,vptr);
	  *vptr = -*vptr;
	} else if (*lptr == '~') {		/* check for complement */
	  lptr = term(lptr+1,vptr);
	  *vptr = ~*vptr;
	} else if (*lptr == '(') {		/* parenthesized expression */
	  lptr = Expr(lptr+1,vptr);
	  skipb(lptr);
	  if (*lptr == ')') lptr += 1;
	  else ERROR((stderr, "unbalanced parenthesis in expression"));
	} else ERROR((stderr, "illegal term in expression"));

	return(lptr);
}

/* output binary number to specified file */
binout(ofile,n,digits)
 FILE *ofile;
 int n,digits;
 {	register unsigned i, j = 1<<(digits-1);

	for (i=0; i<digits; i++, n += n)
	 { putc((n&j) ? '1' : '0',ofile);
	 }
 }

/* remove any macro definitions between passes, and allocate map array */
cleanup()
  {	register int i;
	register struct symbol *s;
	register struct macro *m;

	for (i = 0; i < HASHSIZE; i += 1) 
	  for (s = hashtbl[i]; s != NULL; s = s->next)
	    if ((m = s->mdef) != NULL) {
	      free(m->body);
	      free(m);
	      s->mdef = NULL;
	    }
	if (Pass == 1) {
	  map = (struct SrcMap *)malloc((MaxLines + 1) * sizeof(*map));
	}
}

main(argc,argv)
  char *argv[];
  {	register int i;
	char filename[100];
        char romfile[100];
        char cwd[256];
        char *dir, *slash;
        char *dot;
	FILE *f;

#ifdef THINK_C
	argc = __ccommand(&argv);
#endif
	for (i = 1; i < argc; i += 1) {
	  if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
	    default:
	      fprintf(stderr, "Unrecognized switch: %s\n", argv[i]);
	    }
	  } else if (File_name == NULL) File_name = argv[i];
	  else fprintf(stderr,"More than one input file specified\n");
	}
	if (File_name == NULL) {
	  fprintf(stderr,"No input file specified!\n");
	  exit(1);
	}

        if((dot = strrchr(File_name, '.')) != NULL)
            *dot = 0;

	/* open source file for input */
	strcpy(filename,File_name);
	strcat(filename,".uasm");
	if ((f = fopen(filename,"r")) == NULL) {
	  fprintf(stderr,"Cannot open %s for input\n",filename);
	  exit(-1);
	}

        /* open ROM for output */
        strcpy(romfile, File_name);
        strcat(romfile, ".bin");
        romfd = open(romfile, O_RDWR|O_CREAT, 0644);
        if(romfd < 0) {
            fprintf(stderr,"Cannot open %s for output\n",romfile);
            exit(-1);
        }

        romsize = DEFROMSIZE;

        ftruncate(romfd, romsize);
        rom = malloc(romsize);
        if(rom == NULL) {
            fprintf(stderr,"Cannot allocate ROM image.\n");
            close(romfd);
            unlink(romfile);
            exit(-1);
        }

        /* Hack; chdir to the same directory while processing */
        getcwd(cwd, 256);
        dir = strdup(filename);
        if ((slash = strrchr(dir, '/')) != NULL) {
            *slash = 0;
            chdir(dir);
        }
        

	/* initialize hash table */
	for (i = 0; i < HASHSIZE; i += 1) hashtbl[i] = NULL;
	hashtbl[hash(Dot.name)] = &Dot;

	/* standard two pass assembly algorithm */
	for (Pass = 1; Pass <= 2; cleanup(), Pass += 1) {
	  Dot.value = 0;
	  rewind(f);
	  errfile = 0;
	  Line_no = 0;
	  infile = f;
	  scan();
	}
	fclose(f);

        /* Restore original WD for writing output */
        chdir(cwd);

	/* write rom contents */
	if (errfile) {
	  fprintf(stderr,"Input file had errors, aborting...\n");
          close(romfd);
          unlink(romfile);
	  exit(-1);
	} else {
            write(romfd, rom, romsize);
            ftruncate(romfd, MaxDotValue);
            close(romfd);
        }

	/* output symbol table */
	strcpy(filename,File_name);
	strcat(filename,".sym");
	if ((f = fopen(filename,"w")) == NULL)
	  fprintf(stderr,"Cannot open %s for output\n",filename);
	else {
/*
	  long t;
 */
	  register struct symbol *s;

/*	  time(&t);	
 *	  fprintf(f,"| This symbol table compiled from %s at %s\n",
 *			File_name,ctime(&t));
 */
	  for (i = 0; i < HASHSIZE; i++) {
	    for (s = hashtbl[i]; s != NULL; s = s->next) {
	      binout(f,(int) s->value, 16); putc(' ',f);
	      fprintf(f," %c %s\n","U=:M"[s->type],s->name);
	    }
	  }
	  fclose(f);
	}

	/* Output .map file:  */
	strcpy(filename,File_name);
	strcat(filename,".map");
	if ((f = fopen(filename,"wb")) == NULL)
	  fprintf(stderr,"Cannot open %s for output\n",filename);
	else {
	  int i;
	  for (i = 0; i < MaxLines + 1; i++) {
	    map[i].offset = htonl(map[i].offset);
	    map[i].codeadr = htonl(map[i].codeadr);
	  }	    
	  fwrite(map, sizeof *map, MaxLines+1, f);
	  fclose(f);
	}
	return 0;
}
