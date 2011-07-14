/*-
 * Copyright (c) 2011, Benedikt Meurer
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)qsort.c	8.1 (Berkeley) 6/4/93";
#endif /* LIBC_SCCS and not lint */
#include <sys/cdefs.h>
/*__FBSDID("$FreeBSD: src/lib/libc/stdlib/qsort.c,v 1.15 2008/01/14 09:21:34 das Exp $");*/

#include <sys/types.h>
#include <sys/sysctl.h>

#include <libkern/OSAtomic.h>

#include <dispatch/dispatch.h>

#include <stdlib.h>

typedef int		 cmp_t(const void *, const void *);
static inline char	*med3(char *, char *, char *, cmp_t *);
static inline void	 swapfunc(char *, char *, int, int);

#define min(a, b)	(a) < (b) ? a : b

/*
 * Qsort routine from Bentley & McIlroy's "Engineering a Sort Function".
 */
#define swapcode(TYPE, parmi, parmj, n) { 		\
	long i = (n) / sizeof (TYPE); 			\
	TYPE *pi = (TYPE *) (parmi); 		\
	TYPE *pj = (TYPE *) (parmj); 		\
	do { 						\
		TYPE	t = *pi;		\
		*pi++ = *pj;				\
		*pj++ = t;				\
        } while (--i > 0);				\
}

#define SWAPINIT(a, es) swaptype = ((char *)a - (char *)0) % sizeof(long) || \
	es % sizeof(long) ? 2 : es == sizeof(long)? 0 : 1;

static inline void
swapfunc(a, b, n, swaptype)
	char *a, *b;
	int n, swaptype;
{
	if(swaptype <= 1)
		swapcode(long, a, b, n)
	else
		swapcode(char, a, b, n)
}

#define swap(a, b)					\
	if (swaptype == 0) {				\
		long t = *(long *)(a);			\
		*(long *)(a) = *(long *)(b);		\
		*(long *)(b) = t;			\
	} else						\
		swapfunc(a, b, es, swaptype)

#define vecswap(a, b, n) 	if ((n) > 0) swapfunc(a, b, n, swaptype)

static inline char *
med3(char *a, char *b, char *c, cmp_t *cmp)
{
	return cmp(a, b) < 0 ?
	       (cmp(b, c) < 0 ? b : (cmp(a, c) < 0 ? c : a ))
              :(cmp(b, c) > 0 ? b : (cmp(a, c) < 0 ? a : c ));
}

typedef struct qsort_state
{
	dispatch_group_t group;
	dispatch_queue_t queue;
	cmp_t *cmp;
	size_t es;
	OSQueueHead cq;
} qsort_state_t;

typedef struct qsort_cont
{
	struct qsort_cont *next;
	qsort_state_t *state;
	void *a;
	size_t n;
} qsort_cont_t;

#define THRESHOLD 512

static void	qsort_invoke(void *context);
static void	qsort_internal(void *a, size_t n, qsort_state_t *state);

static void
qsort_invoke(void *context)
{
	qsort_cont_t *c;
	qsort_state_t *state;
	void *a;
	size_t n;

	/* Grab the data from the continuation and release it */
	c = (qsort_cont_t *)context;
	state = c->state;
	a = c->a;
	n = c->n;
	OSAtomicEnqueue(&state->cq, c, offsetof(qsort_cont_t, next));

	qsort_internal(a, n, state);
}

static void
qsort_internal(void *a, size_t n, qsort_state_t *state)
{
	char *pa, *pb, *pc, *pd, *pl, *pm, *pn;
	size_t d, r;
	int cmp_result;
	int swaptype, swap_cnt;
	qsort_cont_t *c;
	size_t es = state->es;
	cmp_t *cmp = state->cmp;

loop:	SWAPINIT(a, es);
	swap_cnt = 0;
	if (n < 7) {
		for (pm = (char *)a + es; pm < (char *)a + n * es; pm += es)
			for (pl = pm; 
			     pl > (char *)a && cmp(pl - es, pl) > 0;
			     pl -= es)
				swap(pl, pl - es);
		return;
	}
	pm = (char *)a + (n / 2) * es;
	pl = a;
	pn = (char *)a + (n - 1) * es;
	if (n > 40) {
		d = (n / 8) * es;
		pl = med3(pl, pl + d, pl + 2 * d, cmp);
		pm = med3(pm - d, pm, pm + d, cmp);
		pn = med3(pn - 2 * d, pn - d, pn, cmp);
	}
	pm = med3(pl, pm, pn, cmp);
	swap(a, pm);
	pa = pb = (char *)a + es;

	pc = pd = (char *)a + (n - 1) * es;
	for (;;) {
		while (pb <= pc && (cmp_result = cmp(pb, a)) <= 0) {
			if (cmp_result == 0) {
				swap_cnt = 1;
				swap(pa, pb);
				pa += es;
			}
			pb += es;
		}
		while (pb <= pc && (cmp_result = cmp(pc, a)) >= 0) {
			if (cmp_result == 0) {
				swap_cnt = 1;
				swap(pc, pd);
				pd -= es;
			}
			pc -= es;
		}
		if (pb > pc)
			break;
		swap(pb, pc);
		swap_cnt = 1;
		pb += es;
		pc -= es;
	}
	if (swap_cnt == 0) {  /* Switch to insertion sort */
		for (pm = (char *)a + es; pm < (char *)a + n * es; pm += es)
			for (pl = pm; 
			     pl > (char *)a && cmp(pl - es, pl) > 0;
			     pl -= es)
				swap(pl, pl - es);
		return;
	}

	pn = (char *)a + n * es;
	r = min(pa - (char *)a, pb - pa);
	vecswap(a, pb - r, r);
	r = min(pd - pc, pn - pd - es);
	vecswap(pb, pn - r, r);
	if ((r = (pb - pa) / es) > THRESHOLD
	 && (c = OSAtomicDequeue(&state->cq, offsetof(qsort_cont_t, next)))) {
		c->a = a;
		c->n = r;
		dispatch_group_async_f(state->group,
				       state->queue,
				       c, qsort_invoke);
	}
	else if (r > 1) {
		qsort_internal(a, r, state);
	}
	if ((r = pd - pc) > es) {
		/* Iterate rather than recurse to save stack space */
		a = pn - r;
		n = r / es;
		goto loop;
	}
}

void
qsort_freebsd_dispatch(void *a, size_t n, size_t es, cmp_t *cmp, dispatch_queue_t queue)
{
	qsort_state_t state = {
		.group = dispatch_group_create(),
		.queue = queue,
		.cmp = cmp,
		.es = es,
		.cq = OS_ATOMIC_QUEUE_INIT
	};

	uint32_t n_cont = 2; // TODO

#ifdef __APPLE__
	size_t szval = sizeof(n_cont);
	sysctlbyname("hw.activecpu", &n_cont, &szval, NULL, 0);
#endif

	uint32_t i;
	qsort_cont_t cont[n_cont];
	for (i = 0; i < n_cont; ++i) {
		cont[i].state = &state;
		OSAtomicEnqueue(&state.cq, &cont[i],
				offsetof(qsort_cont_t, next));
	}
	qsort_internal(a, n, &state);
	dispatch_group_wait(state.group, DISPATCH_TIME_FOREVER);
	dispatch_release(state.group);
}

