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

#include <sys/types.h>

#ifdef __APPLE__
#include <mach/mach_time.h>
#endif

#include <arpa/inet.h>

#include <dispatch/dispatch.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int	cmp_t(const void *, const void *);
typedef void	sort_t(void *, size_t, size_t, cmp_t *);
extern void	qsort_freebsd(void *a, size_t n, size_t es, cmp_t *cmp);
extern void	qsort_naive(void *a, size_t n, size_t es, cmp_t *cmp);
extern void	qsort_naive_dispatch(void *a, size_t n, size_t es, cmp_t *cmp, dispatch_queue_t queue);

static void
random_longs(void *a, size_t n)
{
	long *p;

	for (p = (long *)a; n > 0; --n)
		*p++ = random();
}

static uint64_t
tstamp(void)
{
#ifdef __APPLE__
	return mach_absolute_time();
#endif
}

static uint64_t
tstamp_delta_us(uint64_t a, uint64_t b)
{
#ifdef __APPLE__
	mach_timebase_info_data_t info;
	mach_timebase_info(&info);
	uint64_t delta = (a > b) ? a - b : b - a;
	delta = (delta * info.numer) / info.denom;
	return delta / 1000ull;
#endif
}

static void
assert_sorted(void *a, size_t n, size_t es, cmp_t *cmp)
{
	char *pa, *pn;
	int cmp_result;

	pa = (char *)a + es;
	pn = (char *)a + n * es;
	while (pa < pn) {
		cmp_result = cmp(pa - es, pa);
		assert(cmp_result <= 0);
		pa += es;
	}
}

static uint64_t
test_sort(void *a, size_t n, size_t es, cmp_t *cmp, sort_t *sort)
{
	uint64_t t = tstamp();
	sort(a, n, es, cmp);
	t = tstamp_delta_us(t, tstamp());
	assert_sorted(a, n, es, cmp);
	return t;
}

static void
qsort_naive_high(void *a, size_t n, size_t es, cmp_t *cmp)
{
	qsort_naive_dispatch(a, n, es, cmp, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_HIGH, 0));
}

static void
test_sorts(const char *name, void *a, size_t n, size_t es, cmp_t *cmp)
{
	void *b;
	char *s;

	b = calloc(n, es);

	memcpy(b, a, n * es);
	uint64_t t_freebsd = test_sort(b, n, es, cmp, qsort_freebsd);

	memcpy(b, a, n * es);
	uint64_t t_naive = test_sort(b, n, es, cmp, qsort_naive);

	memcpy(b, a, n * es);
	uint64_t t_naive_high = test_sort(b, n, es, cmp, qsort_naive_high);

	free(b);

	asprintf(&s, "%s/%ld", name, (long)n);
	printf("%-24s %6ldms %6ldms %6ldms\n", s,
			(long)(t_freebsd / 1000),
			(long)(t_naive / 1000),
			(long)(t_naive_high / 1000));
	free(s);
}

static int
icmp(const void *a, const void *b)
{
	return *(const int *)a - *(const int *)b;
}

static void
test_integer(size_t n)
{
	int *a;
	size_t i;

	a = (int *)calloc(n, sizeof(int));

	/* Sorted integer array */
	for (i = 0; i < n; ++i)
		a[i] = (int)i;
	test_sorts("i-sorted", a, n, sizeof(int), icmp);

	/* Reversed sorted integer array */
	for (i = 0; i < n; ++i)
		a[i] = (int)(n - i);
	test_sorts("i-sorted-rev", a, n, sizeof(int), icmp);

	/* Random integer array (TODO) */
	for (i = 0; i < n; ++i)
		a[i] = random();
	test_sorts("i-random", a, n, sizeof(int), icmp);

	free(a);
}

static int
scmp(const void *a, const void *b)
{
	return strcmp(*(const char **)a, *(const char **)b);
}

static void
test_string(size_t n)
{
	char **a;
	size_t i;

	a = (char **)calloc(n, sizeof(char *));

	/* Sorted string data */
	for (i = 0; i < n; ++i)
		asprintf(&a[i], "%08x", (unsigned)i);
	test_sorts("s-sorted", a, n, sizeof(char *), scmp);
	for (i = 0; i < n; ++i)
		free(a[i]);

	/* Reversed sorted chunk data */
	for (i = 0; i < n; ++i)
		asprintf(&a[i], "%08x", (unsigned)(n - i));
	test_sorts("s-sorted-rev", a, n, sizeof(char *), scmp);
	for (i = 0; i < n; ++i)
		free(a[i]);

	/* Random string data (TODO) */
	for (i = 0; i < n; ++i)
		asprintf(&a[i], "%08lx", random());
	test_sorts("s-sorted-random", a, n, sizeof(char *), scmp);
	for (i = 0; i < n; ++i)
		free(a[i]);

	free(a);
}

typedef union
{
	char c[4096];
} chunk4k_t;

static int
ccmp(const void *a, const void *b)
{
	return memcmp((const chunk4k_t *)a, (const chunk4k_t *)b, sizeof(chunk4k_t));
}

static void
test_4k(size_t n)
{
	chunk4k_t *a;
	size_t i;

	a = (chunk4k_t *)calloc(n, sizeof(chunk4k_t));

	/* Sorted chunk data */
	for (i = 0; i < n; ++i)
		*(uint32_t *)(a + i) = htonl((uint32_t)i);
	test_sorts("4k-sorted", a, n, sizeof(chunk4k_t), ccmp);

	/* Reversed sorted chunk data */
	for (i = 0; i < n; ++i)
		*(uint32_t *)(a + i) = htonl((uint32_t)(n - i));
	test_sorts("4k-sorted-rev", a, n, sizeof(chunk4k_t), ccmp);

	/* Random chunk data (TODO) */
	random_longs(a, (n * sizeof(chunk4k_t)) / sizeof(long));
	test_sorts("4k-random", a, n, sizeof(chunk4k_t), ccmp);

	free(a);
}

int
main(int argc, char *argv[])
{
	srandomdev();

	printf("%-24s %8s %8s %8s\n\n", "", "freebsd", "naive", "naive/h");

	test_integer(1000 * 1000);
	test_string(1000 * 1000);
	test_4k(100 * 1000);

	return EXIT_SUCCESS;
}
