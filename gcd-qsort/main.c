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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int	cmp_t(const void *, const void *);
typedef void	sort_t(void *, size_t, size_t, cmp_t *);
extern void	qsort_naive(void *a, size_t n, size_t es, cmp_t *cmp);
static void	assert_sorted(void *a, size_t n, size_t es, cmp_t *cmp);

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

static void
test_sort(void *a, size_t n, size_t es, cmp_t *cmp, sort_t *sort)
{
	sort(a, n, es, cmp);
	assert_sorted(a, n, es, cmp);
}

static void
test_sorts(const char *name, void *a, size_t n, size_t es, cmp_t *cmp)
{
	void *b;
	char *s;

	b = calloc(n, es);
	memcpy(b, a, n * es);
	test_sort(b, n, es, cmp, qsort_naive);
	free(b);

	asprintf(&s, "%s/%ld", name, (long)n);
	printf("%24s\n", s);
	free(s);
}

static int
intcmp(const void *a, const void *b)
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
	test_sorts("i-sorted", a, n, sizeof(int), intcmp);

	/* Reversed sorted integer array */
	for (i = 0; i < n; ++i)
		a[i] = (int)(n - i);
	test_sorts("i-sorted-rev", a, n, sizeof(int), intcmp);

	/* Random integer array (TODO) */
	for (i = 0; i < n; ++i)
		a[i] = random();
	test_sorts("i-random", a, n, sizeof(int), intcmp);

	free(a);
}

int
main(int argc, char *argv[])
{
	srandomdev();

	test_integer(1000);
	test_integer(1000 * 1000);

	return EXIT_SUCCESS;
}
