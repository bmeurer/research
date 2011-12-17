/*-
 * Copyright (c) 2010-2011, Benedikt Meurer <benedikt.meurer@googlemail.com>
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
#include <sys/mman.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


/* --- types --- */


struct busage {
  int64_t bu_mintime; /* minimum running time in microseconds */
  int64_t bu_maxtime; /* maximum running time in microseconds */
  int64_t bu_minsize; /* minimum resident set size utilized in bytes */
  int64_t bu_maxsize; /* maximum resident set size utilized in bytes */
};


/* --- global variables --- */


static int         opt_runs = 2; /* number of runs */
static const char *progname;


/* --- helpers --- */


static void sigdummy(int signo)
{
  (void)signo;
}


static void xerr(int code, const char *format, ...)
{
  const char *errmsg;
  va_list ap;

  errmsg = strerror(errno);
  va_start(ap, format);
  fprintf(stderr, "%s: ", progname);
  vfprintf(stderr, format, ap);
  fprintf(stderr, ": %s\n", errmsg);
  fflush(stderr);
  va_end(ap);
  exit(code);
}


static void _xerr(int code, const char *format, ...)
{
  const char *errmsg;
  va_list ap;

  errmsg = strerror(errno);
  va_start(ap, format);
  fprintf(stderr, "%s: ", progname);
  vfprintf(stderr, format, ap);
  fprintf(stderr, ": %s\n", errmsg);
  fflush(stderr);
  va_end(ap);
  _exit(code);
}


static void xerrx(int code, const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  fprintf(stderr, "%s: ", progname);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  fflush(stderr);
  va_end(ap);
  exit(code);
}


/* --- functions --- */


static void benchmark(const char *prog, char **argv, struct busage *busage)
{
  struct rusage *rusage_self;
  struct rusage *rusage_children;
  int64_t        size;
  int64_t        time;
  size_t         shared_size;
  pid_t          child;
  pid_t          pid;
  char          *shared;
  int            status;
  int            fd;
  int            j;

  /* allocate shared memory to communicate the rusage stats from the child processes */
  shared_size = (((2 * sizeof(struct rusage)) + (getpagesize() - 1)) / getpagesize()) * getpagesize();
  shared = (char *)mmap(NULL, shared_size, PROT_READ | PROT_WRITE, MAP_ANON | MAP_SHARED, -1, 0);
  if (shared == (char *)MAP_FAILED)
    xerr(EXIT_FAILURE, "mmap(%ld) failed", (long)shared_size);
  rusage_self = (struct rusage *)shared;
  rusage_children = rusage_self + 1;

  busage->bu_mintime = INT64_MAX;
  busage->bu_maxtime = INT64_MIN;
  busage->bu_minsize = INT64_MAX;
  busage->bu_maxsize = INT64_MIN;

  fd = open("/dev/null", O_RDWR);
  if (fd < 0)
    xerr(EXIT_FAILURE, "open(\"/dev/null\") failed");

  for (j = 0; j < opt_runs; ++j) {
    /* flush our buffers prior to fork()ing */
    fflush(NULL);

    /* we need an indirection of two fork()'s here  due to the way that RUSAGE_CHILDREN works */
    child = fork();
    if (child < 0)
      xerr(EXIT_FAILURE, "fork() failed");
    else if (child == 0) {
      /* first child process */
      child = fork();
      if (child < 0)
        _xerr(EXIT_FAILURE, "fork() failed");
      else if (child == 0) {
        /* second child process */
        dup2(fd, STDIN_FILENO);
        dup2(fd, STDOUT_FILENO);
        dup2(fd, STDERR_FILENO);
        close(fd);
        execv(prog, argv);
        _exit((errno == ENOENT) ? 127 : 126);
      }
      /* first child process */
      for (;;) {
        pid = wait4(child, &status, 0, rusage_self);
        if (pid < 0)
          _xerr(EXIT_FAILURE, "wait4(%ld) failed", (long)child);
        else if (pid == child) {
          if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
            break;
          fprintf(stderr, "\"%s", prog);
          for (j = 1; argv[j] != NULL; ++j)
            fprintf(stderr, " %s", argv[j]);
          fprintf(stderr, "\" ");
          if (WIFSIGNALED(status)) {
#ifdef WCOREDUMP
            if (WCOREDUMP(status))
              fprintf(stderr, "dumped core\n");
            else
#endif
              fprintf(stderr, "terminated due to receipt of signal %d (%s)\n",
                      WTERMSIG(status), strsignal(WTERMSIG(status)));
          }
          else {
            fprintf(stderr, "exited with code %d\n", WEXITSTATUS(status));
            _exit(WEXITSTATUS(status));
          }
          _exit(EXIT_FAILURE);
        }
      }
      /* rusage of all (terminated) children */
      if (getrusage(RUSAGE_CHILDREN, rusage_children) < 0)
        _xerr(EXIT_FAILURE, "getrusage(RUSAGE_CHILDREN) failed");
      _exit(EXIT_SUCCESS);
    }

    /* parent process */
    for (;;) {
      pid = waitpid(child, &status, 0);
      if (pid < 0)
        xerr(EXIT_FAILURE, "wait4(%ld) failed", (long)child);
      else if (pid == child) {
        if (WIFEXITED(status)) {
          if (WEXITSTATUS(status) == 0)
            break;
          exit(WEXITSTATUS(status));
        }
        else {
          xerrx(EXIT_FAILURE, "child process terminated due to receipt of signal %d (%s)",
                WTERMSIG(status), strsignal(WTERMSIG(status)));
        }
      }
    }

    /* combined system + user time in microseconds */
    time = (uint64_t)rusage_children->ru_stime.tv_usec
         + (uint64_t)rusage_children->ru_stime.tv_sec * 1000u * 1000u
         + (uint64_t)rusage_children->ru_utime.tv_usec
         + (uint64_t)rusage_children->ru_utime.tv_sec * 1000u * 1000u;
    if (time < busage->bu_mintime)
      busage->bu_mintime = time;
    if (time > busage->bu_maxtime)
      busage->bu_maxtime = time;

    /* resident set size in bytes */
    size = (int64_t)rusage_self->ru_maxrss * 1024u;
    if (size < busage->bu_minsize)
      busage->bu_minsize = size;
    if (size > busage->bu_maxsize)
      busage->bu_maxsize = size;
  }

  /* cleanup */
  munmap(shared, shared_size);
  close(fd);
}


static void usage(int exitcode)
{
  fprintf(stderr, "Usage: %s [-r RUNS] PATH [ARGS...]\n", progname);
  exit(exitcode);
}


/* --- entry --- */


int main(int argc, char *argv[])
{
  struct busage busage;
  char         *path;
  char         *s;
  int           ch;

  /* figure out the progname (basename of argv[0]) */
  for (path = s = argv[0]; *s != '\0'; ++s)
    if (*s == '/')
      path = s + 1;
  progname = (*path != '\0') ? path : argv[0];
  
  /* ignore SIGCHLD and SIGPIPE */
  signal(SIGCHLD, sigdummy);
  signal(SIGPIPE, sigdummy);

  /* parse command line parameters */
  while ((ch = getopt(argc, argv, "hr:")) != -1) {
    switch (ch) {
    case 'h':
      usage(EXIT_SUCCESS);

    case 'r':
      opt_runs = strtol(optarg, NULL, 10);
      break;

    default:
      usage(EXIT_FAILURE);
    }
  }

  argc -= optind;
  argv += optind;
  if (argc == 0)
    usage(EXIT_FAILURE);

  /* benchmark the program */
  benchmark(argv[0], argv, &busage);

  printf("%10.3f\n", (busage.bu_mintime / 1000.) / 1000.);

  return EXIT_SUCCESS;
}

