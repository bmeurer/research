#include <sys/resource.h>
#include <sys/wait.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>


#define OCAMLPREFIX "/tmp/ocaml"
#define OCAMLJIT2PREFIX "/tmp/ocamljit2"


static FILE *latex;


double profile(const char *prog, char **argv)
{
  pid_t child, pid;
  int status;
  int fd;
  long best = LONG_MAX;
  long time;
  int j;
  struct rusage rusage;

  fd = open("/dev/null", O_RDWR);
  if (fd < 0)
    err(EXIT_FAILURE, "Failed to open /dev/null");

  for (j = 0; j < 5; ++j) {
    child = vfork();
    if (child == 0) {
      dup2(fd, 0);
      dup2(fd, 1);
      dup2(fd, 2);
      close(fd);
      execv(prog, argv);
      _exit(127);
    }
    else if (child < 0) {
      err(EXIT_FAILURE, "Failed to fork");
    }
    else {
      for (;;) {
        pid = wait4(child, &status, 0, &rusage);
        if (pid < 0) {
          err(EXIT_FAILURE, "wait4(%ld) failed", (long) child);
        }
        else if (pid == child) {
          if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
            break;
          fprintf(stderr, "Test failed (%s)\n", prog);
          exit(EXIT_FAILURE);
        }
      }

      time = rusage.ru_utime.tv_sec * 1000
           + rusage.ru_utime.tv_usec / 1000
           + rusage.ru_stime.tv_sec * 1000
           + rusage.ru_stime.tv_usec / 1000;
      if (time < best)
        best = time;
    }
  }

  close(fd);
  return best / 1000.;
}


void compile(int opt, int unsafe, char *file)
{
  char *argv[4];
  pid_t child, pid;
  int status;
  const char *prog;

  argv[0] = opt ? "ocamlopt" : "ocamlc";
  if (unsafe) {
    argv[1] = "-unsafe";
    argv[2] = file;
    argv[3] = NULL;
  }
  else {
    argv[1] = file;
    argv[2] = NULL;
  }
  prog = opt ? OCAMLPREFIX "/bin/ocamlopt" : OCAMLPREFIX "/bin/ocamlc";

  child = fork();
  if (child == 0) {
    execv(prog, argv);
    _exit(127);
  }
  else if (child < 0) {
    err(EXIT_FAILURE, "Failed to fork");
  }
  else {
    for (;;) {
      pid = wait4(child, &status, 0, NULL);
      if (pid < 0) {
        err(EXIT_FAILURE, "wait4(%ld) failed", (long) child);
      }
      else if (pid == child) {
        if (WIFEXITED(status) && WEXITSTATUS(status) == 0)
          break;
        fprintf(stderr, "Compilation failed\n");
        exit(EXIT_FAILURE);
      }
    }
  }
}


void result(const char *title, const char *comment, double byt, double jit, double opt)
{
  double sigma_jit_byt, sigma_opt_jit, sigma_opt_byt;

  sigma_jit_byt = byt / jit;
  sigma_opt_jit = jit / opt;
  sigma_opt_byt = byt / opt;

  printf("%-24s %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n",
         title, byt, jit, opt,
         sigma_jit_byt, sigma_opt_jit, sigma_opt_byt);

  fprintf(latex, "\\texttt{%s} & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ & %s \\\\\n",
         title, byt, jit, opt,
         sigma_jit_byt, sigma_opt_jit, sigma_opt_byt,
         comment);
}


void simple(const char *name, int unsafe, const char *comment)
{
  char *title = NULL;
  char *file = NULL;
  char *argv[3];
  double byt, jit, opt;

  argv[0] = "a.out";
  argv[1] = "a.out";
  argv[2] = NULL;

  if (unsafe)
    asprintf(&title, "%s.unsafe", name);
  else
    title = (char *) name;
  asprintf(&file, "%s.ml", name);

  compile(0, unsafe, file);
  byt = profile(OCAMLPREFIX "/bin/ocamlrun", argv);
  jit = profile(OCAMLJIT2PREFIX "/bin/ocamlrun", argv);
  compile(1, unsafe, file);
  argv[1] = NULL;
  opt = profile("./a.out", argv);

  result(title, comment, byt, jit, opt);

  if (unsafe)
    free(title);
  free(file);
}


void cleanup()
{
  system("rm -f *.cmi *.cmo *.cmx *.o a.out");
}


void compiler(char *compiler, const char *title, const char *comment, char *opt1, ...)
{
  char *argv[128];
  int i;
  va_list ap;
  double byt, jit, opt;
  char *path = NULL;

  argv[0] = compiler;
  argv[1] = opt1;
  va_start(ap, opt1);
  for (i = 2;; ++i) {
    argv[i] = va_arg(ap, char *);
    if (argv[i] == NULL)
      break;
  }
  va_end(ap);

  asprintf(&path, "%s/bin/%s", OCAMLPREFIX, compiler);
  byt = profile(path, argv);
  free(path);
  cleanup();
  asprintf(&path, "%s/bin/%s", OCAMLJIT2PREFIX, compiler);
  jit = profile(path, argv);
  free(path);
  cleanup();
  asprintf(&path, "%s/bin/%s.opt", OCAMLPREFIX, compiler);
  opt = profile(path, argv);
  free(path);
  cleanup();

  result(title, comment, byt, jit, opt);
}


int main(int argc, char **argv)
{
  latex = fopen("results.tex", "w");
  if (latex == NULL)
    err(EXIT_FAILURE, "Failed to open results.tex for writing");

  printf("%-24s %8s %8s %8s %8s %8s %8s\n",
         "command", "byt", "jit", "opt",
         "byt/jit", "jit/opt", "byt/opt");
  printf("------------------------------------------------------------------------------\n");
  simple("almabench", 0, "number crunching");
  simple("almabench", 1, "number crunching {\\tiny(no bounds check)}");
  simple("bdd", 0, "binary decision diagram");
  simple("boyer", 0, "term processing");
  simple("fft", 0, "fast fourier transformation");
  simple("nucleic", 0, "floating point");
  simple("quicksort", 0, "array quicksort");
  simple("quicksort", 1, "array quicksort {\\tiny(no bounds check)}");
  simple("soli", 0, "tiny solitaire");
  simple("soli", 1, "tiny solitaire {\\tiny(no bounds check)}");
  simple("sorts", 0, "various sorting algorithms");
  cleanup();
  return EXIT_SUCCESS;
}
