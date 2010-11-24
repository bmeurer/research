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


#define RUNS 5

#define OCAMLPREFIX "/tmp/ocaml"
#define OCAMLJITPREFIX "/tmp/ocamljit"
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

  for (j = 0; j < RUNS; ++j) {
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


void compile(const char *prefix, int opt, int unsafe, char *file)
{
  char *argv[4];
  pid_t child, pid;
  int status;
  char *prog;

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
  asprintf(&prog, "%s/bin/%s", prefix, opt ? "ocamlopt" : "ocamlc");

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
  free(prog);
}


void result(const char *title, double byt, double jit, double jit2, double opt)
{
  if (jit == 0.0) {
    double byt_jit2 = byt / jit2;
    double byt_opt = byt / opt;
    double jit2_opt = jit2 / opt;

    printf("%-16s %8.2f %8s %8.2f %8.2f %8s %8.2f %8.2f %8s %8s %8.2f\n",
           title,
           byt, "", jit2, opt,
           "", byt_jit2, byt_opt, "", "", jit2_opt);

    fprintf(latex,
            "\\texttt{%s} & $%.2f$ & %s & $%.2f$ & $%.2f$ &"
            " %s & $%.2f$ & $%.2f$ & %s & %s & $%.2f$\\\\\n",
           title,
           byt, "", jit2, opt,
           "", byt_jit2, byt_opt, "", "", jit2_opt);

  }
  else {
    double byt_jit = byt / jit;
    double byt_jit2 = byt / jit2;
    double byt_opt = byt / opt;
    double jit_jit2 = jit / jit2;
    double jit_opt = jit / opt;
    double jit2_opt = jit2 / opt;

    printf("%-16s %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f %8.2f\n",
           title,
           byt, jit, jit2, opt,
           byt_jit, byt_jit2, byt_opt,
           jit_jit2, jit_opt, jit2_opt);

    fprintf(latex,
            "\\texttt{%s} & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ &"
            " $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$ & $%.2f$\\\\\n",
           title,
           byt, jit, jit2, opt,
           byt_jit, byt_jit2, byt_opt,
           jit_jit2, jit_opt, jit2_opt);
  }
}


void simple(const char *name, int unsafe, const char *comment)
{
  char *title = NULL;
  char *file = NULL;
  char *argv[3];
  double byt, jit, jit2, opt;

  argv[0] = "a.out";
  argv[1] = "a.out";
  argv[2] = NULL;

  if (unsafe)
    asprintf(&title, "%s.unsafe", name);
  else
    title = (char *) name;
  asprintf(&file, "%s.ml", name);

  if (access(OCAMLJITPREFIX "/bin/ocamljitrun", X_OK) == 0) {
    compile(OCAMLJITPREFIX, 0, unsafe, file);
    jit = profile(OCAMLJITPREFIX "/bin/ocamljitrun", argv);
  }
  else {
    jit = 0.0;
  }
  compile(OCAMLPREFIX, 0, unsafe, file);
  byt = profile(OCAMLPREFIX "/bin/ocamlrun", argv);
  jit2 = profile(OCAMLJIT2PREFIX "/bin/ocamlrun", argv);
  compile(OCAMLPREFIX, 1, unsafe, file);
  argv[1] = NULL;
  opt = profile("./a.out", argv);

  result(title, byt, jit, jit2, opt);

  if (unsafe)
    free(title);
  free(file);
}


void cleanup()
{
  system("rm -f *.cmi *.cmo *.cmx *.o a.out");
}


int main(int argc, char **argv)
{
  latex = fopen("results.tex", "w");
  if (latex == NULL)
    err(EXIT_FAILURE, "Failed to open results.tex for writing");

  printf("%-16s %8s %8s %8s %8s %8s %8s %8s %8s %8s %8s\n",
         "command", "byt", "jit", "jit2", "opt",
         "byt/jit", "byt/jit2", "byt/opt",
         "jit/jit2", "jit/opt", "jit2/opt");
  printf("----------------------------------------------------------------------------------------------------------\n");
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
