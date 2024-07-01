#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP srcpos(void);

static const R_CallMethodDef CallEntries[] = {
  {"srcpos",(DL_FUNC) &srcpos, 0},
  {NULL, NULL, 0}
};

void R_init_addr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
