#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _lime_get_index_permutations(SEXP, SEXP);
extern SEXP _lime_rowSumsSq(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_lime_get_index_permutations", (DL_FUNC) &_lime_get_index_permutations, 2},
  {"_lime_rowSumsSq",              (DL_FUNC) &_lime_rowSumsSq,              1},
  {NULL, NULL, 0}
};

void R_init_lime(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
