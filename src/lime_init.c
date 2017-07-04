#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
   This fix is to avoid a Note in the Cran checking.
   https://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols
*/

/* .Call calls */
extern SEXP lime_get_index_permutations(SEXP, SEXP);
extern SEXP lime_rowSumsSq(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"lime_get_index_permutations", (DL_FUNC) &lime_get_index_permutations, 2},
    {"lime_rowSumsSq",              (DL_FUNC) &lime_rowSumsSq,              1},
    {NULL, NULL, 0}
};

void R_init_lime(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
