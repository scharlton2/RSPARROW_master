#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(deliv_fraction)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(mptnoder)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ptnoder)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(sites_incr)(void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(tnoder)(void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"deliv_fraction", (DL_FUNC) &F77_NAME(deliv_fraction), 7},
    {"mptnoder",       (DL_FUNC) &F77_NAME(mptnoder),       8},
    {"ptnoder",        (DL_FUNC) &F77_NAME(ptnoder),        7},
    {"sites_incr",     (DL_FUNC) &F77_NAME(sites_incr),     8},
    {"tnoder",         (DL_FUNC) &F77_NAME(tnoder),         7},
    {NULL, NULL, 0}
};

void R_init_RSPARROW(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
