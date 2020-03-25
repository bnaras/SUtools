// Automatically generated, editing not advised.
#ifndef R_SUTOOLS_H
#define R_SUTOOLS_H
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("SUtools", String)
#else
#define _(String) (String)
#endif

#define FDEF(name)  {#name, (DL_FUNC) &F77_SUB(name), sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}

extern SEXP storePB(SEXP tpb, SEXP env);
static const R_CallMethodDef CallEntries[] = {
					      {"storePB", (DL_FUNC) &storePB, 2},
					      {NULL, NULL, 0}
};

void F77_SUB(testpb)(void);
static R_NativePrimitiveArgType testpb_t[] = {
};

void F77_SUB(setpb)(
		    int *val
		    );
static R_NativePrimitiveArgType setpb_t[] = {
  INTSXP
};


static R_FortranMethodDef fMethods[] = {
					FDEF(testpb),
					FDEF(setpb),
					{NULL, NULL, 0}
};

void R_init_SUtools(DllInfo *dll){
  R_registerRoutines(dll, NULL, CallEntries, fMethods, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
#endif
