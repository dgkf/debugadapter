#include <R.h>
#include <Rinternals.h>

// 
// 

// Largely inspired by SrcrefPrompt in eval.c
//
// NOTE:
//   Currently defunct, body(), once in a browser, appears to reset the 
//   body() within the debug session so we can no longer grab the 
//   "srcfile" attribute used internally when debugging.
// 
//   Do I need to include my own RCTNXT typedef here to get access to 
//   cntxt.srcref?
//
SEXP srcpos(void) {
    SEXP srcref = R_Srcref;
    SEXP srcpos;

    PROTECT(srcpos = allocVector(INTSXP, 1));
    INTEGER(srcpos)[0] = Rf_asInteger(srcref);

    // if (srcref && srcref != R_NilValue) {
    //   	if (TYPEOF(srcref) == VECSXP) srcref = VECTOR_ELT(srcref, 0);
    //   	SEXP srcfile = getAttrib(srcref, install("srcfile"));
    //   	if (TYPEOF(srcfile) == ENVSXP) {
    //   	    // when called within a browser prompt, we don't enter this
    //   	    // branch, as srcfile is either a string or NULL
    //   	    SEXP filename = Rf_findVar(install("filename"), srcfile);
    //   	    if (isString(filename) && length(filename)) {
    //             INTEGER(srcpos)[0] = Rf_asInteger(srcref);
    //   	    }
    //   	}
    // }

    UNPROTECT(1);
    return srcpos;
}
