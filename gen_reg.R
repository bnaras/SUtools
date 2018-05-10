##
## Copyright (C) Balasubramanian Narasimhan
##
## This is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## The code is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this code.  If not, see <http://www.gnu.org/licenses/>.

## Generate registration code for glasso
fns <- c(
    "subroutine glasso(nn,sss,rrho,ia,is,itr,ipen,thr,maxit,www,wwwi,nniter,ddel,jerr)",
    "subroutine glassopath(beta,what,jerrs,rholist, nrho,n,ss,rho,ia,itr,ipen,thr,maxit,ww,wwi,niter,del,jerr)"
)

library(stringi)

implicit_type <- function(argNames) {
    ifelse(startsWith(argNames, "i") | startsWith(argNames, "j") |
           startsWith(argNames, "k") | startsWith(argNames, "l") |
           startsWith(argNames, "m") | startsWith(argNames, "n"),
           "int", "double")
}

implicit_sexp <- function(argNames) {
    sexps <- c(int = "INTSXP", double = "REALSXP")
    sexps[implicit_type(argNames)]
}

process_fn <- function(fn) {
    words <- stri_extract_all_words(fn)[[1]]
    fnName <- words[2]
    argNames <- words[-(1:2)]
    f77 <- gen_f77(fnName, argNames)
    types <- gen_types(fnName, argNames)
    list(fn = fnName,
         reg_lines = c(f77, "", "", types),
         f77_def = paste0("\tFDEF(", fnName, ")")
         )
}

gen_f77 <- function(fnName, argNames) {
    n <- length(argNames)
    if (n > 0) {
        f77_args <- paste0("\t", implicit_type(argNames), " *", argNames, ",")
        f77_args[n] <- gsub(",", "", f77_args[n]) ## Fix last arg
    } else {
        f77_args <- c()
    }
    f77_lines <- c(
        paste0("void F77_SUB(", fnName, ")("),
        f77_args,
        ");"
    )
    f77_lines
}

gen_types <- function(fnName, argNames) {
    n <- length(argNames)
    if (n > 0) {
        args <- paste0("\t", implicit_sexp(argNames), ",")
        args[n] <- gsub(",", "", args[n]) ## Fix last arg
    } else {
        args <- c()
    }
    lines <- c(
        paste0("static R_NativePrimitiveArgType ", fnName, "_t[] = {"),
        args,
        "};"
    )
    lines
}

prologue <- c(
    '// Automatically generated, editing not advised.',
    '#ifndef R_<pkg>_H',
    '#define R_<pkg>_H',
    '#include <R.h>',
    '#include <Rinternals.h>',
    '#include <R_ext/Rdynload.h>',
    '#ifdef ENABLE_NLS',
    '#include <libintl.h>',
    '#define _(String) dgettext ("<pkg>", String)',
    '#else',
    '#define _(String) (String)',
    '#endif',
    '',
    '#define FDEF(name)  {#name, (DL_FUNC) &F77_SUB(name), sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}'
)

epilogue <- c(
    "",
    "void R_init_<pkg>(DllInfo *dll){",
    "  R_registerRoutines(dll, NULL, NULL, fMethods, NULL);",
    "  R_useDynamicSymbols(dll, FALSE);",
    "}",
    "",
    "#endif"
)

gen_init_file <- function(fns, pkg, prologue, epilogue) {

    out_lines <- gsub("<pkg>", pkg, prologue)
    results <- lapply(fns, process_fn)
    for (result in results) {
        out_lines <- c(out_lines, result$reg_lines)
    }
    out_lines <- c(out_lines,
                   "static R_FortranMethodDef fMethods[] = {",
                   paste0(sapply(results, function(x) x$f77_def), ","),
                   "\t{NULL, NULL, 0}",
                   "};",
                   gsub("<pkg>", pkg, epilogue)
                   )
    out_file <- paste0(pkg, "_init.c")
    writeLines(text = out_lines, con = out_file)
}

gen_init_file(fns, "glasso", prologue, epilogue)
