## Global variables
PADDING <- c(FORTRAN = "      ",  ## 6 spaces for fortran
             MORTRAN = "")        ## None

##
## Get Line starts and line end boundaries of functions or subroutines
##
get_boundaries <- function(lines, what = c("subroutine", "function"),
                           type = c("FORTRAN", "MORTRAN")) {
    what <- match.arg(what)
    type <- match.arg(type)
    padding <- PADDING[type]
    line_start <- grep(paste0("^", padding, what),  lines)
    line_end <- line_start
    if (type == "MORTRAN") {
        ## Since statements can span multiple lines
        ## figure out which is the last line of the
        ##  definition. I.e. look for semi-colon for mortran
        for (i in seq_along(line_end)) {
            x <- line_end[i];
            while(!grepl(";", stringr::str_trim(lines[x])))  {
                x <- x + 1
            }
            line_end[i] <- x
        }
    } else {
        for (i in seq_along(line_end)) {
            x <- line_end[i];
            all_open_paren <- nrow(stringr::str_locate_all(lines[x], "(")[[1]])
            all_close_paren <- nrow(stringr::str_locate_all(lines[x], ")")[[1]])
            diff <- all_open_paren - all_close_paren
            while(diff != 0) {  ## assuming first open paren not continued!!
                x <- x + 1
                all_open_paren <- nrow(stringr::str_locate_all(lines[x], "(")[[1]])
                all_close_paren <- nrow(stringr::str_locate_all(lines[x], ")")[[1]])
                diff <- diff + (all_open_paren - all_close_paren)
            }
            line_end[i] <- x
        }
    }
    list(start = line_start, end = line_end)
}


##
## Jerry tries to be efficient by trying to allocate what he needs in
## one block and returning with error on any failure. But the compiler
## isn't smart enought to detect that, so we fix those.
## Fortunately, Jerry follows a pattern we can rely on.
## However, we assume it is all on one line
##
fix_jhf_allocate <- function(lines) {
    indices <- grep("^allocate", lines)
    lines[indices] <- sub("ierr", "jerr", lines[indices])
    ## look for semi-colon
    semi_loc <- str_locate(lines[indices], ";")
    lines[indices] <- paste0(substring(lines[indices], first = 1, last = semi_loc[, 1]),
                             " if(jerr.ne.0) return;")
    ## Remove additional check on jerr on a line by itself
    jerr_indices <- grep("^if\\(jerr.ne.0\\)", lines)
    matches <- match(indices + 1, jerr_indices)
    matches <- jerr_indices[matches[!is.na(matches)]]
    lines[-matches]
}

#' Return all mortran subroutines with args
#' @param lines character vector of lines of a mortran file
#' @return a list of subroutines
#' @export
#' @examples
#' \dontrun{
#' mortran_subroutines("pclasso.m")
#' }
mortran_subroutines <- function(lines) {
    boundaries <- get_boundaries(lines = lines, what = "subroutine", type = "MORTRAN")
    line_start <- boundaries$start; line_end <- boundaries$end;
    if (length(line_start) > 0) {
        sub_str <- mapply(FUN = function(start, end) lines[start:end], line_start, line_end, SIMPLIFY = FALSE)
        sub_str <- lapply(sub_str, stringr::str_trim)
        sub_str <- lapply(sub_str, sub, pattern = ";", replacement = "") ## rid the semicolon
        lapply(sub_str, paste0, collapse = "")
    } else {
        list()
    }
}

#' Return all fortran subroutines with args
#' @param lines character vector of lines of a mortran file
#' @return a list of subroutines
#' @export
#' @examples
#' \dontrun{
#' fortran_subroutines("pclasso.m")
#' }
fortran_subroutines <- function(lines) {
    boundaries <- get_boundaries(lines = lines, what = "subroutine", type = "FORTRAN")
    line_start <- boundaries$start; line_end <- boundaries$end;
    if (length(line_start) > 0) {
        sub_str <- mapply(FUN = function(start, end) lines[start:end], line_start, line_end, SIMPLIFY = FALSE)
        sub_str <- lapply(sub_str, function(x) {
            n <- length(x)
            if (n > 1) { ## there was a continuation
                x[n] <- stringr::str_trim(substring(x, first = 7))
            }
            x
        })
        lapply(sub_str, paste0, collapse = "")
    } else {
        list()
    }
}

##
## Jerry uses both plain fortran, escaped with %FORTRAN
## and mortran (mode forced with %MORTRAN after a %FORTRAN
## directive).
##
insert_implicit_mortran <- function(in_what = c("subroutine", "function"), lines) {
    boundaries <- get_boundaries(lines = lines, what = in_what, type = "MORTRAN")
    ## padding <- PADDING["MORTRAN"]
    ## ## This handles the MORTRAN statements
    ## in_what <- match.arg(in_what)
    ## ## Find all definitions of in_what at line beginning
    ## line_start <- grep(paste0("^", padding, in_what),  f)
    ## line_end <- line_start
    ## ## Since statements can span multiple lines
    ## ## figure out which is the last line of the
    ## ##  definition. I.e. look for semi-colon
    ## for (i in seq_along(line_end)) {
    ##     x <- line_end[i];
    ##     while(!grepl(";", stringr::str_trim(f[x])))  {
    ##         x <- x + 1
    ##     }
    ##     line_end[i] <- x
    ## }
    ff <- as.list(lines)
    ## Append implicit statement after each statement
    for (x in boundaries$end) {
        ff[[x]] <- c(ff[[x]], "implicit double precision(a-h,o-z);")
    }
    unlist(ff)
}


insert_implicit_fortran <- function(in_what = c("subroutine", "function"), lines) {
    boundaries <- get_boundaries(lines = lines, what = in_what, type = "FORTRAN")
    ## padding <- PADDING["FORTRAN"]
    ## ## This handles the FORTRAN statements
    ## in_what <- match.arg(in_what)
    ## ## Find all definitions of in_what at line beginning
    ## line_start <- grep(paste0("^", padding, in_what),  f)
    ## line_end <- line_start
    ## ## Since statements can span multiple lines
    ## ## figure out which is the last line of the
    ## ##  definition. I.e. look for matched paren
    ## for (i in seq_along(line_end)) {
    ##     x <- line_end[i];
    ##     all_open_paren <- nrow(stringr::str_locate_all(x, "(")[[1]])
    ##     all_close_paren <- nrow(stringr::str_locate_all(x, ")")[[1]])
    ##     diff <- all_open_paren - all_close_paren
    ##     while(diff != 0) {  ## assuming first open paren not continued!!
    ##         x <- x + 1
    ##         all_open_paren <- nrow(stringr::str_locate_all(x, "(")[[1]])
    ##         all_close_paren <- nrow(stringr::str_locate_all(x, ")")[[1]])
    ##         diff <- diff + (all_open_paren - all_close_paren)
    ##     }
    ##     line_end[i] <- x
    ## }
    ff <- as.list(lines)
    ## Append implicit statement after each statement
    for (x in boundaries$end) {
        ff[[x]] <- c(ff[[x]], ##paste0(padding, "implicit integer(i-n)"),
                     paste0(padding, "implicit double precision(a-h,o-z)"))
    }
    unlist(ff)
}

#' Generate fortran and registration code from mortran
#' @param input_mortran_file the input mortran file
#' @param register if TRUE, will generate a registration pkg_init.c file automatically and pkg_name must be supplied
#' @param pkg_name see register argument above, must be the actual package name
#' @param output_mortran_file the output fortran file, optional
#' @import stringr
#' @import tools
#' @export
#' @examples
#' \dontrun{
#' process_file("./pcLasso.m")
#' }
#'
process_mortran <- function(input_mortran_file,
                            register = FALSE,
                            pkg_name = NULL,
                            output_fortran_file = paste0(tools::file_path_sans_ext(basename(input_mortran_file)), ".f")) {

    output_mortran_file = paste0("Fixed-", tools::file_path_sans_ext(basename(input_mortran_file)), ".m")
    cat("Processing Mortran: fixing allocate statements\n")
    lines <- readLines(input_mortran_file)
    lines <- fix_jhf_allocate(lines) ## Fix allocate statements
    cat("Processing Mortran: inserting implicit statements\n")
    lines <- insert_implicit_mortran("subroutine", lines) ## fix mortran subroutines
    lines <- insert_implicit_mortran("function", lines)  ## fix mortran functions
    lines <- insert_implicit_fortran("subroutine", lines) ## fix fortran subroutines
    lines <- insert_implicit_fortran("function", lines)  ## fix fortran functions

    ## Next replace all real by double
    cat("Processing Mortran; replacing reals by double precision\n")
    lines <- gsub("real", "double precision", lines)

    ## Finally fix constants with e[+-]?[0-9]+.
    ##(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)
    ##const.rex <- "([eE][+/]?[[:digit:]]+)"
    ##hits <- gregexpr(const.rex, f, perl =  TRUE)

    cat("Checking for long lines; can cause problems downstream if not fixed\n")
    cat("  Note: Some lines could become longer (> 72 cols) in %FORTRAN sections\n")
    cat("        as a result of 'real' being replaced by 'double precision'. Split\n")
    cat("        such lines into two with a continuation character in col 6\n")

    long_lines <- FALSE; in_fortran <- FALSE; in_mortran <- TRUE
    for (i in seq_along(lines)) {
        x <- stringr::str_trim(lines[i], "right")
        if (!in_fortran) {
            in_fortran <- grepl("^%fortran", x)
            in_mortran <- !in_fortran
        }
        if (!in_mortran) {
            in_mortran <- grepl("^%mortran", x)
            in_fortran <- !in_mortran
        }
        if (in_fortran) {
            if (nchar(x) > 72 && (tolower(substr(x, start = 1, stop = 1)) != 'c')) {
                long_lines <- TRUE
                cat(sprintf("Near line %5d: %s\n", i, x))
            }
        }
    }

    writeLines(lines, con = "temp.m")
    if (long_lines) {
        cat("TODO: Examine offending lines > 72 columns in temp.m\n")
        cat("TODO:     and fix those in *original* mortran. Then rerun.\n")
        stop("Long lines.")
    } else {
        cat("Seems ok, continuing\n")
    }

    ## Now run mortran
    MORTRAN <- system.file("bin", "m77", package = "SUtools")
    MORTRAN_MAC <- system.file("mortran", "src", "m77.mac", package = "SUtools")

    cat("Running Mortran\n")
    system2(command = MORTRAN,
            args = c(MORTRAN_MAC, "./mo.for", "./mortlist"),
            stdin = "temp.m")
    cat("Chopping Lines at 72 cols\n")
    code_lines <- substring(readLines(con = "./mo.for"), 1, 72)
    writeLines(code_lines, con = "temp.f")
    cat("Running gfortran to detect warning lines on unused labels\n")
    system2(command = "gfortran", args = c("-Wunused", "-c", "temp.f"), stderr = "gfortran.out")
    cat("Scanning gfortran output for warnings on unusued labels\n")
    warnings <- readLines("gfortran.out")
    line_numbers <- grep('^temp.f', warnings)
    label_warning_line_numbers <- grep(pattern = "^Warning: Label [0-9]+ at", warnings)
    ## Crude check that no other unused warnings besides labels are present
    nW <- length(label_warning_line_numbers)
    if (sum(label_warning_line_numbers[-nW] + 1 - line_numbers[-1]) > 0) {
        stop("Warnings besides numeric label warnings need fixing; stopping!")
    }
    for (i in seq_len(nW)) {
        offending_line <- as.integer(stringr::str_extract(warnings[line_numbers[i]], pattern = "([0-9]+)"))
        code_line <- code_lines[offending_line]
        offending_label <- stringr::str_extract(warnings[label_warning_line_numbers[i]],
                                       pattern = "([0-9]+)")
        code_lines[offending_line] <- sub(pattern = offending_label,
                                          replacement = str_pad("", width = nchar(offending_label)),
                                          x = code_lines[offending_line])
    }
    writeLines(code_lines, con = output_fortran_file)
    file.rename(from = "temp.m", to = output_mortran_file)
    cat(sprintf("Fixed mortran in %s; fortran in %s\n", output_mortran_file, output_fortran_file))
    if (register) {
        if (is.null(pkg_name)) {
            stop("Registration impossible pkg_name not specified!\n")
        }
        subs <- c(mortran_subroutines(lines), fortran_subroutines(lines))
        gen_registration(pkg_name, unlist(subs))
        cat(sprintf("Registration file in %s\n", paste0(pkg_name, "_init.c")))
    }
    invisible(TRUE)
}

