## Global variables
PADDING <- c(FORTRAN = "      ",  ## 6 spaces for fortran
             MORTRAN = "")        ## None
LINE_LIMIT <- c(FORTRAN = 72L,
                MORTRAN = 80L)
##
## Get Line starts and line end boundaries of functions or subroutines
##
get_boundaries <- function(lines, what = c("subroutine", "function"),
                           type = c("FORTRAN", "MORTRAN"), ignore_entries = FALSE) {
    what <- match.arg(what)
    ## entries are also subroutines!
    if (!ignore_entries && what == "subroutine") what  <- "(subroutine|entry)"
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
            all_open_paren <- nrow(stringr::str_locate_all(lines[x], "\\(")[[1]])
            all_close_paren <- nrow(stringr::str_locate_all(lines[x], "\\)")[[1]])
            diff <- all_open_paren - all_close_paren
            while(diff != 0) {  ## assuming first open paren not continued!!
                x <- x + 1
                all_open_paren <- nrow(stringr::str_locate_all(lines[x], "\\(")[[1]])
                all_close_paren <- nrow(stringr::str_locate_all(lines[x], "\\)")[[1]])
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
    ## Fix in v 0.4 where I was not accounting for this to be empty!
    if (length(jerr_indices) > 0) {
        matches <- match(indices + 1, jerr_indices)
        matches <- jerr_indices[matches[!is.na(matches)]]
        lines <- lines[-matches]
    }
    lines
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
        sub_str <- lapply(sub_str, sub, pattern = ";", replacement = "") ## drop the semicolon
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
            if (length(x) > 1) {
                ## there was a continuation, so trim all lines but first
                rest <- sapply(x[-1], function(line) stringr::str_trim(stringr::str_sub(line, start = 7)))
                result  <- paste0(x[[1]], rest, collapse = "")
            } else {
                result  <- x
            }
            result
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
    ## implicit only applies to subroutines and functions, not to entries
    boundaries <- get_boundaries(lines = lines, what = in_what, type = "MORTRAN", ignore_entries = TRUE)
    ff <- as.list(lines)
    n <- length(ff)
    ## Append implicit statement after each statement
    ## if not already present!!
    for (x in boundaries$end) {
        if (x < n && !grepl(pattern = "implicit", x = ff[[x+1]])) {
            ff[[x]] <- c(ff[[x]],
                         paste0(PADDING["MORTRAN"], "implicit double precision(a-h,o-z);"))
        }
    }
    unlist(ff)
}


insert_implicit_fortran <- function(in_what = c("subroutine", "function"), lines) {
    ## implicit only applies to subroutines and functions, not to entries
    boundaries <- get_boundaries(lines = lines, what = in_what, type = "FORTRAN", ignore_entries = TRUE)
    ff <- as.list(lines)
    n <- length(ff)
    ## Append implicit statement after each statement
    for (x in boundaries$end) {
        if (x < n && !grepl(pattern = "implicit", x = ff[[x+1]])) {
            ff[[x]] <- c(ff[[x]],
                         paste0(PADDING["FORTRAN"], "implicit double precision(a-h,o-z)"))
        }
    }
    unlist(ff)
}


get_long_lines <- function(lines, section = c("MORTRAN", "FORTRAN")) {
    section <- match.arg(section)
    limit <- LINE_LIMIT[section]
    index <- which(nchar(lines) > limit & !str_starts(tolower(lines), "c"))
    if (length(index) > 0) {
        data.frame(lineNo = index, line = lines[index], stringsAsFactors = FALSE)
    } else {
        data.frame(lineNo = integer(0), line = character(0))
    }
}

#' Generate fortran and registration code from mortran, geared
#' specifically towards the mortran used by Jerome Friedman at
#' Stanford University
#' @param input_mortran_file the input mortran file
#' @param pkg_name the package name signalling that registration code
#'     has to be generated
#' @param verbose a flag for verbose output
#' @return a named list of processed mortran lines (`mortran`),
#'     fortran lines (`fortran`) and, if so requested and no warnings
#'     occur, a registration c code as the third argument
#'     appropriately named using the package name provided
#' @importFrom knitr kable
#' @importFrom stringr str_trim
#' @importFrom tools file_path_sans_ext
#' @export
#' @examples
#' \dontrun{
#' process_mortran("./pcLasso.m")
#' }
#'
process_mortran <- function(input_mortran_file,
                            pkg_name = NULL,
                            verbose = TRUE) {
    result = list(mortran = NULL, fortran = NULL)
    if (verbose) cat("Processing Mortran: fixing allocate statements\n")
    lines <- readLines(input_mortran_file)
    lines <- stringr::str_trim(lines, side = "right")
    ## Check for long lines
    ## long_lines <- get_long_lines(lines)
    ## if (nrow(long_lines) > 0) {
    ##     print(long_lines)
    ##     stop("Lines longer than 80 chars found in mortran; see above.")
    ## }
    lines <- fix_jhf_allocate(lines) ## Fix allocate statements
    if (verbose) cat("Processing Mortran: inserting implicit statements\n")
    lines <- insert_implicit_mortran("subroutine", lines) ## fix mortran subroutines
    lines <- insert_implicit_mortran("function", lines)  ## fix mortran functions
    lines <- insert_implicit_fortran("subroutine", lines) ## fix fortran subroutines
    lines <- insert_implicit_fortran("function", lines)  ## fix fortran functions

    ## Next replace all real by double
    if (verbose) cat("Processing Mortran; replacing reals by double precision\n")
    lines <- gsub("real", "double precision", lines)

    ## Finally fix constants with e[+-]?[0-9]+.
    ##(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)
    ##const.rex <- "([eE][+/]?[[:digit:]]+)"
    ##hits <- gregexpr(const.rex, f, perl =  TRUE)

    if (verbose) {
        cat("Checking for long lines; can cause problems downstream if not fixed\n")
        cat("  Note: Some lines could become longer, > 72 cols in %FORTRAN sections\n")
        cat("        and > 80 cols in MORTRAN sections as a result of 'real' being\n")
        cat("        replaced by 'double precision'. Split such lines into two;\n")
        cat("        in %FORTRAN sections, use a continuation character in col 6.\n")
    }

    long_lines_df  <- detect_long_lines(lines)

    ##writeLines(lines, con = file.path(output_dir, "temp.m"))
    if (nrow(long_lines_df) > 0) {
        cat("TODO: Examine offending lines > 72 columns in temp.m\n")
        cat("TODO:     and fix those in *original* mortran. Then rerun.\n")
        print(knitr::kable(long_lines_df))
        return(result)
    } else {
        if (verbose) cat("Seems ok, continuing\n")
    }
    result$mortran  <- lines  ## save the mortran
    ## Now generate fortran
    if (verbose) cat("Generating Fortran from Mortran\n")
    fortran_lines  <- generate_fortran(lines)
    if (verbose) cat("Checking Fortran\n")
    ## Fix unused labels automatically if possible
    result$fortran  <- fix_unused_labels(fortran_lines, verbose)

    if (!is.null(pkg_name)) {
        subs <- c(mortran_subroutines(lines), fortran_subroutines(lines))
        registration <- gen_registration(pkg_name = pkg_name, fun_list = unlist(subs))
        result[[paste0(pkg_name, "_init.c")]]  <- registration
    }
    result
}

#' Detect if there are long lines in mortran or fortran sections
#' @param mortran_lines the mortran file lines resulting from a
#'     `base::readLines()`, say.
#' @return a possibly empty data frame of the approximate line number
#'     and the offending line if any
#' @importFrom stringr str_trim
#' @export
#' @examples
#' \dontrun{
#' check_long_lines(readLines("./pcLasso.m"))
#' }
#'
detect_long_lines <- function(mortran_lines) {
    long_lines <- FALSE; in_fortran <- FALSE; in_mortran <- TRUE
    out_df <- data.frame(line_no = integer(0),  line =character(0), stringsAsFactors = FALSE)
    count  <- 0
    for (i in seq_along(mortran_lines)) {
        x <- stringr::str_trim(mortran_lines[i], "right")
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
                count  <- count + 1
                out_df[count, ]  <- c(i, x)
            }
        }
        if (in_mortran) {
            if (nchar(x) > 80 && (tolower(substr(x, start = 1, stop = 1)) != 'c')) {
                long_lines <- TRUE
                count  <- count + 1
                out_df[count, ]  <- c(i, x)
            }
        }

    }
    out_df
}

#' Generate fortran from a mortran file and return the fortran
#' @param mortran_lines the mortran file lines resulting from a
#'     `base::readLines()`, say.
#' @return fortran lines as a character vector
#' @importFrom stringr str_trim str_extract str_pad
#' @export
#' @examples
#' \dontrun{
#' generate_fortran(readLines("./pcLasso.m"))
#' }
#'
generate_fortran <- function(mortran_lines) {
    ## Now run mortran

    if (.Platform$OS.type == "windows") {
        m77_exe <- "m77.exe"
        MORTRAN <- system.file("bin", .Platform$r_arch, m77_exe, package = "SUtools")
    } else {
        m77_exe <- "m77"
        MORTRAN <- system.file("bin", m77_exe, package = "SUtools")
    }
    MORTRAN_MAC <- system.file("mortran", "src", "m77.mac", package = "SUtools")
    output_dir  <- tempdir()
    tfile <- file.path(output_dir, "temp.m")
    writeLines(mortran_lines, tfile)
    ## cat("Running Mortran\n")
    system2(command = MORTRAN,
            args = c(MORTRAN_MAC, file.path(output_dir, "mo.for"), file.path(output_dir, "mortlist")),
            stdin = file.path(output_dir, "temp.m"))
    readLines(con = file.path(output_dir, "mo.for"))
}


#' Fix unused label warnings from gfortran if possible
#' @param mortran_lines the mortran file lines resulting from a
#'     `base::readLines()`, say.
#' @param verbose a flag for verbose output
#' @return fortran lines as a character vector
#' @importFrom stringr str_trim str_extract str_pad str_locate
#' @export
#' @examples
#' \dontrun{
#' fix_unused_labels(readLines("foo.f"))
#' }
#'
fix_unused_labels <- function(fortran_lines, verbose = FALSE) {
    if (verbose) cat("Chopping Lines at 72 cols\n")
    code_lines <- substring(fortran_lines, 1, 72)
    output_dir  <- tempdir()
    writeLines(code_lines, con = file.path(output_dir, "temp.f"))
    if (verbose) cat("Running gfortran to detect warning lines on unused labels\n")
    system2(command = "gfortran",
            args = c("-Wunused-label", "-c", file.path(output_dir, "temp.f"), "-o", file.path(output_dir, "temp.o")),
            stderr = file.path(output_dir, "gfortran.out"))
    if (verbose) cat("Scanning gfortran output for warnings on unusued labels\n")
    ##
    ## Explanation: In the compiler output, each warning for unusued label has this form:
    ## ./temp.f:183:5:
    ## 183 | 10382 continue
    ##     |     1
    ## Warning: Label 10382 at (1) defined but not used [-Wunused-label]
    ## So we first locate the "Warning: Label 10382..." lines in the compiler output,
    ## then work backward from that to locate the "/temp.f:183:5" line
    ## from which we can extract the actual offending line, line 183
    ## However since we are not sure it is always a group of four such lines,
    ## we use brute force search rather than something sexier.
    ##
    compiler_output <- readLines(file.path(output_dir, "gfortran.out"))
    label_warning_line_numbers <- grep(pattern = "^Warning: Label [0-9]+ at", compiler_output)
    ## Locate preceding "/temp.f" line for each unused label warning
    line_numbers <- sapply(label_warning_line_numbers, function(l) {
        repeat({
            l  <- l - 1
            if (grepl('/temp.f', compiler_output[l])) break
        })
        if (l < 1) stop("Error in SUtools::fix_unused_labels; report to author with reprex!")
        l
    })

    line_number_str_start  <- stringr::str_locate((compiler_output[line_numbers])[1], "/temp.f:")[2]

    for (i in seq_along(label_warning_line_numbers)) {
        lno  <- line_numbers[i]
        line  <- compiler_output[lno]
        ##cat(sprintf("i: %d; lno : %d;  line: %s\n", i, lno, line))
        line_part  <- substring(line, line_number_str_start)
        offending_line <- as.integer(stringr::str_extract(line_part, pattern = "([0-9]+)"))
        code_line <- code_lines[offending_line]
        offending_label <- stringr::str_extract(compiler_output[label_warning_line_numbers[i]],
                                       pattern = "([0-9]+)")
        code_lines[offending_line] <- sub(pattern = offending_label,
                                          replacement = stringr::str_pad("", width = nchar(offending_label)),
                                          x = code_lines[offending_line])
    }
    code_lines
}
