## Global variables
PADDING <- c(FORTRAN = "      ",  ## 6 spaces for fortran
             MORTRAN = "")        ## None
LINE_LIMIT <- c(FORTRAN = 72L,
                MORTRAN = 80L)
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
    boundaries <- get_boundaries(lines = lines, what = in_what, type = "FORTRAN")
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

#' Generate fortran and registration code from mortran
#' @param input_mortran_file the input mortran file
#' @param register if TRUE, will generate a registration pkg_init.c file automatically and pkg_name must be supplied
#' @param pkg_name see register argument above, must be the actual package name
#' @param output_mortran_file the output fortran file, optional
#' @param output_directory the output directory, default current directory
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
                            output_fortran_file = paste0(tools::file_path_sans_ext(basename(input_mortran_file)), ".f"),
                            output_dir = ".") {

    output_mortran_file = paste0("Fixed-", tools::file_path_sans_ext(basename(input_mortran_file)), ".m")
    cat("Processing Mortran: fixing allocate statements\n")
    lines <- readLines(input_mortran_file)
    lines <- stringr::str_trim(lines, side = "right")
    ## Check for long lines
    ## long_lines <- get_long_lines(lines)
    ## if (nrow(long_lines) > 0) {
    ##     print(long_lines)
    ##     stop("Lines longer than 80 chars found in mortran; see above.")
    ## }
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
    cat("  Note: Some lines could become longer, > 72 cols in %FORTRAN sections\n")
    cat("        and > 80 cols in MORTRAN sections as a result of 'real' being\n")
    cat("        replaced by 'double precision'. Split such lines into two;\n")
    cat("        in %FORTRAN sections, use a continuation character in col 6.\n")

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
        if (in_mortran) {
            if (nchar(x) > 80 && (tolower(substr(x, start = 1, stop = 1)) != 'c')) {
                long_lines <- TRUE
                cat(sprintf("Near line %5d: %s\n", i, x))
            }
        }

    }

    writeLines(lines, con = file.path(output_dir, "temp.m"))
    if (long_lines) {
        cat("TODO: Examine offending lines > 72 columns in temp.m\n")
        cat("TODO:     and fix those in *original* mortran. Then rerun.\n")
        stop("Long lines.")
    } else {
        cat("Seems ok, continuing\n")
    }

    ## Now run mortran
    m77_exe <- "m77"
    if (.Platform$OS.type == "windows") m77_exe <- "m77.exe"
    MORTRAN <- system.file("bin", m77_exe, package = "SUtools")
    MORTRAN_MAC <- system.file("mortran", "src", "m77.mac", package = "SUtools")

    cat("Running Mortran\n")
    system2(command = MORTRAN,
            args = c(MORTRAN_MAC, file.path(output_dir, "mo.for"), file.path(output_dir, "mortlist")),
            stdin = file.path(output_dir, "temp.m"))
    cat("Chopping Lines at 72 cols\n")
    code_lines <- substring(readLines(con = file.path(output_dir, "mo.for")), 1, 72)
    writeLines(code_lines, con = file.path(output_dir, "temp.f"))
    cat("Running gfortran to detect warning lines on unused labels\n")
    system2(command = "gfortran",
            args = c("-Wunused", "-c", file.path(output_dir, "temp.f"), "-o", file.path(output_dir, "temp.o")),
            stderr = file.path(output_dir, "gfortran.out"))
    cat("Scanning gfortran output for warnings on unusued labels\n")
    warnings <- readLines(file.path(output_dir, "gfortran.out"))
    line_numbers <- grep('/temp.f', warnings)
    label_warning_line_numbers <- grep(pattern = "^Warning: Label [0-9]+ at", warnings)
    just_warnings <- sum(grepl('Warning:', warnings))

    nW <- length(label_warning_line_numbers)
    for (i in seq_len(nW)) {
        offending_line <- as.integer(stringr::str_extract(warnings[line_numbers[i]], pattern = "([0-9]+)"))
        code_line <- code_lines[offending_line]
        offending_label <- stringr::str_extract(warnings[label_warning_line_numbers[i]],
                                       pattern = "([0-9]+)")
        code_lines[offending_line] <- sub(pattern = offending_label,
                                          replacement = str_pad("", width = nchar(offending_label)),
                                          x = code_lines[offending_line])
    }
    writeLines(code_lines, con = file.path(output_dir, output_fortran_file))
    file.rename(from = file.path(output_dir, "temp.m"), to = file.path(output_dir, output_mortran_file))
    cat(sprintf("Fixed mortran in %s; fortran in %s\n", output_mortran_file, output_fortran_file))
    ## Crude check that no other unused warnings besides labels are present
    if (nW != just_warnings) {
        cat("There exist warnings _besides_ numeric label warnings that need fixing;\n")
        stop(sprintf("Try running gfortran -c -Wunused %s\n", output_fortran_file))
    }

    if (register) {
        if (is.null(pkg_name)) {
            stop("Registration impossible pkg_name not specified!\n")
        }
        subs <- c(mortran_subroutines(lines), fortran_subroutines(lines))
        gen_registration(pkg_name = pkg_name, fun_list = unlist(subs), output_dir = output_dir)
        cat(sprintf("Registration file in %s\n", paste0(pkg_name, "_init.c")))
    }
    invisible(TRUE)
}

