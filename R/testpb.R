#' Create a progress bar for calling from fortran
#' @param ... args to `utils::txtProgressBar` including min and max
#'
#' @export createPB
createPB  <- function(...) {
    pb  <- txtProgressBar(...)
    .Call("storePB", pb, new.env(), PACKAGE = "SUtools")
    pb
}

#' Call the Fortran to exercise the saved progress bar
#'
#' @export test_fort_pb
test_fort_pb  <- function() {
    .Fortran("testpb", PACKAGE = "SUtools")
}

#' Bump the progress bar from Fortran
#'
#' @export test_fort_pb2
test_fort_pb2  <- function() {
    pb  <- createPB(min = 0, max = 100, style = 3)
    for (i in 1:100) {
        .Fortran("setpb", as.integer(i), PACKAGE = "SUtools")
    }
    close(pb)
}

