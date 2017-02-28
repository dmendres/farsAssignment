#' Read a FARS Data File
#'
#' This function reads a (possibly compressed) .csv format file specified by the \code{filename} argument and returns it as a
#' \code{tbl_df data.frame}.
#'
#' @param filename A character string giving the name of the .csv format file to be read.
#'
#' @return This function returns a \code{tbl_df} form of a \code{data.frame}.
#'
#' @details The function will fail if the file specified by the \code{filename} argument does not exist.
#' The \code{read_csv} function may fail if the file is not readable. Any warnings introduced by parsing failures are suppressed.
#'
#' The following functions are imported: \code{readr::read_csv}, \code{dplr::tbl_df}.
#'
#' @examples
#' #improper name format
#' fars_read("fars.csv")
#'
#' fars_read("accident_2014.csv.bz2")
#'
#'
#' @importFrom readr read_csv
#' @importFrom dplr tbl_df
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make a FARS Data File Name
#'
#' This function produces a filename which follows the conventions of
#' FARS (Federal Accident Reports) naming including the supplied \code{year} argument.
#'
#' @param year The 4-digit year number (YYYY) to be embedded in the file name.
#'
#' @return This function returns a \code{string} of the form: "accident_YYYY.csv.bz2".
#'
#' @examples
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read multiple FARS data files, retaining only the month and year of accident reports
#'
#' This function reads the FARS file for each of the years in the \code{years} list argument,
#' retaining only the \code{MONTH} and \code{year} of the FARS reports in a list containing one \code{data.frame} per year.
#'
#' @param years A list of 4-digit year numbers (YYYY) specifying the FARS files to be read.
#'
#' @return This function returns a \code{list} of \code{data.frame}s, one per year,
#' in which each observation reflects the month and year of an accident report.
#'
#' @details The function will produce a warning and skip the year if there is no FARS data file,
#' or the data file cannot be read, for any of the given years.
#'
#' The following functions are imported: \code{dplr::mutate, dplr::select}.
#'
#' @examples
#' #warns of an invalid year
#' fars_read_years(list(2015,2019))
#'
#' fars_read_years(list(2013,2014))
#'
#' @importFrom dplr mutate
#' @importFrom dplr select
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize the content of multiple FARS data files
#'
#' This function produces a summary \code{data.frame} containing the number of reports
#' by month for the FARS files corresponding to each year in the \code{years} list.
#'
#' @param years A list of 4-digit year numbers (YYYY) specifying the FARS files to be read.
#'
#' @return This function returns a \code{data.frame} of 12 observations, one each month,
#' of the number FARS reports in the specified years.
#'
#' @details  Missing years will be skipped with a warning.
#'
#' The following functions are imported: \code{dplr::bind_rows, dplr::group_by, dply::summarize}, and \code{tidyr::spread}
#'
#' @examples
#' fars_summarize_years(list(2013, 2014, 2015))
#' # will produce a warning
#' fars_summarize_years(list(2013,2016))
#' # will fail
#' fars_summarize_years(1999)
#'
#' @importFrom dplr bind_rows
#' @importFrom dplr group_by
#' @importFrom dply summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Plot the location of fars accident reports
#'
#' This function plots the accident reports of a given year
#' and in a given state on a map of the state.
#'
#' @param state.num the numeric code for the state or territory to be plotted.
#' @param year A 4-digit year number (YYYY) specifying the FARS file to be read.
#'
#' @return The function returns a NULL value.
#'
#' @details The function will fail if the state code is not in FARS file.
#' If there are no accidents to plot, a message is produced to that effect.
#'
#' The following functions are imported: \code{maps::map}, \code{graphics::points}, \code{dply::filter}.
#'
#' @examples
#' fars_map_state(1, 2014)
#' # following will fail
#' fars_map_state(1, 2019)
#'
#' @importFrom maps map
#' @importFrom graphics points
#' @importFrom dply filter
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
