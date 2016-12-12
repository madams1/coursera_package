#' Read FARS data
#'
#' Reads a file from `/data` inside fars_data.zip based on its filename.
#' Useful in conjunction with `make_filename()`.
#'
#' Needs `dplyr` and `readr` packages available.
#'
#' @param filename A character string for the name of a file in the `data` directory within fars_data.zip
#'
#' @return Returns a dataframe of accident data for a given filename.
#'     An error is produced if the file can't be located with the given `filename`.
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
#' @examples
#' fars_2015 <- fars_read(make_filename(2015))
#' dplyr::glimpse(fars_2015)


#' Create FARS filename
#'
#' A helper function for creating a filename based on the desired accident year.
#' Useful in conjunction with `fars_read()`.
#'
#' @param year An integer-valued, 4 number year (or a value that can be coerced to such)
#'
#' @return Returns a character string that represents a filename for
#'     accident data located in fars_data.zip
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}
#' @examples
#' make_filename(2013)


#' Get month and year from multiple FARS data sets at once
#'
#' Needs `dplyr` and `readr` packages available.
#'
#' @param years A list of integer-valued, 4 number years (or values that can be coerced to such)
#'
#' @return Returns a list with a length equal to `length(years)`.
#'     For a valid year, the list element is a tibble with `MONTH` and `year` columns from the corresponding yearly FARS data.
#'     Each invalid year will return `NULL` and a warning. Specifically if `!year %in% 2013:2015`.
#'
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
#' @examples
#' fars_2013_to_2015 <- fars_read_years(2013:2015)


#' Count monthly records from multiple (yearly) FARS data sets
#'
#' Needs `dplyr`, `readr`, and `tidyr` packages available.
#'
#' @param years A list of integer-valued, 4 number years (or values that can be coerced to such)
#'
#' @return Returns a tibble containing the number of monthly fatal injuries suffered in motor
#'     vehicle traffic crashes for each year in `years`.
#'     Provides a warning for (and excludes) any invalid years from the calculated summary.
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}
#' @examples
#' monthly_fars_2013_to_2015 <- fars_summarize_years(2013:2015)


#' Plot accidents in year for state
#'
#' Needs `dplyr`, `readr`, `maps`, and `graphics` packages available. `maps` package should be loaded.
#'
#' @param state.num An integer (or a value that can be coerced to such) representing a state's numeric code.
#' @param year An integer-valued, 4 number year (or a value that can be coerced to such)
#'
#' @return Plots the location of accidents for the given state and year based on their latitude and longitude.
#'     Will return an error if `state.num` is invalid.
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
#' @examples
#' fars_map_state(1, 2013)
