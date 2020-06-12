#' This function use the read_csv function in the package readr.
#' It reads a single csv file into R for analysis.
#' the packages readr and dplyr should be installed.
#'
#' @note If the file does not exist at the path it will create an error
#' @param filename A character string denoting the .csv or .csv.bz2 file to be read into R.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @return An R object of class data.frame
#' @examples \dontrun{
#' path <- "accident_2013.csv.bz"
#' fars_read(path)
#' fars_read("accident_2013.csv.bz")
#' }
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' This function use the sprintf function to create a filename
#' for the data in the year  observed in csv.bz2 format.
#'
#' @note If input cannot be coerced to integer(s) warning will result and NA(s) will be returned.
#' @param year A numerical vector for the year to include in the file name
#' @return A character vector indicating the name of the files in .csv.bz2 format
#' @examples \dontrun{
#' make_filename(2013)
#' make_filename('2013')
#' make_filenam(c(2013,2015))
#' }
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' This function takes a numerical vector of years to be included in the data frame output.
#' Data should be in .csv.bz2.
#' If a year is not included in the data the function shows a warning and return null for that year.
#'
#' @note If file does not exist for corresponding year a warning will be generated and NULL returned for that year
#' @param years A numerical vector indicating the years to include in the search
#' @return A list of data frames for each year that was requested.
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read_years}}
#' @importFrom readr read_csv
#' @importFrom magrittr %>%
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @examples \dontrun{
#' fars_read_years(2013)
#' fars_read_years(2013L)
#' fars_read_years('2013')
#' fars_read_years(c(2013, 2014))
#' fars_read_years(list(2013, 2015))
#' }
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

#'This functions creates a data frame containing number of records available per month
#'
#' @note If file does not exist for corresponding year a warning will be generated.
#' @param years A numerical vector indicating which years to include in the output dataframe.
#' @return A data frame with the variables MONTH and each year where data is
#'   avaiable, summarizing the number of observations recorded each month per year.
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read_years}}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @examples \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(2013L)
#' fars_summarize_years('2013')
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years(list(2013, 2015))
#' }
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' This function takes a numerical vector of the state and year when the accident occurred
#' and shows the location where traffic accidents occurred
#' Data should be in .csv.bz2
#'
#' @note If state number does not exist in file will generate an error
#' If no records for state exist will write message and return an a invisible(NULL)
#' @param state.num A numerical vector of a particular state in the USA.
#' @param year A numerical vector of a year.
#' @seealso \code{\link{make_filename}}
#' @seealso \code{\link{fars_read}}
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @return A plot of the map of the state with points where of the accidents occurred
#' @examples \dontrun{
#' fars_map_state(4 , 2014)
#' fars_map_state('8' , '2015')
#' }
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


