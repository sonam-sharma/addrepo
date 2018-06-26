#' Reads input file containing data regarding fatal injuries suffered in motor vehicle
#' traffic crashes.
#' @title Reading an input file.
#' @description This function checks whether a given file exists or not. If the file exists , it reads
#' the file into a data frame. Otherwise it throws an error with the message "File <filename>  does not exists".
#' @param filename This can be either a variable holding the name of the file or the filename itself.
#' @return This function returns a data frame for the input file.
#' @details If the system is not able to locate the given file in the current working
#' directory or the complete filename is not provided, this function will throw an error.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' fars_read("accident_2013.csv")
#' fars_read("accident_2015.csv")
#' data1<-"data/accident.csv"
#' fars_read(data1)
#' @export

fars_read <- function(filename) {
  # if(!file.exists(filename))
  #  stop("file '", filename, "' does not exist")
  data1 <- suppressMessages({
    lapply(list.files(system.file('extdata', package = 'mynewpackage'), full.names = TRUE), read.csv)
  })
  class(data1)

  head(data1)
  data2<-as.data.frame(data1)
  dplyr::tbl_df(data2)

}
#' @title Generating name of the input file based on year provided.
#' @description This function returns the name of the file by formatting it with user supplied
#' year.
#' @param year  The year for which the information regarding fatal injuries is required. Valid value for <year>
#' will be those year for which user has "Accident_<year>.csv" file present in the local directory.
#' @return This function returns the name of the file for the year provided.
#' @examples
#' make_filename(2013)
#' yr<-2013
#' make_filename(yr)
#' make_filename(2015)
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}
#' @title Selecting and printing the months present in the input file for the given year.
#' @description  This function creates the filename by appending  user provided year to it.
#' It reads the file and selects the month present in the file for that year  as well as the year also.
#' @inheritDotParams make_filename year
#' @return This function returns the year and the month present in the file for the selected year.
#' @details  This function will throw an error if the year provided by user is not a valid year.
#' @importFrom dplyr mutate select
#' @examples
#' fars_read_years(2013)
#' fars_read_years(2015)
#' @export
fars_read_years <- function(years) {
  lfile<-dat<-MONTH<-NULL
  lapply(years, function(year) {
    file <- make_filename(year)

    tryCatch({
      dat <- fars_read(file)
      dat%>%dplyr::mutate_( year = ~year) %>%
        dplyr::select_(~MONTH, ~year)

    },
    error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#' @title Total number of accidental injuries based on month for the given year.
#' @description  This function reads the input file for the year provided by the user and
#' then returns the number of observation for each month of the given year.
#' @inheritDotParams make_filename year
#' @return This function returns the month and the total number of observation for each
#'  month present in the file for the given year.
#' @details  this function will throw an error if the year provided by user is not a valid year.
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' fars_summarize_years(2013)
#' fars_summarize_years(2015)
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_(key_col='year',value_col='n')
}


#' @title Draws a geographical map for the accidental injuries for the given state and year.
#' @description  This function reads the input file for the year provided by the user
#' and returns a geographical map depicting the fatal injuries suffered in motor vehicle
#' traffic crashes.
#' @inheritDotParams make_filename year
#' @param state Number for each state. For more information , \href{Fatality Analysis Reporting System}{Fatality Analysis Reporting System}.
#' @return This function returns a geographical map for the given state(s) with each dot as an obsevation
#' of the accidental injuries.
#' @details  this function will throw an error if the year provided by user is not a valid year. For the definition on
#' valid year look at parameter 'year'.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' fars_map_state(1,2013)
#' fars_map_state(2,2015)
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  # if(!(state.num %in% unique(data["STATE"])))
  #  stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
