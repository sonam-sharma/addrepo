## ------------------------------------------------------------------------
list.files()
file.exists("accident_2013.csv")

## ----reading the file, include = TRUE------------------------------------
fars_read <- function(filename) {
  list.files()
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
     readr::read_csv(filename, progress = FALSE)
   
    
  })
   dplyr::tbl_df(data)
  
}
head(data)
 fars_read("accident_2013.csv")


## ----generating file name,include=TRUE-----------------------------------
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv", year)
}


## ----return year and month,include=TRUE----------------------------------
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(~dat, year = ~year) %>%
        dplyr::select_(~MONTH, ~year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

## ----summarize years,include=TRUE----------------------------------------
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(~dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_(key_col='year',value_col='n')
}

## ----map state,include=TRUE----------------------------------------------
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
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


fars_map_state(1,2013)




