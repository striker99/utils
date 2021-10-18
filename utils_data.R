# Modify the `Shiller` function from the NMOF package to read the 
# chapt26.xlsx, an annual data set provided by Robert Shiller.
# Based on:
# https://github.com/enricoschumann/NMOF/blob/master/R/market_data.R

Shiller_annual <- function(dest.dir = ".",
                    url = "http://www.econ.yale.edu/~shiller/data/chapt26.xlsx") {
  
  f.name <- paste0(format(Sys.Date(), "%Y%m%d_"),
                   "chapt26.xlsx")
  f.path <- file.path(normalizePath(dest.dir), f.name)
  
  if (!file.exists(f.path))
    dl.result <- download.file(url, destfile = f.path)
  else
    dl.result <- 0
  
  if (dl.result != 0L) {
    warning("download failed with code ", dl.result, "; see ?download.file")
    return(invisible(NULL))
  }
  
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("file downloaded, but package ",
         sQuote("readxl"), " is not available")
  if (!requireNamespace("datetimeutils", quietly = TRUE))
    stop("file downloaded, but package ",
         sQuote("datetimeutils"), " is not available")
  
  data <- suppressMessages(suppressWarnings(
    readxl::read_xlsx(f.path, sheet = "Data")))
  data <- as.data.frame(data)
  data <- data[-(1:7), ]
  data <- data[, 1:21]
  data <- data[, -c(10)] ## drop the extra year column
  
  colnames(data) <- c("date",
                      "p",
                      "div",
                      "e",
                      "y1",
                      "y10",
                      "cpi",
                      "rate_1yr_real",
                      "cons",
                      "p_real",
                      "div_real",
                      "pv_div_market_r",
                      "pv_div_cons_disc",
                      "div_real",
                      "r_real",
                      "r_log",
                      "e_real",
                      "pe_real",
                      "e_10yr_avg",
                      "CAPE")
  

  data <- data[!is.na(data[["date"]]), ]
  tmp <- seq(as.Date("1871-01-01"), by = "1 year",
             length.out = nrow(data))
  data[["date"]] <- datetimeutils::end_of_month(tmp)
  
  for (i in 2:ncol(data)) ## there will be NAs => warnings
    data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
  
  
  
  
  data
}
