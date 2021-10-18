Shiller2 <- function(dest.dir,
                    url = "http://www.econ.yale.edu/~shiller/data/ie_data.xls") {

    f.name <- paste0(format(Sys.Date(), "%Y%m%d_"),
                     "ie_data.xls")
    f.path <- file.path(normalizePath(dest.dir), f.name, fsep = .Platform$file.sep)

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
        readxl::read_xls(f.path, sheet = "Data")))
    data <- as.data.frame(data)
    data <- data[-(1:6), ]
    data <- data[, 1:22]
    data <- data[, -c(6, 14, 16)] ## drop column 'Date Fraction' and empty column

    colnames(data) <- c("Date",
                        "Price",
                        "Dividend",
                        "Earnings",
                        "CPI",
                        "Long Rate",
                        "Real Price",
                        "Real Dividend",
                        "Real Total Return Price",
                        "Real Earnings",
                        "Real TR Scaled Earnings",
                        "CAPE",
                        "TR CAPE",
                        "Excess CAPE Yield",
                        "Monthly Total Bond Returns",
                        "Real Total Bond Returns",
                        "Ten Year Annualized Stock Real Return",
                        "Ten Year Annualized Bonds Real Return",
                        "Real 10 Year Excess Annualized Returns"
                        )

    data <- data[!is.na(data[["Date"]]), ]
    tmp <- seq(as.Date("1871-01-01"), by = "1 month",
               length.out = nrow(data))
    data[["Date"]] <- datetimeutils::end_of_month(tmp)

    for (i in 2:ncol(data)) ## there will be NAs => warnings
        data[[i]] <- suppressWarnings(as.numeric(data[[i]]))
    data
}
