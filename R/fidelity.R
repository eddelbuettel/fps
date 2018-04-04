
#' Retrieve Most Recent Closing Prices from Fidelity
#'
#' @details This is fairly raw code which may break.
#' @return A data.table object with column Symbol, Name, NAV and Change.
#' @examples
#' getFidelity()
getFidelity <- function() {
    d <- read_html("https://fundresearch.fidelity.com/mutual-funds/fidelity-funds-daily-pricing-yields")
    dt <- html_table(html_nodes(d, "table"), fill=TRUE)
    l <- rbindlist(dt, fill=TRUE)

    ## there are dual headers so we set the upper one ...
    rowone <- unname(as.data.frame(l[1,])[1,])
    rowone[1] <- "FundName"
    setnames(l, as.character(rowone))

    ## ... and then remove the others leading each subsection
    l <- l[ !grepl("^Fund.*Name", FundName), ]

    ## split off name and symbol
    l[, c("Name", "Symbol") := tstrsplit(FundName, "\n\t\t\t\t")]
    l[, FundName := NULL]
    l[, Symbol:=gsub("[()]", "", Symbol)]

    setnames(l, 1, "NAV")
    setnames(l, 2, "Change")
    l[, Change := gsub("\\+", "", Change)]
    l <- l[, .(Name, Symbol, NAV, Change)]
    l <- l[, head(.SD,1), by=Symbol]
    l <- l[ !is.na(Change) & !is.na(NAV),]
    l <- l[ , NAV := as.numeric(NAV)]
    l <- l[ !is.na(NAV) , ]
    l <- l[, Change:=as.numeric(Change)]

    l
}

## R CMD check and data.table ...
utils::globalVariables(c("FundName", ":=", "Symbol", "Change", ".", "Name", "NAV", ".SD"))
