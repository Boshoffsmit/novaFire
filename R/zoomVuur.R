#' Zoom Vuur
#'
#' Creates a line and point plot of data and allows the user to manipulate the data
#' by being able to zoom in or out of the date axis
#'
#' @param x Data frame. Contains fire data
#' @param mn Date object. Refers to the minimum date
#' @param mx Date object. Refers to the maximum date
#' @param hoof Character vector. The header of the plot. Does not have a header
#' by default
#' @param timeZone Character vector. The time zone under consideration
#' @param useOnlyNumeric Logical. Plots only numeric columns when TRUE
#' @details Use in format x = vuurlys[[2]] or
#' structure(list(B = c(15, 15, 14.75, 14.5, 14.5, 14.5), K = c(15, 15, 14.75, 14.5, 14.5, 14.5), L = c(14.5, 14.5, 14.5, 14.5, 14.5,  14.5), S = c(14.75, 14.5, 14.5, 14.5, 14.5, 14.5), sdif = c(-0.0833333333333339,
#' -0.333333333333334, -0.166666666666666, 0, 0, 0), vuur = structure(c(1L, 1L, 1L, 1L, 1L, 1L), .Label = c("no", "fire"), class = "factor"), date = c("2013-07-04 08:00:00", "2013-07-04 08:15:00", "2013-07-04 08:30:00",
#' "2013-07-04 08:45:00", "2013-07-04 09:00:00", "2013-07-04 09:15:00")), .Names = c("B", "K", "L", "S", "sdif", "vuur", "date"), row.names = c("2013-07-04 08:00:00", "2013-07-04 08:15:00",
#' "2013-07-04 08:30:00", "2013-07-04 08:45:00", "2013-07-04 09:00:00",  "2013-07-04 09:15:00"), class = "data.frame")
#' @export

zoomVuur <- function(x, mn = NULL, mx = NULL, hoof="",
                     timeZone = "Africa/Johannesburg", useOnlyNumeric = TRUE){
  require(manipulate)

  x$date <- as.POSIXct(x = x$date, tz = timeZone)

  if (useOnlyNumeric) {
    isNumeric <- apply(X = as.array(1:ncol(x)), MARGIN = 1, FUN = function(c) {
      return(is.numeric(x[[c]]))
    })
    colsToKeep <- c("date", "fire", names(x)[which(isNumeric)])
    x <- x[, colsToKeep]
  }

  xm = melt(x, id.vars = c("date", "fire"))
  xm$date = as.POSIXct(xm$date)
  xm$weekday <- factor(format(as.POSIXct(x$date), format = "%A"))

  if (is.null(mn)) {mn = as.POSIXct(x = min(xm$date, na.rm = TRUE),
                                    tz = timeZone,
                                    origin = "1970-01-01 00:00:00")}
  if (is.null(mx)) {mx = as.POSIXct(x = max(xm$date, na.rm = TRUE),
                                    tz = timeZone,
                                    origin = "1970-01-01 00:00:00")}

  manipulate(qplot(data = xm[which(xm$date >= mn & xm$date <= mx),],
                   x=date,
                   y = value,
                   group=variable,
                   #color=vuur,
                   facets = variable~.,
                   geom=gm,
                   main=hoof), # + geom_rect((aes(NULL, NULL, xmin=mn, xmax=mx))),

             gm = picker(list("line", "point"), initial = "line", label = "Kies geometrie"),

             mn = slider(min = as.numeric(min(xm$date)),
                         max = as.numeric(max(xm$date)),
                         initial = as.numeric(min(xm$date)),
                         step = 10,
                         label = "min.date"),

             mx = slider(min = as.numeric(min(xm$date)),
                         max = as.numeric(max(xm$date)),
                         initial = as.numeric(max(xm$date)),
                         step = 10,
                         label = "max.date")





#             mn = {
#                     init_jr <- as.character(min(year(xm$date), na.rm = TRUE))
#                     jare <- as.character(unique(year(xm$date)))
#                     message("jare:")
#                     print(jare)
#                     min_jr <- (as.character(picker(... = as.list(jare),
#                                     label = "begin jaar:")))
#
#                     min_mnd <- (as.character(picker(... = unique(as.numeric(month(xm$date))),
#                                      #initial = min(unique(integer(month(xm$date))), na.rm = TRUE),
#                                      label = "begin maand:")))[[1]]
#                     if (nchar(min_mnd) < 2) {min_mnd <- paste("0", min_mnd, sep = "")}
#
#                     min_dag <- (as.character(picker(... = ifelse(min_mnd %in% c(1,3,5,7,8,10,12), c(1:31),
#                                                        ifelse(min_mnd == 2,
#                                                           c(1:28),
#                                                           c(1:30))),
#                                                    #initial = "1",
#                                                    label = "begin dag")))[[1]]
#                     if (nchar(min_dag) < 2) {min_dag <- paste("0", min_dag, sep = "")}
#
#                     min_uur <- (as.character(picker(... = c(0:23),
#                                                    #initial = 0,
#                                                    label = "begin uur")))[[1]]
#                     if (nchar(min_uur) < 2) {min_uur <- paste("0", min_uur, sep = "")}
#
#                     min_datum <- paste(min_jr, min_mnd, min_dag, sep = "-")
#                     min_tyd <- paste(min_uur, ":00:00", sep = "")
#                     message("min_datum")
#                     print(min_datum)
#                     message("min_tyd")
#                     print(min_tyd)
#                     mn <- as.POSIXct(x = paste(min_datum, min_tyd, sep = " "), tz = timeZone)
#             },


#             mx = {
#                     max_jr <- as.integer(picker(... = unique(as.numeric(year(xm$date))),
#                                      #initial = max(unique(as.numeric(year(xm$date))), na.rm = TRUE),
#                                      label = "begin jaar:"))
#
#                     max_mnd <- as.character(picker(... = unique(as.numeric(month(xm$date))),
#                                      #initial = max(unique(integer(month(xm$date))), na.rm = TRUE),
#                                      label = "begin maand:"))
#                     if (nchar(max_mnd) < 2) {max_mnd <- paste("0", max_mnd, sep = "")}
#
#                     max_dag <- as.character(picker(... = ifelse(max_mnd %in% c(1,3,5,7,8,10,12), c(1:31),
#                                                        ifelse(max_mnd == 2,
#                                                           c(1:28),
#                                                           c(1:30))),
#                                                    #initial = "1",
#                                                    label = "begin dag"))
#                     if (nchar(max_dag) < 2) {max_dag <- paste("0", max_dag, sep = "")}
#
#                     max_uur <- as.character(picker(... = c(0:23),
#                                                    #initial = 0,
#                                                    label = "begin uur"))
#                     if (nchar(max_uur) < 2) {max_uur <- paste("0", max_uur, sep = "")}
#
#                     max_datum <- paste(max_jr, max_mnd, max_dag, sep = "-")
#                     max_tyd <- paste(max_uur, ":00:00", sep = "")
#                     mx <- as.POSIXct(x = paste(max_datum, max_tyd, sep = " "), tz = timeZone)
#                   }
      )

}


#' Zoomplot
#'
#' Generic Function
#'
#' @param m Data frame
#' @param varnames Character vector. Variable names
#' @param mn Date object. Refers to the minimum date
#' @param mx Date object. Refers to the maximum date
#' @param cll Character vector. Guide color label
#' @param hoof Character vector. The header of the plot. Does not have a header
#' by default
#' @param ... Arguments to be passed to/from calling function
#' @export

zoomPlot <- function(m,
                     varnames = NULL,
                     mn = NULL,
                     mx = NULL,
                     cll = NULL,
                     hoof="",...){

  m <- m[grep(varnames, m$variable) ,]
  require(manipulate)
  require(Hmisc)
  manipulate(qplot(data = m[which(m$date > mn & m$date < mx),],
                   x = date,
                   y = value,
                   group = variable,
                   color = factor(eval(parse(text = cll))),
                   facets = variable ~ .,
                   geom = gm,
                   main = hoof) + guides(color=guide_legend(title = capitalize(cll)))
             ,
             gm = picker(list("line", "point"), initial = "line", label = "Kies geometrie"),
             mn = slider(min = as.numeric(min(m$date)), max = as.numeric(max(m$date)), initial = as.numeric(min(m$date)), step = 10, label = "min.date"),
             mx = slider(min = as.numeric(min(m$date)), max = as.numeric(max(m$date)), initial = as.numeric(max(m$date)), step = 10, label = "max.date"))
}

