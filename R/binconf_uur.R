#' Binary Configuration Hour
#'
#' Function receives a data frame and creates a graph based on the
#' proportion of fires active by hour. Also gives xlsx output of prop, upper and lower. Makes use
#' of Monte Carlo Simulation
#'
#' @param x Data frame that contains fire data
#' @param datumvar Character vector. Name of the column containing dates
#' @param vuurvar Character vector. Name of the column containing fires being active or not
#' @param vuurnaam Character vector. Is the fire active? Either "fire" or "yes".
#' @param plot Character vector. Plot nothing (NULL), barplot (bar), or lineplot (line)
#' @param stoor Logical. Store the data?
#' @param xlsx Logical. Print out xlsx or not.
#' @param stoordir Character vector. The directory for storage to take place in
#' @param fn Character vector. Filename of stored data
#' @param ttl Character vector. Title of the generated graph
#' @param addttl Character vector. Add a title
#' @param addfn Character vector. Add a filename
#' @param ylm Numeric vector. Contains y axis limits
#' @param verbose Logical. Displays messages
#' @param ylable Character vector. The y-axis lable
#' @param uurvar Character vector. Name of the hours variable. If null the function will
#' extract the hour of day from the data contained in the date column (recommended)
#' @param maandvar Character vector. Name of the month variable
#' @param jaarvar Character vector. Name of the year variable
#' @export

binconf_uur <- function(x = stowe.df,
                        datumvar = "datum",
                        vuurvar = "vuur",
                        vuurnaam = "fire",
                        plot = c(NULL, "bar", "line"),
                        stoor = FALSE,
                        xlsx = TRUE,
                        stoordir = NULL,
                        fn = "binconf.uur.pdf",
                        ttl = "Proportion of fires active\n by hour of day (with 95% CI)",
                        addttl = NULL,
                        addfn = NULL,
                        ylm = c(0, 1),
                        verbose = TRUE,
                        ylable = "Proportion of fires active",
                        uurvar = "hour",
                        maandvar = "month",
                        jaarvar = "year"){

  # check prerequisites...
  if (!require(binom))install.packages("binom")
  if (!require(reshape2)) install.packages("reshape2")
  if (!require(ggplot2)) install.packages("ggplot2")
  if (!require(scales)) install.packages("scales")
  if (!require(lubridate)) install.packages("lubridate")

  # banana proofing...
  if (!(vuurvar %in% names(x))) {
    warning(paste("Column '", vuurvar, "' not found in received data frame. Returning NULL.", sep = ""))
    return(NULL)
  }

  if (!(vuurnaam %in% x[, vuurvar])) {
    warning(paste("'", vuurnaam, "' not found in column '", vuurvar, "'. Returning NULL.", sep = ""))
    return(NULL)
  }

  if ((is.null(uurvar) | is.null(maandvar) | is.null(jaarvar)) & is.null(datumvar)) {
    stop("Jy moet of uurvar, maandvar en jaarvar eksplisiet spesifiseer, of jy moet vir my sê wat is die naam van die datumvar sodat ek daardie velde self kan skep.")
  }

  if ((is.null(uurvar) | is.null(maandvar) | is.null(jaarvar)) & (!(datumvar %in% names(x)))) {
    stop("datumvar is nie in x nie. Is jy seker die naam van datumvar is reg?")
  }

  if (is.null(uurvar)) {
    x$hod <- hour(x[[datumvar]])
    uurvar <- "hod"
  }
  if (is.null(maandvar)) {
    x$mo <- month(x[[datumvar]])
    maandvar <- "mo"
  }
  if (is.null(jaarvar)) {
    x$yr <- year(x[[datumvar]])
    jaarvar <- "yr"
  }

  tt <- data.frame(rbind(addmargins(table(x[ ,uurvar], x[ ,vuurvar]), margin = 2, FUN = sum)))
  tt$lower <- binom.confint(x = tt[,vuurnaam], n = tt$sum, methods = "exact")$lower
  tt$upper <- binom.confint(x = tt[,vuurnaam], n = tt$sum, methods = "exact")$upper
  tt$prop <- tt[,vuurnaam]/tt$sum
  tt$hod <- as.integer(rownames(tt))

  if (!is.null(plot)) {
    tttl <- if (!is.null(addttl)) {
      tttl <- paste(ttl, levels(as.factor(as.character(x[ ,addttl]))))
    } else {tttl <- ttl}
    if (verbose == TRUE) {message(tttl)}
  }

  if (plot == "bar"){

    p1 <- ggplot(data = tt,
                 mapping = aes(x = hod, y = prop)) +
      geom_bar(stat = "identity", fill = I("firebrick")) +
      geom_errorbar(ymin = tt$lower, ymax = tt$upper) +
      scale_y_continuous(labels = percent_format()) +
      xlab("Hour of day") +
      ylab(ylable) +
      ylim(ylm) +
      ggtitle(tttl) # + geom_smooth(aes(x = Var1, y = value, group=1))
  }

  if (plot == "line"){
    tt <- dcast(data.frame(addmargins(table(factor(x[ ,jaarvar]),
                                            x[, maandvar],
                                            x[ ,uurvar],
                                            x[,vuurvar]), 4)), Var1 + Var2 + Var3 ~ Var4)
    if (verbose == TRUE) message(str(tt))
    tt$lower <- binom.confint(x = tt[ ,vuurnaam], n = tt$Sum, methods = "exact")$lower
    tt$upper <- binom.confint(x = tt[ ,vuurnaam], n = tt$Sum, methods = "exact")$upper
    tt$prop = tt[ ,vuurnaam]/tt$Sum
    names(tt)[1:3] <- c("year", "month", "hour")
    tm = melt(tt, id.vars = c("year", "month", "hour"), measure.vars = c("lower","upper", "prop"))
    p1 <- qplot(data = tt, x= hour, geom = "line", y = prop, group = year, color = year) +
      geom_ribbon(aes(x = hour, ymin=lower, ymax = upper), alpha = 1/4) +
      scale_y_continuous(labels=percent_format()) +
      xlab("Hour of day") +
      ylab("Proportion fires active") +
      ylim(ylm) + facet_grid(month ~ .) +
      ggtitle(tttl)
  }

  if (stoor == TRUE) {
    if (is.null(stoordir)) {
      stoordir <- getwd()
      message("Saving to ", stoordir, "...")
    }
    fffn <- if (!is.null(addfn)){
      fffn <- paste(stoordir, gsub(".pdf", "", fn), levels(as.factor(as.character(x[ ,addfn]))), ".pdf", sep="")
    } else {fffn = fn}

    if (verbose == TRUE) {message(fffn, "\n")}
    ggsave(p1, filename = fffn, width = 14)
  }

  if (!is.null(plot)) {plot(p1)}

  if (xlsx == TRUE){
    if (require(openxlsx) == FALSE) {
      install.packages("openxlsx", dependencies = TRUE)
      library(openxlsx)
    }
    if (is.null(stoordir)) {
      stoordir <- getwd()
      message("Saving to ", stoordir, "...")
    }
    write.xlsx(tt, file = paste(stoordir, gsub("pdf", "xlsx", fn), sep = ""))
  }

  return(list(df = tt, plt = p1))
}

#' Kg Hour
#'
#' Uses output of binconf.uur and obtains kg per hour while also plotting a graph using Monte Carlo
#'
#' @param x Data frame such as the tt data frame generated in binconf.uur
#' @param propvar Character vector. Name of the proportion column
#' @param fuelprop Numeric. The fuel proportion
#' @param nnn Numeric vector. The number of timesteps for use in monte carlo simulation
#' @param mcspecfn Character Vector. Filename for MC simulation of specific variables
#' @param mcspecfn.add Character vector. Additional extention to filename mcspecfn
#' @param mcsimfn Character Vector. Filename for MC simulation of coal burned per hour of day
#' @param mcsimfn.add Character vector. Additional extention to filename mcsimfn
#' @param yl Numeric. Y axis limit
#' @export

kg.uur <- function(x = tt, propvar = "prop", fuelprop = 0.65, nnn = c(200, 14),
                   mcspecfn = "~/tmp/MC_estimation_kg_coal_hod_MC_vars", mcspecfn.add = NULL,
                   mcsimfn = "~/tmp/MC_estimation_kg_coal_hod", mcsimfn.add = NULL, yl = 1000){
  #message(ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] )
  #stopifnot("mc.sim" %in% ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] , " jy benodig mc.sim")
  #stopifnot("generateMCSample" %in% ls()[sapply(ls(envir = .GlobalEnv), function(x) is.function(get(x)))] , " jy benodig generateMCSample")
  #ll = list(A= list(var = "pop", dist = "unif", params = c(min=1000, max = 1100)), B = list(var = "consumption", dist = "weibull", params = c(shape=2.669757, scale= 4.8/3)))
  require(reshape2)
  require(ggplot2)

  if (!is.null(mcspecfn)) {mcspecfn = paste(mcspecfn, mcspecfn.add, ".pdf", sep="")}

  if (!is.null(mcsimfn)) {mcsimfn = paste(mcsimfn, mcsimfn.add, ".pdf", sep="")}

  uurvuur <- x[ ,propvar]
  l = ll
  rs =  lapply(uurvuur, function(x) {
    mc.sim(l = l, prop=c(fuelprop, x), nn = nnn, plot = TRUE)})
  message(paste(str(rs)," "))
  rsm=melt(rs)
  dev.off()  # modeleer verskil tussen Julie en sept
  #qplot(data=rsm, x=L1, y=value, geom="boxplot", group=L1)
  qplot(data=rsm, x=L1, y=value, geom="jitter", group=L1, alpha=I(1/10)) +
    geom_smooth(aes(group=1), size = I(2)) +
    labs(x = "hour of day", y = "kg coal per hour",
         title = "Estimation of coal burned per hour of day for Kwadela")
  ggsave(filename = mcsimfn)

  plot(sapply(rs, mean), ylim=c(0, yl), type = "n", xlab = "Hour of day", ylab = "kg. coal per household")
  lines(sapply(rs, mean))
  lines(sapply(rs, function(x) mean(x) - sd(x) ), lty = 2)
  lines(sapply(rs, function(x) mean(x) + sd(x) ), lty = 2)


  res = do.call("rbind", (lapply(rs, summary)))
  res
}

# nou moet ons die kg steenkool per uur regkry: kom ons begin met 'n emertjie oor 3 uur : 5/3
