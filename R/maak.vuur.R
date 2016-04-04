#' Vuur Diagnoseer Plot
#'
#' Function used to create multiple plots of the PCA output from function maak.vuur.
#'
#' @param dpca List such as the PCA generated from maak.vuur
#' @param code Information regaring the zz environment generated in maak.vuur
#' @param sstf Character vector. Name of the column containing stove temperatures
#' @param debug Logical. Assignments for debugging process
#' @param grafdir Character vector. Directory to which graphs will be saved as .pdf
#' @param naam Character vector. Header for the temperature graphs
#' @export

 vuur.diagnoseer.plot <- function(dpca = get("dpca", envir = xx), code = zz$stt, sstf = "c",
                                  debug = TRUE, grafdir = "Grafieke/",
                                  naam = get("naam", environment(vuur.diagnoseer.plot))){
   message("Jy's nou binne in vuur.diagnoseer.plot")
   message(code, "\n")
   message("naam is ", naam)
   message("sstf is ", sstf)

   data = dpca$call$X
   data$vuur = eval(parse(text = code))
   data$date = as.character(rownames(data))
   dm = melt(data)
   dev.off()
   p1  <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=variable==sstf, geom="line", facets = variable~., group=variable, xlab = "date", ylab = "temperature", main = naam)
   p3 <- qplot(data=dm, x = vuur, y = value, geom = "boxplot", fill=variable)
   p2 <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=vuur, geom="line", group=variable, xlab = "date", ylab = "temperature")
   p4 <- qplot(data = dm[which(dm$variable==sstf),],
               x = as.integer(format(as.POSIXlt(dm$date[which(dm$variable==sstf)]), format = "%H")),
               y = value,  color=vuur, geom="jitter", group=variable, xlab = "hour", ylab = "temperature", alpha=I(1/7), size = I(0.6))

   #p1 geom smooth, geom path per dag , boxplot, smooth + temp x tod , col=vuur, alle sensors lyne faset = vuur
   multiplot(p1, p2, p3, p4 + geom_smooth(aes(color=vuur, group=vuur)), layout = matrix(c(1,1,2,2,3,4),nrow=3, byrow=TRUE))
   datateller = max(as.integer(grep("data", ls(envir = .GlobalEnv))))
   if (debug == TRUE) assign(paste("data", datateller,sep=""), data, envir = .GlobalEnv)
   if (debug == TRUE) dev.copy2pdf(file=paste(grafdir,"diagnostiese.vuurplot", naam,".pdf", sep=""))
   message("jy verlaat vuur.diagnoseer.plot")
 }

#' Strip function
#'
 #' @param x Coordinates of points in the plot. Alternatively, a single
 #' plotting structure, function or any R object with a plot method can be provided
 #' @param y The y coordinates of points in the plot, optional if x is an appropriate structure.
 #' @param tp Character vector. The type of plot. Defaults to a point plot. Types are
 #' "p" for points,
 #' "l" for lines,
 #' "b" for both,
 #' "c" for the lines part alone of "b",
 #' "o" for both overplotted,
 #' "h" for histogram like (or high-density) vertical lines,
 #' "s" for stair steps,
 #' "S" for other steps,
 #' "n" for no plotting.
 #' @param sdif Character vector. Name of the column containing difference between wall and chimney temp
 #' @param sdifval Numeric. Used in logical as minimum sdif
 #' @param pcadf Data frame. The PCA data frame
 #' @param cl Numeric. Contribution. To be used in the contribution slider
 #' @param clII Numeric. Cos2 contribution. To be used in the cos2 > slider
 #' @param mn Numeric. To be used for the month slider
 #' @param nm Character vector. Contribution dimension to be used in picker.
 #' @param nmII Character vector. Cos2 dimension to be used in picker.
 #' @param envr Environment under consideration
 #' @param qplot Logical. Create a qplot or not?
 #' @param verbose Logical. Display function messages or not?
 #' @param naam Character vector. Header for graphs
 #' @export

pl <- function(x = 1:nrow(dpca$ind$contrib), y = dpca$call$X[,sstf], tp = "p", sdif = "sdif", sdifval = 3,
               pcadf = cluscont, cl, clII, mn , nm , nmII ,
               envr = zz, qplot=TRUE, verbose = TRUE, naam = "naam") {
  code = sprintf("ifelse(%s$call$X[,%s] > %s & %s$ind$contrib[ ,'%s']  > %s & %s$ind$cos2[,'%s']  > %s & %s > %s , 'vuur', 'nie')",
                 deparse(substitute(pcadf)), deparse(substitute(sdif)), sdifval,
                 deparse(substitute(pcadf)), nm, cl,
                 deparse(substitute(pcadf)), nmII, clII,
                 "dpca$call$X[,'c']", mn)  # morsige hack !!!!!!!!!!!!!!!
  if (verbose == TRUE) cat(code)
  kex = ifelse(pcadf$call$X[ ,sdif] > sdifval & pcadf$ind$contrib[,nm]  > cl & pcadf$ind$cos2[,nmII] > clII & y > mn, "vuur", "nie")
  if (qplot != TRUE) {
    message("base")
    plot(x, y, type = tp, col=factor(kex),
       pch = 19, cex=0.25,
       main = assign("stt", code, envir = envr))
  }

  if (qplot == TRUE) {
    require(ggplot2)
    if (!exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", color = kex, main = assign("stt", code, envir = envr))
    if (exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", facets = week~ ., color = kex, main = assign("stt", code, envir = envr))
  }
}

#' Experimental Plotting Function
#'
#' Function is used to plot data output from the PCA
#'
#' @param x Coordinates of points in the plot. Alternatively, a single
#' plotting structure, function or any R object with a plot method can be provided
#' @param y The y coordinates of points in the plot, optional if x is an appropriate structure.
#' @param tp Character vector. The type of plot. Defaults to a point plot. Types are
#' "p" for points,
#' "l" for lines,
#' "b" for both,
#' "c" for the lines part alone of "b",
#' "o" for both overplotted,
#' "h" for histogram like (or high-density) vertical lines,
#' "s" for stair steps,
#' "S" for other steps,
#' "n" for no plotting.
#' @param sdif Character vector. Name of the column containing difference between wall and chimney temp
#' @param sdifval Numeric. Used in logical as minimum sdif
#' @param pcadf Data frame. The PCA data frame
#' @param cl Numeric. Contribution. To be used in the contribution slider
#' @param clII Numeric. Cos2 contribution. To be used in the cos2 > slider
#' @param mn Numeric. To be used for the month slider
#' @param nm Character vector. Contribution dimension to be used in picker.
#' @param nmII Character vector. Cos2 dimension to be used in picker.
#' @param envr Environment under consideration
#' @param qplot Logical. Create a qplot or not?
#' @param verbose Logical. Display function messages or not?
#' @param naam Character vector. Header for graphs
#' @export

pl.pca <- function(x = dpca$ind$coord[,1], y = ddpca$ind$coord[,2], tp = "p", sdif = "sdif", sdifval = 3,
               pcadf = cluscont, cl, clII, mn , nm , nmII ,
               envr = zz, qplot=TRUE, verbose = TRUE, naam = "naam") {
  code = sprintf("ifelse(%s$call$X[,%s] > %s & %s$ind$contrib[ ,'%s']  > %s & %s$ind$cos2[,'%s']  > %s & %s > %s , 'vuur', 'nie')",
                 deparse(substitute(pcadf)), deparse(substitute(sdif)), sdifval,
                 deparse(substitute(pcadf)), nm, cl,
                 deparse(substitute(pcadf)), nmII, clII,
                 "dpca$call$X[,'c']", mn)  # morsige hack !!!!!!!!!!!!!!!
  if (verbose == TRUE) cat(code)
  kex = ifelse(pcadf$call$X[ ,sdif] > sdifval & pcadf$ind$contrib[,nm]  > cl & pcadf$ind$cos2[,nmII] > clII & y > mn, "vuur", "nie")
  if (qplot != TRUE) {
    message("base")
    plot(x, y, type = tp, col=factor(kex),
         pch = 19, cex=0.25,
         main = assign("stt", code, envir = envr))
  }

  if (qplot == TRUE) {
    require(ggplot2)
    if (!exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", color = kex, main = assign("stt", code, envir = envr))
    if (exists("week", envir = environment(pl))) qplot(x = x, y = y, geom = "smooth", facets = week~ ., color = kex, main = assign("stt", code, envir = envr))
  }
}

#' Maak Vuur
#'
#' Main function that receives a data frame containing fire and temperature data and produces multiple plots
#' that describes the data. Also makes use of the vuur.diagnoseer.plot function.
#'
#' @param df Data frame. Contains fire and temperature data, normally the iButton data
#' @param aggr Logical. Initialises date aggregation when TRUE
#' @param drops Character vector. Names of the columns to be dropped
#' @param datum.naam Character vector. Name of the column containing dates
#' @param skalleer Logical. If TRUE then data are scaled to unit variance
#' @param stoofnaam Character vector. Name of the column containing chimney temperatures
#' @param verbose Logical. Display function messages
#' @param debug Logical. Assign values to global environment for debugging purposes
#' @param tpp Character vector. The plot type
#' @param qp Logical. Make qplot or not
#' @details Use this function in the form df = maak.vuur(df)
#' or res = lapply(kortLYS, maak.vuur, aggr = TRUE, verbose=T, qp = F)
#' @export

maak.vuur <- function(df, aggr = FALSE,
                      drops = c("fire","E"),
                      datum.naam = "date",
                      skalleer = TRUE,
                      stoofnaam = "c",
                      verbose = FALSE, debug = FALSE,
                      tpp = "p",
                      qp = FALSE){
  if(suppressMessages(require(FactoMineR))) {if (verbose ==TRUE) {cat("FactoMineR gelaai ")}} else {install.packages("FactoMineR")}
  if(suppressMessages(require(ggplot2))) {if (verbose ==TRUE) {cat("ggplot gelaai ")}} else {install.packages("ggplot2")}
  if(require(data.table)) {if (verbose ==TRUE) {cat("data.table gelaai ")}} else {install.packages("ata.table")}
  if(require(openair)) {if (verbose ==TRUE) {cat("openair gelaai ")}} else {install.packages("openair")}
  if(require(manipulate)) {if (verbose ==TRUE) {cat("manipulate gelaai ")}} else {install.packages("manipulate")}

  if (any(!is.na(match(drops, names(df))))) df = df[ ,-na.omit(match(drops, names(df)))]
  if (verbose == TRUE) message("Drops gedoen ")
  if (aggr == TRUE) {
    week = as.integer(format(df[ ,match(datum.naam, names(df))], "%W")) # oorweeg om as.POSIXlt te gebruik
    xvals = as.integer(format(df[ ,match(datum.naam, names(df))], "%H"))
    if (verbose == TRUE) message("xvals is ", paste(head(xvals), " "), "\n week is ", paste(names(table(week)), " "))
    } else {xvals = 1:nrow(df)}

  naam = ifelse(!is.na(match("naam", names(df))), unique(df$naam), "naam")
  if (!is.na(match("naam", names(df)) ==FALSE)) df = df[ ,-grep("naam", names(df))]
  rownames(df) <- df[,datum.naam]
  df[,datum.naam] = NULL # nou is die rynaam die datum en die datum kolom is weg (vereis deur PCA)
  names(df) <- gsub('[[:digit:]]+_+', "",names(df))
  if (verbose == TRUE) message(paste(names(df), " "))
  #dff <- unlist(df[,stoofnaam]) - unlist(apply(df[-match(stoofnaam, names(df))], 1, mean))
  #df[,"sdif"] = dff
  if (verbose == TRUE) message(paste(names(df), " "))
  dpca <- PCA(df[, c("w", "c", "sdif")], scale.unit=skalleer)
  if (verbose == TRUE) message(paste(names(df), " "), "\nHoofkomponent-analise voltooi")
  ig = readline(prompt=cat("Kyk na die dimensies. Tik iets hier as jy klaar is"))
  if (exists("zz")) rm(zz)
  zz = new.env()
  xx = new.env()
  assign("dpca", dpca, envir = xx)
  if (verbose == TRUE) message("zz geskep", ls(zz))
  if (verbose == TRUE) message("xx geskep", ls(xx))
  klaar = FALSE

  while(klaar == FALSE) {
     manipulate(pl(x = xvals, y = df[,stoofnaam], naam = naam, tp = tpp, pcadf = dpca, cl=cll, clII = cllII, mn = mnn, nm = nnm, nmII = nnmm, envr = zz, qplot = qp, sdifval = ssdifval),
                ssdifval = slider(label = "Stoof diff", 0,10, step=0.5, initial = 3),
                cll = slider(label = "bydrae tot", 0, 1, step=0.001, initial = 0.0375),
                mnn = slider(0,30, step = 0.5, initial = 15),
                cllII = slider(label = "cos2 > ", 0, 1, step=0.005, initial = 0.135),
                nnm = picker(label = "Bydrae dimensie", "Dim.1", "Dim.2", "Dim.3", "Dim.4", initial="Dim.2"),
                nnmm = picker(label = "cos2 dimensie", "Dim.1", "Dim.2", "Dim.3", "Dim.4", initial="Dim.2")
                  )

  ig = readline(prompt=cat("Tik iets hier om te kyk hoe dit werk"))
  # plot hier
  vuur.diagnoseer.plot(dpca, zz$stt, sstf = stoofnaam, naam = naam)
  #
  kl = readline(prompt = cat("Tik JA as jy teverede is, anders enigiets anders"))
  if (kl == "JA") klaar = TRUE
  }

  if (verbose == TRUE) message("zz$st is: " , zz$stt)
  kolom = eval(parse(text = zz$stt))
  df[,"vuur"] = kolom
  if (verbose == TRUE) message("zz$stt geskryf")
  df$date <- rownames(df)
  if (verbose == TRUE) message(paste(table(df$vuur), " "))
  if (debug == TRUE) assign("df", df, envir= .GlobalEnv)
  dm = melt(df)
  if (verbose == TRUE) message("df is: ", dim(df), "\n en dm is ", dim(dm))
  if (verbose == TRUE) message("ons stip nou " , paste(names(dm), " "), "\n", paste(table(dm$variable), " "), "\n", paste(table(dm$value), " "))
  qplot(data=dm, x=date, y=value, geom="point", size = I(0.7), main="", color = vuur) + facet_grid(facets = variable~.)
  if (verbose == TRUE) message(paste(ls(), " "))
  if (verbose == TRUE) message(paste(str(df), " "))
  return(df)
}

#' Vuur Publikasie Plot
#'
#' Creates multiple plots relating to chimney, wall and fire temperatures as well as plotting whether
#' fires were made or not throughout the day
#'
#' @param x Data frame. Fire data such as the data output from the maak.vuur function
#' @param vuurnaam Character vector. Name of the column indicating whether fire is active or not
#' @param sdif Character vector. Name of the column containing the difference in stove and wall temperatures
#' @param sstf Character vector. Name of the column containing chimney temperatures
#' @param naam Character vector. Name that will appear in the header of the graphs
#' @param grafdir Character vector. Directory that the graph will be saved in as a .pdf. The default
#' directory will be inside a Grafieke folder in your working directory
#' @param verbose Logical. Display function messages
#' @param save Logical. Saves the graphs in pdf format inside grafdir when TRUE
#' @param DEBUG Logical. Assigns values for debugging purposes
#' @export

vuur.publikasie.plot <- function(x, vuurnaam = "vuur", sdif = "sdif",
                                 sstf = "c", naam = "", grafdir = "Grafieke/",  verbose = FALSE,
                                 save = TRUE, DEBUG = FALSE){

  stopifnot(class(x)=="data.frame")
  require(reshape2)
  require(ggplot2)

  naam = gsub("Huis", "House ", naam)
  names(x)[match(vuurnaam,names(x))] <- "vuur"
  if (verbose == TRUE) {names(x)}

  idxx <- which(!is.na(x$vuur))
  if (length(idxx) > 0) {
    x <- x[idxx,]
  }
  else {
    warning("Ons het geen idee of hierdie ou vuur gemaak het of nie...Kan geen grafieke maak nie.")
    return(NULL)
  }

  dm = melt(x, id.vars = c("date", "vuur"))
  dm$value <- as.numeric(dm$value)

  p1  <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color = variable, geom="line", facets = variable~.,
               group = variable, xlab = "date", ylab = "temperature", main = naam)
  p3 <- qplot(data=dm, x = vuur, y = value, geom = "boxplot", fill=variable, xlab = "fire on?", ylab="temperature")
  p2 <- qplot(data = dm, x = as.POSIXlt(date), y = value,  color=vuur, geom="line", group=variable,
              xlab = "date", ylab = "temperature", main = naam)
  p4 <- qplot(data = dm[which(dm$variable==sstf),],
              x = as.integer(format(as.POSIXlt(dm$date[which(dm$variable==sstf)]), format = "%H")),
              y = value,  color=vuur, geom="jitter", group=variable, xlab = "hour", ylab = "temperature", alpha=I(1/2), size = I(0.7))
  p4 <- p4 + geom_smooth(aes(color=vuur, group=vuur))
  p5 <- qplot(data= dm[which(dm$variable==sstf),], x = factor(format(as.POSIXlt(date), format = "%H")), fill=vuur, xlab = "hour")

  dc <- dcast(dm, formula = date + vuur ~ variable, fun.aggregate = mean)
  if (DEBUG) {assign(x = "dc", value = dc,envir = .GlobalEnv)}
  dx = names(dc[which(sapply(dc, class)=="numeric")])
  dx = dx[-na.omit(match(c(sstf,"sdif"), dx))]
  if (DEBUG) {assign(x = "dx", value = dx)}
  dc$coldest.room = dc[,grep(names(which.min(sapply(dc[dx], mean, na.rm = TRUE))), names(dc))]

  idx <- match(x = sstf, table = names(dc))
  if (!is.na(idx)) {
    names(dc)[[idx]] <- "sstf"
  }
  p6 <- qplot(data = dc , x = coldest.room, y = sstf, geom="jitter", color = vuur, alpha = I(0.5), size = I(0.7))

  #p1 geom smooth, geom path per dag , boxplot, smooth + temp x tod , col=vuur, alle sensors lyne faset = vuur
  multiplot(p1, p2, p3, p4 , p5, p6, layout = matrix(c(1,1,2,3,4,5),nrow=3, byrow=TRUE))
  if (save == TRUE) {dev.copy2pdf(file=paste(grafdir,"vuurplot", naam,".pdf", sep=""), paper = "a4r", width = 11.69, height = 8.27)}
}

#' Multiplot
#'
#' Plot multiple graphs on one page
#'
#' @references  Winston Chang: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' Multiple plot function
#' @param ... Arguments to be passed to/from other methods
#' @param plotlist A list of ggplot objects
#' @param file Filename (Not present in this function)
#' @param cols Numeric. Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored
#' @details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @export

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      if (nrow(matchidx) > 0) {
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
}

