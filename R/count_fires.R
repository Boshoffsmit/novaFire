#' Count Fires Per Day
#'
#' Creates a summary of the mean fires per day for certain months as well as fire proportions for
#' each day
#'
#' @param df Data frame. Contains fire data
#' @param datevar Character vector. Name of the column containing dates
#' @param firevar Character vector. Name of the column specifying whether a fire is active or not
#' @param vuurnaam Character vector. Text that correspond to a fire being active
#' @param groupvar Character vector. An optional grouping variable
#' @param export

count.fires.per.day <- function(df = NULL,
                        datevar = "date",
                        firevar = "vuur",
                        vuurnaam = "vuur",
                        groupvar = NULL){

   if (require(lubridate) == FALSE){
                install.packages("lubridate", dependencies = TRUE)
                message("Ek probeer lubridate laai. Hou bietjie aan ...")
   }

  if (is.null(df)) {
    warning("Received argument 'df' is NULL. Returning NULL.")
    return(NULL)
  }

  if (is.na(match(firevar, names(df)))) {
    warning(paste("Variable '", firevar, "' not found. Returning NULL", sep = ""))
    return(NULL)
  }

  if (!is.factor(df[, firevar])) {
    warning("Fire variable not a factor. Attempting to coerce...")
    catch <- plyr::tryNULL(as.factor(df[, firevar]))
    if (is.null(catch)) {
      warning("Failed to coerce fire variable to a factor. Returning NULL.")
      return(NULL)
    } else {  df[, firevar] <- catch  }
  }

  if (nlevels(df[ ,firevar]) != 2) {
    warning("Fire variable must have two (and only two) factor levels. Returning NULL.")
    return(NULL)
  }

  if (is.na(match(vuurnaam, levels(df[ ,firevar])))) {
    warning(paste("'", vuurnaam, "' is not one of the levels in '", firevar, "'. Returning NULL.", sep = ""))
    return(NULL)
  }

  if (is.na(match(datevar, names(df)))) {
    warning(paste(datevar, " not found in argument 'df'. Returning 'NULL.'.", sep = ""))
    return(NULL)
  }



  if (!is.POSIXt(df[,datevar])) {
    warning("The date variable in the received data set is not in POSIXt format. Coercing to POSIXct...")
    catch <- plyr::tryNULL(expr = as.POSIXct(df[, datevar]))
    if (is.null(catch)) {
      warning("Failed to coerce the date variable to POSIXct. Returning NULL.")
      return(NULL)
    } else {
      df[,datevar]}
  }

  if (!is.null(groupvar)) {
    if (is.na(match(x = groupvar, table = names(df)))) {
      warning(paste("'", groupvar, "' (grouping variable) not found in the given data set. Ignoring grouping command.", sep = ""))
      groupvar = NULL
    }
  }

  if (require(dplyr) == FALSE){
    install.packages("dplyr", dependencies = TRUE)
    message("Ek probeer dplyr laai. Hou bietjie aan ...")
  }

  if (require(tidyr) == FALSE){
    install.packages("tidyr", dependencies = TRUE)
    message("Ek probeer tidyr laai. Hou bietjie aan ...")
  }

  data <- df

  data$yrday <- yday(data[,datevar])
  data$mo <- month(data[,datevar], label = TRUE)
  data$yr <- year(data[,datevar])

  names(data)[match(firevar, table = names(data))] = "fire"

  if (!is.null(groupvar)) {
    idx <- match(x = groupvar, table = names(data))
    names(data)[idx] <- "gRpVr"

    res_raw = data %>%
    group_by(yrday, mo, yr, gRpVr) %>%
      mutate(fire = as.character(fire)) %>%
      dplyr::summarise(fires = table(rle(fire)$values)[vuurnaam],
                       fires_prop = table(fire)[vuurnaam]/sum(table(fire))
                      )

    res_sum = res_raw %>% group_by(gRpVr, yr, mo) %>%
      dplyr::summarise(mean_fires_per_day = mean(fires, na.rm = TRUE))

    names(res_raw)[match("gRpVr", table = names(res_raw))] <- groupvar
    names(res_sum)[match("gRpVr", table = names(res_sum))] <- groupvar

  } else {
      res_raw = data %>%
      group_by(yrday, mo, yr) %>%
      dplyr::summarise(fires = table(rle(as.character(fire))$values)[vuurnaam],
                fires_prop = table(fire)[vuurnaam]/sum(table(fire)))

      res_sum = res_raw %>% group_by(yr, mo) %>%
        dplyr::summarise(mean_fires_per_day = mean(fires, na.rm = TRUE))
    }

  if (nlevels(as.factor(res_raw$yr)) > 1) {res_raw = res_raw %>% spread(yr, fires)}
  if (nlevels(as.factor(res_sum$yr)) > 1) { res_sum = res_sum %>% spread(yr, mean_fires_per_day) }

  return(list("res_sum" = res_sum, "res_raw" = res_raw))
}

#' Count Fires Per Day List
#'
#' Uses the count.fire.per.day function while receiving a list. Function returns raw counts instead
#' of summarised ones
#'
#' @param ldata List containing fire information
#' @param returnAsSingleDataFrame Logical. Returns the results as is if TRUE
#' @param ... Arguments to be passed to/from other methods
#' @export

count.fires.per.day.list <- function(ldata = NULL, returnAsSingleDataFrame = FALSE, ...){

  # banana proofing
  if (is.null(ldata)) {
    warning("No list received. Returning.")
    return(ldata)
  }

  if (!is.list(ldata)) {
    warning("Received argument is not a list. Returning.")
    return(ldata)
  }

  # count the fires (and return the raw counts, not the summarised counts)
  ll = lapply(X = ldata, FUN = function(x) {
    results <- count.fires.per.day(df = x, ...)
    return(results[["res_raw"]])
  })

  # if the results should just be returned as is, return
  if (!returnAsSingleDataFrame) {
    return(ll)}

  # otherwise, combine to a data frame and return
  ## put the id back to each list element
  for (i in 1:length(names(ldata))) {
    df <- ll[[i]]
    df$id <- names(ldata)[[i]]
    nms <- names(df)[-match("id", names(df))]
    df <- df[, c("id", nms)]
    ll[[i]] <- df
  }

  ## now bind them together to a df
  df_ll <- do.call(what = "rbind", args = ll)
  rownames(df_ll) <- NULL
  rm(ll)

  ## and return
  return(df_ll)
}

#' Summarise Fires Per Day List
#'
#' Uses the count.fire.per.day function while receiving a list. Function returns summarised counts
#'
#'  @param lys List containing fire information
#'  @param groupvar Character vector. Grouping variable that defaults to "year"
#'  @param ... Arguments to be passed to/from other methods
#'  @export

summarise.fires.per.day.list <- function(lys, groupvar = "yr", ...){

  # banana proofing
  if (is.null(ldata)) {
    warning("No list received. Returning.")
    return(ldata)
  }

  if (!is.list(ldata)) {
    warning("Received argument is not a list. Returning.")
    return(ldata)
  }

  # count the fires and return the summarised counts)
  ll = lapply(X = ldata, FUN = function(x) {
    results <- count.fires.per.day(df = x, ...)
    return(results[["res_sum"]])
  })

  # if the results should just be returned as is, return
  if (!returnAsSingleDataFrame) {
    return(ll)}

  # otherwise, combine to a data frame and return
  ## put the id back to each list element
  for (i in 1:length(names(ldata))) {
    df <- ll[[i]]
    df$id <- names(ldata)[[i]]
    nms <- names(df)[-match("id", names(df))]
    df <- df[, c("id", nms)]
    ll[[i]] <- df
  }

  ## now bind them together to a df
  df_ll <- do.call(what = "rbind", args = ll)
  rownames(df_ll) <- NULL
  rm(ll)

  ## and return
  return(df_ll)
}


# # -------------------------------------- #
# summarise.fires.per.day <- function(df, groupvar = "yr", ...){
#
#   dfl = count.fires.per.day(df = df, ...)
#
#   idx <- match(x = groupvar, table = names(dfl))
#   if (is.na(idx)) {
#     warning(paste("'", groupvar, "' (grouping variable) not found in the given data set. Returning unsummarised results.", sep = ""))
#     return(dfl)
#   }
#   names(dfl)[[idx]] <- "gRpVr"
#
#   res = dfl %>% group_by(gRpVr) %>% dplyr::summarise(mean_fires_per_group = mean(mean_fires_per_day, na.rm = TRUE))
#
#   idx <- match(x = "gRpVr", table = names(res))
#   if (!is.na(idx)) {names(res)[[idx]] <- groupvar}
#
#   return(res)
# }

