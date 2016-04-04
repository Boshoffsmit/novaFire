#' Skoonmaak Vlamme
#'
#' Function is used to remove the cleaning flame temperatures at the beginning or end of
#' certain fires. It does this by testing if there are any unreasonably high temperatures
#' in the first or last 10 minutes of a fire episode
#'
#' @param df Data frame. Contains fire data
#' @param fireField Character vector. Name of the fire column
#' @param fireLabel Character vector. Name indicating active fire
#' @param noFireLabel Character vector. Name indicating inactive fire
#' @param chimneyField Character vector. Column containing chimney values
#' @param dateField Character vector. Name of the date column
#' @param timeStep Numeric. The number of minutes between
#' two consecutive records in the received df
#' @param verbose Logical. Display function messages?
#' @details THIS FUNCTION ASSUMES THAT THE ARGUMENT RECEIVED BY DF ARE SORTED
#' ACCORDING TO DATE/TIME - ASCENDINGLY
#' @export

skoonmaakvlamme <- function(df = NULL,
                            fireField = "fire",
                            fireLabel = "fire",
                            noFireLabel = "no",
                            chimneyField = "C",
                            dateField = "date",
                            timeStep = 20, verbose = FALSE) {

  if (!require(knitr))install.packages("knitr")
  # bananananana
  if (is.null(df)) {
    warning("No argument received for parameter 'df'. Returning.")
    return(NULL)
  }

  if (!(fireField %in% names(df))) {
    warning("fireField argument not found in names of df argument. Returning df.")
    return(df)
  }

  if (!(fireLabel %in% df[[fireField]]) & !(noFireLabel %in% df[[fireField]])) {
    warning("Neither fireLabel nor noFireLabel found in fireField. I have their names as '", fireLabel, "' and '", noFireLabel, "'. Is that correct? Returning df.")
    return(df)
  }

  if (!(fireLabel %in% df[[fireField]])) {
    if (verbose) {message("No fire episodes found in the argument received by df. Returing df.")}
  }

  if (!(chimneyField %in% names(df))) {
    warning("chimneyField not found in names(df). Returning df.")
    return(df)
  }
  ## --------------------- ##
  # initialise
  df[[fireField]] <- as.character(df[[fireField]])
  fireLabel <- as.character(fireLabel)
  noFireLabel <- as.character(noFireLabel)
  df$fire_ep_idx <- NA_integer_

  ## --------------------- ##
  # label all fire records with the index of the fire episode to which they belong
  fireEp <- 0L

  # initialise the first cell of the fire_ep_idx field
  if (!(is.na(df[1, fireField]))) {
    if (df[1, fireField] == fireLabel) {
      fireEp <- fireEp + 1
      df[1, "fire_ep_idx"] <- fireEp
    }
  }

  for (r in 2:nrow(df)) {
    if (is.na(df[r, fireField])) {next} # it is not part of any fire episode, so just move on
    if (df[r, fireField] == noFireLabel) {next} # it is not part of a fire episode, so just move on
    if (df[r, fireField] == fireLabel) { # it is part of a fire episode, but is it the start thereof or not?
      if (is.na(df[r-1, fireField])) { # it must be the start of a new fire episode, so increase fireEp, set it = to fireEp and move on
        fireEp <- fireEp + 1
        df[r, "fire_ep_idx"] <- fireEp
        next
      }
      if (df[r-1, fireField] == noFireLabel) { # it must be the start of a new fire episode, so increase fireEp, set it = to fireEp and move on
        fireEp <- fireEp + 1
        df[r, "fire_ep_idx"] <- fireEp
        next
      }
      if (df[r-1, fireField] == fireLabel) { # this record does not mark the start of a new fire ep; it is simply a continuation of an already running fire ep, so just assign the fire episode's index to this record and move on
        df[r, "fire_ep_idx"] <- fireEp
        next
      }
    }
  }

  ## --------------------- ##
  # labelling done. now determine the distribution of the max temperatures among the fire episodes
  fireEpSplits <- split(x = df, f = df$fire_ep_idx, drop = FALSE)

  # i.v.m. '2:(nrow(fe)-1)' - wat as fe net 2 rye het...?
  maxtemps <- sapply(X = fireEpSplits, FUN = function(fe) {
    if (all(is.na(fe$fire_ep_idx))) {return(NA_real_)}
    return(max(fe[2:(nrow(fe)-1), chimneyField], na.rm = TRUE))
  }) # starting only from index two and ending at the second-last in order to exclude the temperatures of the cleaning flames at the start or end of a fire ep

  if (verbose) {
    message("The distribution of the max temperatures among the identified fire episodes are as follows:")
    print(kable(as.data.frame.AsIs(summary(maxtemps))))
  }

  # determine the distribution of the fire-start temperatures
  starttemps <- sapply(X = fireEpSplits, FUN = function(fe) {
    if (all(is.na(fe$fire_ep_idx))) {return(NA_real_)}
    return(fe[2, chimneyField])
  }) # starting only from index two in order to exclude the temperatures of the cleaning flames at the start of a fire ep

  if (verbose) {
    message("The distribution of the fire-start chimney temperatures among the identified fire episodes are as follows:")
          print(kable(as.data.frame.AsIs(summary(starttemps))))
  }
}
