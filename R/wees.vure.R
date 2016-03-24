#' Wees Vure
#'
#' Function analyses fire episodes and isolates those that are seen as too short
#' (specified by "wees" numeric), whereby it replaces the episodes with longer ones
#'
#' @param x Data frame. Fire data
#' @param vuurvar Character vector. Name of the fire column
#' @param verbose Logical. Display function messages
#' @param wees Numeric. The minimum duration allowed for a fire episode - given as the
#' number of records/time steps across which it should span
#' @export

wees.vure <- function(x, vuurvar = "fire", verbose = FALSE, wees = 2){
  z <- rle(as.character(x[,vuurvar]))

  if (length(z) >= 2){
    zz  <- data.frame(lengths = z[[1]], values = z[[2]])
    meerindeks <- lapply(which(zz[,1] <= wees), function(xx) {(sum(zz[1:(xx-1),1]) + 1):(sum(zz[1:(xx-1),1]) + wees)})
    if (length(meerindeks) > 1){
      for (i in 1:length(meerindeks)){
        if (verbose == TRUE) message("i is ", i)
        x[meerindeks[[i]], vuurvar] <- x[max(meerindeks[[i]])+1, vuurvar]
      }
    }
  }
  return(x)
}

#' Wees vure Dood
#'
#' Function analyses fire episodes and isolates those that are seen as too short
#' (specified by minFireLength numeric), whereby it removes them
#'
#' @param df Data frame. Fire data
#' @param fireField Character vector. Name of the fire column
#' @param fireLabel Character vector. Name of the label indicating active fire
#' @param noFireLabel Character vector. Name of the label indicating non-active fire
#' @param minFireLength Numeric. The minimum duration allowed for a fire episode - given as
#' the number of records/time steps across which it should span
#' @export

wees.vure.dood <- function(df = NULL, fireField = "fire", fireLabel = "fire",
                           noFireLabel = "no", minFireLength = 2) {

  fireEpLength <- 0L

  df[[fireField]] <- as.character(df[[fireField]])
  fireLabel <- as.character(fireLabel)
  noFireLabel <- as.character(noFireLabel)

  df$fire_ep_length <- NA_integer_ # length of the current fire episode
  for (r in 1:nrow(df)) {
    if (is.na(df[r, fireField])) {
      fireEpLength <- 0L
      next
    }
    if (df[r, fireField] == fireLabel) {
      fireEpLength <- fireEpLength + 1
      df[r, "fire_ep_length"] <- fireEpLength
      next
    }
    if (df[r, fireField] == noFireLabel) {
      fireEpLength <- 0L
      df[r, "fire_ep_length"] <- 0
      next
    }
    # if the loop reaches this point it means that the value in df[r, fireField] is not a fire,
    # not a non-fire and also not an NA;
    # it is something yet unknown to humankind,
    # so we just set fireEpLength to 0 to break any possible fire continuation
    fireEpLength <- 0
  }

  if (nrow(df) > 1) {
    for (r in (nrow(df)-1):1) {
      if (is.na(df[r, "fire_ep_length"])) {next}
      if (df[r, "fire_ep_length"] == 0) {next}
      if (df[r, "fire_ep_length"] > 0) {
        if (is.na(df[r+1, "fire_ep_length"])) {next}
        if (df[r+1, "fire_ep_length"] == 0) {next}
        if ((df[r+1, "fire_ep_length"]) > df[r, "fire_ep_length"]) {
          df[r, "fire_ep_length"] <- df[r+1, "fire_ep_length"]
          next
        }
      }
    }
  }

  # now unfire the fires that are too short
  df[which(df$fire_ep_length < minFireLength), fireField] <- noFireLabel

  # remove the fire_ep_length field
  df <- df[, -match(x = "fire_ep_length", table = names(df))]

  # return
  return(df)
}
