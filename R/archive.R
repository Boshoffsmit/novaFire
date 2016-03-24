#' Archive
#'
#' Function is used to archive an R data file
#'
#' @param fileName Character vector. The name of the file that must be archived
#' @param currentDir Character vector. The current directory of the file
#' @param archiveDir Character vector. The directory that the file must be archived in
#' @param verbose Logical. Displays function messages when TRUE
#' @export

archive <- function(fileName = NULL, currentDir = NULL, archiveDir = NULL, verbose = FALSE)
{
	# input error checking
  if (is.null(fileName)) {
    stop("No file name received. Archiving process stopped.")
  }

	if (is.null(currentDir)) {
	  warning("No current directory info received. Defaulting to working directory.")
	  currentDir <- getwd()
	 }

  if (!(substr(x = currentDir, start = nchar(currentDir), stop = nchar(currentDir)) ==  '/')) {
	  currentDir <- paste(currentDir, '/', sep = "")
	 }

  archiveDir <- ifelse(is.null(archiveDir), paste(currentDir, "Archive/", sep = ""), archiveDir)

	if (!(substr(x = archiveDir, start = nchar(archiveDir), stop = nchar(archiveDir)) ==  '/')) {
	  archiveDir <- paste(archiveDir, '/', sep = "")
	}
	if (verbose) {print(paste("Archive dir: ", archiveDir, sep = ""))}

  # ---------- # input error checking done

	if (file.exists(paste(currentDir, fileName, sep = "")))
	{
		if (verbose) {print("Archiving previous version found...")}

	  # extract the date of creation of the file that should be archived
	  dateCreated <- as.integer(file.info(paste(currentDir, fileName, sep = ""))[["ctime"]])
	  dateCreated <- as.POSIXct.numeric(dateCreated, origin = "1970-01-01 00:00:00", tz = "Africa/Johannesburg")
	  dateCreated <- substr(x = dateCreated, start = 1, stop = 19)
	  if (verbose) {print(paste("Date created:", dateCreated, sep = " "))}

	  # format dateCreated so that it can be inserted in the file name of the old file
	  dateCreated <- gsub(pattern = " ", replacement = "_", x = dateCreated)
	  dateCreated <- gsub(pattern = ":", replacement = "-", x = dateCreated)
	  dateCreated <- paste(dateCreated, "_", sep = "")

	  if (!dir.exists(archiveDir)) {
	    dir.create(path = archiveDir, showWarnings = TRUE, recursive = FALSE)
	   }
	  file.copy(from = paste(currentDir, fileName, sep = ""), to = paste(archiveDir, fileName, sep = ""), copy.mode = TRUE, copy.date = TRUE, overwrite = TRUE)
		file.remove(paste(currentDir, fileName, sep = ""))
		file.rename(from = paste(archiveDir, fileName, sep = ""), to = paste(archiveDir, dateCreated, fileName, sep = ""))
		if (verbose) {print("Previous version successfully archived.")}
	}
}
