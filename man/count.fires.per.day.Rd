% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_fires.R
\name{count.fires.per.day}
\alias{count.fires.per.day}
\title{Count Fires Per Day}
\usage{
count.fires.per.day(df = NULL, datevar = "date", firevar = "vuur",
  vuurnaam = "vuur", groupvar = NULL)
}
\arguments{
\item{df}{Data frame. Contains fire data}

\item{datevar}{Character vector. Name of the column containing dates}

\item{firevar}{Character vector. Name of the column specifying whether a fire is active or not}

\item{vuurnaam}{Character vector. Text that correspond to a fire being active}

\item{groupvar}{Character vector. An optional grouping variable}
}
\description{
Creates a summary of the mean fires per day for certain months as well as fire proportions for
each day
}

