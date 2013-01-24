\name{getNAEPData}
\alias{getNAEPData}
\title{Read NAEP data.}
\usage{
  getNAEPData(year, grade, subject, directory = NULL,
    vars = NULL, sample = c("AT", "PM"),
    type = c("Student", "School"))
}
\arguments{
  \item{year}{the year of the NAEP data.}

  \item{grade}{the grade to read.}

  \item{subject}{the subject to read (e.g. Math, Read)}

  \item{directory}{the root directory of the NAEP data
  disc.}

  \item{vars}{the variables to return. If NULL, the default
  set will be returned.}

  \item{sample}{either AT or PM.}

  \item{type}{the type of data to read (i.e. Student or
  School)}
}
\description{
  Read the NAEP database and return a data object.
}
\details{
  This function will read data from the NAEP data discs and
  return a list containing the following:

  \itemize{ \item{data}{ a data frame containing the read
  data.} \item{catalog}{ a data frame containing metadata.}
  \item{design}{ the design of the complex survey from
  \code{\link{svydesign}}.} \item{grade}{ the grade level.}
  \item{subject}{ the subject.} \item{year}{ the year.}
  \item{repweights}{ a character vector containing the
  names of the replicate weights.} \item{sample}{ the
  sample (either AT or PM)} \item{sourceDir}{ the directory
  where the data was read from.} \item{type}{ the type of
  data (either student or school).} }
}

