\name{selectVariables}
\alias{selectVariables}
\title{Variable selection dialog box.}
\usage{
  selectVariables(year = NULL, grade = NULL,
    subject = NULL, directory = getBaseDirectory(),
    sample = c("AT", "PM"), type = c("Student", "School"))
}
\arguments{
  \item{year}{the year of the NAEP data.}

  \item{grade}{the grade to read.}

  \item{subject}{the subject to read (e.g. Math, Read)}

  \item{directory}{the root directory of the NAEP data
  disc.}

  \item{sample}{either AT or PM.}

  \item{type}{the type of data to read (i.e. Student or
  School)}
}
\value{
  a character vector with selected variables.
}
\description{
  This function provides a dialog box for selecting
  variables to read from the NAEP database. Returns a
  vector of variable names.
}

