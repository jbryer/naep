\name{getNAEPCatalog}
\alias{getNAEPCatalog}
\title{Return the NAEP catalog as a data frame}
\usage{
  getNAEPCatalog(year, grade, subject,
    directory = getBaseDirectory(), sample = c("AT", "PM"),
    type = c("Student", "School"))
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
\description{
  Read NAEP catalog
}
\details{
  Read the NAEP catalog and return a data frame of the
  available variables in the NAEP database.
}

