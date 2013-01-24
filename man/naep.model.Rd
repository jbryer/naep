\name{naep.model}
\alias{naep.model}
\title{Executes statistical analysis using the replicate weights and multiple
plausible values.}
\usage{
  naep.model(formula, var = NULL, naepData,
    svyFUN = svyglm, ...)
}
\arguments{
  \item{formula}{model formula}

  \item{var}{the variable prefix for an outcome with
  multiple plausible values (e.g. MRPCM for overall
  composite math value for math assessment)}

  \item{naepData}{a NAEP data object.}

  \item{svyFUN}{the function to use from the survey
  package.}

  \item{...}{other parameters passed to the survey
  function}
}
\value{
  an object of class \code{naep.model} with \code{print}
  and \code{summary} methods defined.
}
\description{
  This function wraps functions from the \code{survey}
  package for the analysis of complex survey designs.
  Specifically, this function will perform statistical
  analyses utilizing the provided replicate weights and
  multiple plausible values (if specified in the model).
  The function will return a list of class
  \code{naep.model} with the following elements: \itemize{
  \item{combined}{ the combined results containing
  estimates and standard errors.} \item{formula}{ the model
  formula.} \item{models}{ a list of the individual models
  before being combined. This has a length of five and
  contains the results of each \code{svyFUN} call for each
  of the five plausible values. Note that this is only
  present for models containing plausible values.}
  \item{svyFUN}{ the name of the \code{survey} function
  called.} \item{variable}{ the name of the plausible
  value.} }
}

