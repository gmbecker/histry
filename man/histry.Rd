% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00first.R
\name{histropts}
\alias{histropts}
\alias{histry}
\alias{histry_tracker}
\alias{histry_addinfo}
\alias{histry_setinknitr}
\title{histry}
\usage{
histropts()

histry()

histry_tracker()

histry_addinfo(...)

histry_setinknitr(val)
}
\arguments{
\item{\dots}{Info to be added}

\item{val}{Logical value indicating whether the evaluation context is currently
in a knitr/rmarkdown.}
}
\value{
A list of R code expressions for \code{histry} and a HistoryTracker
object for \code{histry_tracker}
}
\description{
These functions return the set of successfully evaluated expressions (\code{histry})
and the \code{HistoryTracker} object (\code{histry_tracker}) for the current context.
}
\details{
This means that when called during a knitr-based weaving process, the
expressions will be those previously evaluted durign that process, and the
tracker will be a KnitrHistoryTracker. When called outside of the weaving
context, the top-level-expression history and history tracker will be returned,
respectively.
}
