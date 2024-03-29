% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/siratio.R
\name{siratio}
\alias{siratio}
\alias{siratioplot}
\alias{ggsiratioplot}
\title{SI-ratio}
\usage{
siratio(x, ...)

siratioplot(
  x,
  labels = NULL,
  add = FALSE,
  box = TRUE,
  col.s = "darkblue",
  col.i = "gray",
  col.mean = "red",
  cex.i = 0.1,
  lwd.s = par("lwd"),
  lwd.mean = lwd.s,
  main = "SI ratio",
  xlab = NULL,
  ylab = NULL,
  xlim = NULL,
  ylim = NULL,
  start = NULL,
  end = NULL,
  ...
)

ggsiratioplot(
  x,
  labels = NULL,
  col.s = "darkblue",
  col.i = "gray",
  col.mean = "red",
  cex.i = 0.5,
  lwd.s = 1,
  lwd.mean = lwd.s,
  main = "SI ratio",
  xlab = NULL,
  ylab = NULL,
  start = NULL,
  end = NULL,
  ...
)
}
\arguments{
\item{x}{input model or data.}

\item{...}{unused parameters.}

\item{labels}{labels.}

\item{add}{boolean indicating whether a new plot should be drawn.}

\item{box}{boolean indicating a box around the current plot should be drawn.}

\item{col.s, col.i, col.mean}{colors of the different components.}

\item{cex.i, lwd.s, lwd.mean}{graphical parameters.}

\item{main, xlab, ylab}{title, X and Y axis label.}

\item{xlim, ylim}{X and Y axis limits.}

\item{start, end}{first and last dates plotted.}
}
\description{
SI-ratio
}
\examples{
x <- RJDemetra::x13(ipi_c_eu[,"FR"])
siratioplot(x)
ggsiratioplot(x)
}
