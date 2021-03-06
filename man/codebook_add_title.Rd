% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codebook_add_title.R
\name{codebook_add_title}
\alias{codebook_add_title}
\title{Optionally Add Title and Subtitle to Codebook}
\usage{
codebook_add_title(rdocx, title = NA, subtitle = NA)
}
\arguments{
\item{rdocx}{rdocx object created with `officer::read_docx()`}

\item{title}{Optional title}

\item{subtitle}{Optional subtitle}
}
\value{
rdocx object
}
\description{
This function is not intended to be a stand-alone function. It is indented
to be used by the `codebook` function.
}
