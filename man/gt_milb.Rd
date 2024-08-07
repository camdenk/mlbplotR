% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gt_helpers.R
\name{gt_milb}
\alias{gt_milb}
\alias{gt_fmt_milb_logo}
\alias{gt_fmt_milb_dot_logo}
\title{Add MiLB team logos into rows of a \code{gt} table}
\usage{
gt_fmt_milb_logo(gt_object, columns, height = 30, locations = NULL)

gt_fmt_milb_dot_logo(gt_object, columns, height = 30, locations = NULL)
}
\arguments{
\item{gt_object}{An existing gt table object of class \code{gt_tbl}}

\item{columns}{The columns wherein changes to cell data colors should occur.
Argument has no effect if \code{locations} is not \code{NULL}.}

\item{height}{The absolute height (px) of the image in the table cell}

\item{locations}{If \code{NULL} (the default), the function will render
logos in argument \code{columns}.
Otherwise, the cell or set of cells to be associated with the team name
transformation. Only the \code{\link[gt:cells_body]{gt::cells_body()}}, \code{\link[gt:cells_stub]{gt::cells_stub()}},
\code{\link[gt:cells_column_labels]{gt::cells_column_labels()}}, and \code{\link[gt:cells_row_groups]{gt::cells_row_groups()}} helper functions
can be used here. We can enclose several of these calls within a \code{list()}
if we wish to make the transformation happen at different locations.}
}
\value{
An object of class \code{gt_tbl}.
}
\description{
The \code{gt_fmt_milb_logo} and \code{gt_fmt_milb_dot_logo} functions take an existing
\code{gt_tbl} object and converts MiLB team names into team logos.
This is a wrapper around
\href{https://jthomasmock.github.io/gtExtras/reference/gt_img_rows.html}{\code{gtExtras::gt_image_rows()}}
written by Tom Mock, which is a wrapper around \code{gt::text_transform()} + \code{gt::web_image()}/
\code{gt::local_image()} with the necessary boilerplate already applied.
}
\examples{
\donttest{
library(gt)
library(mlbplotR)
gt_milb_example <- mlbplotR::load_milb_teams() \%>\%
  dplyr::filter(parent_org_name == "Texas Rangers") \%>\%
  dplyr::mutate(dot = team_name) \%>\%
  dplyr::select(team_name, dot, team_location, team_mascot) \%>\%
  gt::gt() \%>\%
  gt_fmt_milb_logo(columns = "team_name") \%>\%
  gt_fmt_milb_dot_logo(columns = "dot")
}
}
