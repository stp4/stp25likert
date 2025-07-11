% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Summarise_likert.R
\name{Summarise_likert}
\alias{Summarise_likert}
\alias{Summarise_likert_long}
\alias{Summarise_likert_wide}
\alias{Summarise_multi_wide}
\alias{Summarise_multi_long}
\alias{Summarise_multi}
\title{Summarise  Likert-type and Multi-Response Items.}
\usage{
Summarise_likert(
  x,
  ...,
  grouping = NULL,
  include.total = FALSE,
  include.order = FALSE,
  decreasing = FALSE,
  reverse.levels = FALSE,
  exclude.levels = NULL,
  reorder.levels = NA,
  type.wide = TRUE,
  use.level = NULL
)

Summarise_likert_long(...)

Summarise_likert_wide(...)

Summarise_multi_wide(
  ...,
  use.level = 1,
  reverse.levels = FALSE,
  reorder.levels = NA
)

Summarise_multi_long(
  ...,
  use.level = 1,
  reverse.levels = FALSE,
  reorder.levels = NA
)

Summarise_multi(
  ...,
  type.wide = FALSE,
  use.level = 1,
  reverse.levels = FALSE,
  reorder.levels = NA
)
}
\arguments{
\item{x}{Tbll_likert - Objekt}

\item{...}{an Summarise}

\item{grouping}{list. aufsplittten in SubTabel}

\item{include.order, decreasing, }{logical. sortiern aber bei Plots geht das nicht so}

\item{reverse.levels, include.total, exclude.levels, reorder.levels}{logical. 
fuer wie in Tbll_likert}

\item{type.wide}{logical.}

\item{use.level}{logical. fuer Multiresponse}
}
\value{
long tibble mit Item, levels,  Freq
}
\description{
Summarise_likert: Summe der Items als Wide-data.frame.
}
\details{
Summarise_likert_wide = Summarise_likert
}
\examples{

Likert <- dummy_likert_data(245)

Likert |> Summarise_likert_long(q1, q2, q3) 
Likert |> Summarise_likert_wide(q1, q2, q3) 


Multi <- dummy_multi_data(245)

Multi |> Summarise_multi_long(q1, q2, q3) 
Multi |> Summarise_multi_wide(q1, q2, q3) 


Likert |>
  Summarise_likert(q1, q2, q3, q4, q5, q6, q7, q8, q9,
                   by =~ Sex + Age,
                   grouping = list(
                     FC.2 = c("q1", "q2"),
                     FC.3 = c("q4", "q5", "q6"),
                     FC.4 = c("q7", "q8", "q9")
                   )
  ) |> attr("tbl")  
  
}
