% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg-likertplot.R, R/lattice-likertplot.R
\name{gg_likertplot}
\alias{gg_likertplot}
\alias{gg_likert_stacked}
\alias{likert_stacked}
\title{Barcharts for Likert}
\usage{
gg_likertplot(
  data,
  x,
  include.order = NULL,
  decreasing = FALSE,
  include.reference = NULL,
  as.percent = TRUE,
  exclude_fill_values = NULL,
  y_label_wrap = 50,
  reverse_likert = FALSE,
  width = 0.9,
  facet_formula = NULL,
  facet_label_wrap = 50,
  cutoff = include.reference,
  palette = "RdBu",
  direction = 1,
  ...
)

gg_likert_stacked(
  data,
  x,
  include.order = FALSE,
  decreasing = FALSE,
  include.value = TRUE,
  facet_formula = x,
  facet_label_wrap = 50,
  labels_size = 3.5,
  labels_color = "black",
  labels_accuracy = 1,
  labels_hide_below = 0.05,
  y_label_wrap = 50,
  reverse_fill = TRUE,
  width = 0.9,
  ...
)

likert_stacked(
  data,
  x = NULL,
  include.order = FALSE,
  decreasing = FALSE,
  main = "",
  ylab = "",
  sub = "",
  xlab = "Freq",
  ylim = NULL,
  xlim = NULL,
  col = NULL,
  rightAxis = FALSE,
  columns = 2,
  space = "top",
  auto.key = list(space = space, columns = columns, between = 1),
  par.settings = NULL,
  ...
)
}
\arguments{
\item{data}{data.frame im long Format}

\item{include.order, decreasing}{character. include.order = c("right", "left")  die Sortierung}

\item{include.reference, cutoff}{numeric. Referenzline}

\item{as.percent}{logical. ggstats}

\item{exclude_fill_values}{character. ggstats}

\item{y_label_wrap, facet_label_wrap}{ggstats numeric. Umbrechen der Labels}

\item{reverse_likert}{logical.  ggstats nicht zu Verwenden Balken umdrehen gethaber nicht für die Beschriftung}

\item{width}{numeric.  ggstats breite der Balken}

\item{facet_formula}{formula. ggplot}

\item{palette, direction}{scharacter. ggstats  scale_fill_brewer  direction = 1 or -1}

\item{...}{not used}

\item{labels_size}{numeric. gg_likert_stacked}

\item{labels_color}{character. gg_likert_stacked}

\item{labels_accuracy}{numeric. gg_likert_stacked}

\item{labels_hide_below}{numeric. gg_likert_stacked}

\item{reverse_fill}{logical.}

\item{add_labels}{logical. gg_likert_stacked}
}
\value{
ggplot

ggplot
}
\description{
Barcharts for Likert
}
\examples{
 
Likert <- dummy_likert_data()
 
Likert |>
  Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9,
                        by =~ Sex,
                        grouping = list(
                          FC.2 = c("q1", "q2"),
                          FC.3 = c("q3","q4", "q5", "q6"),
                          FC.4 = c("q7", "q8", "q9")
                        )
  ) |>
  gg_likertplot()


1+1

}
