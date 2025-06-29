#' Analysis multi-response items.
#' 
#' Tbll_multi() ist praktisch die Funktio stp25stat2::Tbll_desc() mit Atribut attr(rslt, "tbll_likert").
#' 
#' @param  ... an prepate.data stp25stat2::Tbll_desc_multi
#' @param by  an prepate.data
#' @param digits numeric.
#' @param include.total,include.order,exclude.last.order logical.
#' @param use.level,include.label .logical Beschriftung
#' @param include.n,include.nr  logical Anzahl
#' @param include.test,include.normality.tests,include.custom,include.value logical. stp25stat2::Tbll_desc_multi
#'
#' @export
#' @examples
#'
#' Multi <- dummy_multi_data()
#' 
#' Multi |>
#'   Tbll_multi(q1, q2, q3, q4, by = ~ Sex) |>
#'  # stp25output2::Output() |> 
#'   likert_stacked()
#' 
Tbll_multi <- function(...,
                    by = NULL,
                       digits = 0,
                       include.order = TRUE,
                       exclude.last.order = FALSE,
                       use.level = 1,
                       include.label = TRUE,
                       include.n = TRUE,
                       include.nr = FALSE,
                       include.total = FALSE,
                       include.test = FALSE,
                       include.normality.tests = FALSE,
                       include.custom = NULL,
                       include.value = NULL) {
  rslt <- stp25stat2::Tbll_desc_multi(
    ...,
    by = by,
    include.label = include.label,
    include.n = include.n,
    include.nr = include.nr,
    include.total = include.total,
    include.test = include.test,
    include.normality.tests = include.normality.tests,
    include.custom = include.custom,
    include.value = include.value,
    digits = digits,
    use.level = use.level
  )

  attr(rslt, "tbll_likert") <-  
      Summarise_multi(...,
                      by = by,
                      include.total = include.total,
                      use.level = use.level) 
 
  rslt
}


 


 
