#' Dummy Daten
#'
#' @param n integer. Anzahl
#' @param levels character. 
#'
#' @return data.frame
#' @import dplyr
#' @export
#'
dummy_likert_data <- 
  function(n = 2*3*9*3,
          levels = c(
                    "Strongly disagree",
                    "Disagree",
                    "Neither agree nor disagree",
                    "Agree",
                    "Strongly agree"
                    )){
 
  set.seed(42)
 
  DF_lik <- tibble(
      q1 = sample(levels, n, replace = TRUE, prob = c(3,2, 1, 4,5)),
      q2 = sample(levels, n, replace = TRUE, prob = c(3,1, 1, 1,1)),
      q3 = sample(levels, n, replace = TRUE, prob = c(7,5, 1, 0,0)),
      q4 = sample(levels, n, replace = TRUE, prob = c(0,0, 0, 1,5)),
      q5 = sample(levels, n, replace = TRUE, prob = c(1,4, 1, 1,5)),
      q6 = sample(levels, n, replace = TRUE, prob = c(2,3, 0, 2,3)),
      q7 = sample(levels, n, replace = TRUE, prob = c(3,2, 0, 1,7)),
      q8 = sample(levels, n, replace = TRUE, prob = c(1,2, 1, 4,1)),
      q9 = sample(levels, n, replace = TRUE, prob = c(6,0, 0, 0,6))
    ) |>
    mutate(across(everything(), ~ factor(.x, levels = levels))) |>
    mutate(Sex = factor(sample(c("male", "female"), n, replace = TRUE)),
           Age = factor(sample(c("18-30", "30-50", ">50"), n, replace = TRUE))) |>
    stp25tools::Label(
      # FC.2
      q1 =  "Nuts Seeds 1",
      q2 =  "Legumes 2",
      # FC.3
      q3 = "Vegetables Juice 3",
      q4 = "Fruit 4",
      q5 = "Vegetables 5",
      #   FC.4
      q6 = "Milk 6",
      q7 = "Cheese 7",
      q8 = "Yoghurt 8",
      q9 = "Dairy Alternatives  9"
    )
    DF_lik$q8[sample.int(n)[seq_len(ceiling (0.02*n))] ] <- NA
    DF_lik$q9[sample.int(n)[seq_len(ceiling (0.06*n))] ] <- NA
    DF_lik[c(10,11,1:9)]
}
#' @export
#' @rdname dummy_likert_data
dummy_multi_data <- function(n=100){
dat <- dummy_likert_data(n) 
  
dat  |>
  mutate(
    q1 = as.numeric(q1) > 3,
    q2 = as.numeric(q2) > 3,
    q3 = as.numeric(q3) > 3,
    q4 = as.numeric(q4) > 3,
    q5 = as.numeric(q5) > 3,
    q6 = as.numeric(q6) > 3,
    q7 = as.numeric(q7) > 3,
    q8 = as.numeric(q8) > 3,
    q9 = as.numeric(q9) > 3
  ) |> stp25tools::set_label(stp25tools::get_label(dat))
}

 


