#' likert data
#'
#'  You could use `gglikert_data()` to just produce the dataset to be plotted.
#'
#' @param x Tbll_likert -Objekt
#' @param ... platzhalter
#' @param include.total margin an Summarise
#' @param  reverse.levels = FALSE, levels umdrehen
#'
#' @return long tibble mit Item, levels,  Freq
#' @export
#'
#' @examples
#'
#' #' require(stp25likert)
#' set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = cut(rnorm(n), 5, lvs),
#'   Comic.books = cut(rnorm(n), 5, lvs),
#'   Fiction = cut(rnorm(n), 5, lvs),
#'   Newspapers = cut(rnorm(n), 5, lvs),
#'   Geschlecht = cut(rnorm(n), 2, c("m", "f"))
#' ) |>  stp25tools::Label(Comic.books = "Comic-books")
#'
#' dat_lg <- Tbll_likert(
#'   DF2,
#'   Magazines,
#'   Comic.books,
#'   Fiction,
#'   Newspapers,
#'   by =  ~ Geschlecht,
#'   include.order = TRUE
#' )
#'
#' likert_data(dat_lg)
#'
#' likert_data(
#'   DF2,
#'   Magazines,
#'   Comic.books,
#'   Fiction,
#'   Newspapers,
#'   by =  ~ Geschlecht,
#'   include.total =TRUE
#' )
#'
#' lvls <- c(TRUE, FALSE)
#' n<- 150
#' DF_multi <-
#'   tibble(
#'     q1 =  rbinom(n, 1, .5) ,
#'     q2 =   rbinom(n, 1, .4),
#'     q3 =   rbinom(n, 1, .55),
#'     q4 =   rbinom(n, 1, .56),
#'     q5 =   rbinom(n, 1, .53),
#'     q6 =   rbinom(n, 1, .2),
#'     q7 =   rbinom(n, 1, .85),
#'     q8 =  rbinom(n, 1, .95),
#'     q9 =  rbinom(n, 1, .25)
#'   ) |>
#'   mutate(across(everything(), ~ factor(.x ))) |>
#'   mutate(Sex = sample(c("male", "female"), n, replace = TRUE),
#'          Age = sample(c("18-30", "30-50", ">50"), n, replace = TRUE)) |>
#'   stp25tools::Label(
#'     # FC.2
#'     q1 =  "Nuts.Seeds",
#'     q2 =  "Legumes",
#'     # FC.3
#'     q3 = "Vegetables Juice ",
#'     q4 = "Fruit",
#'     q5 = "Vegetables",
#'     #   FC.4
#'     q6 = "Milk",
#'     q7 = "Cheese",
#'     q8 = "Yoghurt",
#'     q9 = "Dairy Alternatives"
#'   )
#'
#' summary(DF_multi)
#'
#' DF_multi |>
#'   likert_data(q1,q2, q3, q4, q5, q6, q7, q8, q9,
#'               by=~ Sex + Age,
#'               grouping= list(
#'                 FC.2 = c("q1", "q2"),
#'                 FC.3 = c("q4", "q5", "q6"),
#'                 FC.4 = c("q7", "q8", "q9")
#'               )
#'   )
#'
likert_data <- function(x,
                        ...,
                        grouping = NULL,
                        include.order = TRUE,
                        reverse.levels = FALSE,
                        decreasing = TRUE,
                        use.level = NULL
                        ) {
  # Hier kommt ein Tbll_likert-Objekt
  lkt <- attr(x, "plot")

  if (is.null(lkt)) {
    lkt <- likert_data_sum(x,..., 
                           use.level = use.level,
                           reverse.levels = reverse.levels)
    if (!is.null(grouping)) {
     # cat("\nin Grouping\n")
      itm <- grp <- 
        likert_data_sum(x,..., 
                        include.label = FALSE, 
                        use.level = use.level,
                        reverse.levels = reverse.levels
                        )$results$Item

      # Schleife duch die unterschiedlich langen labels
       for (i in seq_along(grouping)) {
        new_lvl <- names(grouping)[i]
        for (k in grouping[[i]]) {
          levels(grp)[levels(grp) == k] <- new_lvl
        }
       }

      lkt$results  <-
        cbind(tibble::tibble(`.Item` = itm, `.grouping` = grp),
              lkt$results)
      
      # um konsistent mit Tbll_Likert zu bleiben
      if(length (all.names(lkt$formula[3]) ) ==1)
        lkt$formula <-  Item ~. |.Item + .grouping
      else
      lkt$formula <- formula(paste("Item~.|",
                           paste(
                             c(all.vars(lkt$formula)[-c(1:2)],
                                ".Item", ".grouping"), collapse="+")))
    }
  }

  param <- all.vars(lkt$formula)[-2]

  lvls <- names(lkt$results)[-seq_along(param)]
  rhs <- paste0("LBL_", seq_along(lvls))
  names(lkt$results)[-seq_along(param)] <- rhs

  datl <-
    tidyr::pivot_longer(
      lkt$results,
      cols = all_of(rhs),
      names_to = "levels",
      values_to = "Freq"
    )
  datl$levels <- factor(datl$levels, labels = lvls)

  if (include.order) {
    rst <- datl["Item"]
    rst$order <- as.numeric(datl$levels) * datl$Freq

    rst <- aggregate(
      order ~ Item,
      rst,
      FUN = function(x) mean(x, na.rm = TRUE)
    )

    datl$Item <- factor(datl$Item,
                        levels(datl$Item)[order(rst$order,
                                                decreasing = !decreasing)])
  }

  datl
}
#' @export
#' @rdname likert_data
multi_data <- function(...,
                       use.level = 1,  reverse.levels = FALSE) {
  likert_data(...,  use.level = use.level,  reverse.levels = reverse.levels)
}

likert_data_sum  <-function(...,
                            include.total = FALSE,
                            use.level = NULL,
                            reverse.levels = FALSE){
  rslt <-
    stp25stat2::Summarise(
      ...,
      fun = function(x) {
        if (is.logical(x)) {
          x <- factor(x, c(TRUE, FALSE))
        }
        else if (is.numeric(x)) {
          if (any(x > 1))
            stop("\nWenn Zahlen übergeben werden durfen die nur im Wertebereich von 0 bis 1 liegen!\n")
          x <- factor(x, 1:0)
        }
        
        if (!is.null(use.level)) {
          x <- factor(x == levels(x)[use.level], c(TRUE, FALSE))
        }
        else if (reverse.levels){
          x <- rev(x)
          }
        
        table(x, useNA = "no")
      },
      key = "Item",
      margins = include.total
    )

  ncl <- ncol(rslt)
  col_names <- names(rslt[-ncl])
  pos_col_names <- grep("Item", col_names)

 list(
   results = rslt,
    formula =  if (pos_col_names == 1) {Item ~ .}
    else{formula(paste("Item ~ .|",
                       paste(col_names[1:(pos_col_names - 1)], collapse = "+")))}
  )
}

# require(tidyverse)
# set.seed(42)
# n <- 243
# lvls <- c(
#   "Strongly disagree",
#   "Disagree",
#   "Neither agree nor disagree",
#   "Agree",
#   "Strongly agree"
# )
#
# DF_likert <-
#   tibble(
#     q1 = sample(lvls, n, replace = TRUE),
#     q2 = sample(lvls, n, replace = TRUE, prob = 5:1),
#     q3 = sample(lvls, n, replace = TRUE, prob = 1:5),
#     q4 = sample(lvls, n, replace = TRUE, prob = 1:5),
#     q5 = sample(lvls, n, replace = TRUE, prob = 1:5),
#     q6 = sample(lvls,n, replace = TRUE, prob = c(1, 0, 1, 1, 0)),
#     q7 = sample(lvls, n, replace = TRUE, prob = 1:5),
#     q8 = sample(lvls, n, replace = TRUE, prob = 1:5),
#     q9 = sample(lvls, n, replace = TRUE, prob = 1:5)
#   ) |>
#   mutate(across(everything(), ~ factor(.x, levels = lvls))) |>
#   mutate(Sex = sample(c("male", "female"), n, replace = TRUE),
#          Age = sample(c("18-30", "30-50", ">50"), n, replace = TRUE),
#          Sex =factor(Sex),
#          Age =factor(Age)
#
#          ) |>
#   stp25tools::Label(
#     # FC.2
#     q1 =  "Nuts.Seeds",
#     q2 =  "Legumes",
#     # FC.3
#     q3 = "Vegetables Juice ",
#     q4 = "Fruit",
#     q5 = "Vegetables",
#     #   FC.4
#     q6 = "Milk",
#     q7 = "Cheese",
#     q8 = "Yoghurt",
#     q9 = "Dairy Alternatives"
#   )
#
#
#
# DF_multi <-
#   tibble(
#     q1 = rbinom(n, 1, .50) ,
#     q2 = rbinom(n, 1, .40),
#     q3 = rbinom(n, 1, .55),
#     q4 = rbinom(n, 1, .56),
#     q5 = rbinom(n, 1, .53),
#     q6 = rbinom(n, 1, .20),
#     q7 = rbinom(n, 1, .85),
#     q8 = rbinom(n, 1, .95),
#     q9 = rbinom(n, 1, .25)
#   ) |>
#   # mutate(across(everything(), ~ factor(.x ))) |>
#   mutate(Sex = sample(c("male", "female"), n, replace = TRUE),
#          Age = sample(c("18-30", "30-50", ">50"), n, replace = TRUE),
#          Sex =factor(Sex),
#          Age =factor(Age)
#
#          ) |>
#   stp25tools::Label(
#     # FC.2
#     q1 =  "Nuts.Seeds",
#     q2 =  "Legumes",
#     # FC.3
#     q3 = "Vegetables Juice ",
#     q4 = "Fruit",
#     q5 = "Vegetables",
#     #   FC.4
#     q6 = "Milk",
#     q7 = "Cheese",
#     q8 = "Yoghurt",
#     q9 = "Dairy Alternatives"
#   )
#
# x<-
# DF_likert |>
#   likert_data(
#     q1,q2,q3,q4,q5,q6,q7,q8,q9,
#     by =~ Sex,
#     grouping = list(
#       FC.2 = c("q1", "q2"),
#       FC.3 = "q3",
#       FC.4 =  c("q4", "q5", "q6"),
#       FC.5 = c("q7", "q8", "q9")
#     ),
#     include.order = TRUE
#   )
#
# DF_likert |>
#   likert_data(
#     q1,q2,q3,q4,q5,q6,q7,q8,q9,
#
#     grouping = list(
#       FC.2 = c("q1", "q2"),
#       FC.3 = "q3",
#       FC.4 =  c("q4", "q5", "q6"),
#       FC.5 = c("q7", "q8", "q9")
#     ),
#     include.order = TRUE
#   )
