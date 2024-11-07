#' Summary likert data
#' 
#' Summe der Items als Long-Datafile.
#' 
#' You could use `gglikert_data()` to just produce the dataset to be plotted.
#'
#' @param x Tbll_likert -Objekt
#' @param ... platzhalter
#' @param reverse.levels logical.  levels umdrehen
#' @param grouping list. 
#' @param include.order logical.
#' @param decreasing logical.
#' @param use.level logical.
#'
#' @return long tibble mit Item, levels,  Freq
#' @export
#'
#' @examples
#'  
#' DF <- dummy_likert_data()
#' 
#' rslt <- DF  |> 
#'   Tbll_likert(
#'     q1,q2,q3,q4,q5,q6,
#'     by = ~ sex,
#'     include.order = TRUE
#'   )
#' Summarise_likert(rslt)
#' 
#' DF |> 
#'   Summarise_likert(q1, q2, q3, q4, q5, q6, q7, q8, q9,
#'               by =~ sex + age,
#'               grouping = list(
#'                 FC.2 = c("q1", "q2"),
#'                 FC.3 = c("q4", "q5", "q6"),
#'                 FC.4 = c("q7", "q8", "q9")
#'               )
#'   )
Summarise_likert <- 
  function(x,
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
#' @rdname Summarise_likert
Summarise_likert_wide <- function(...){
  levels<- Freq <- .grouping <- Item <- NULL
  long_df <- Summarise_likert(...)
  
  if( any( names( long_df ) == ".grouping"))
    long_df[,-1] |>  
    tidyr::spread(levels , Freq ) |>
    dplyr::mutate(#.grouping =as.character(.grouping),
      Item =as.character(Item)
    ) |>
    dplyr::arrange(.grouping)
  else long_df |>     
    tidyr::spread(levels , Freq )
  
}

#' @export
#' @rdname Summarise_likert
Summarise_multi <-
  function(...,
           use.level = 1,
           reverse.levels = FALSE) {
    Summarise_likert(..., 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
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

