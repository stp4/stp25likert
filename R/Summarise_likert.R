#' Summary likert data
#' 
#' Summarise_likert: Summe der Items als Wide-data.frame.
#' 
#' Summarise_likert_wide = Summarise_likert
#' 
#' You could use `gglikert_data()` to just produce the dataset to be plotted.
#'
#' @param x Tbll_likert - Objekt
#' @param ... an Summarise
#' @param type.wide logical.
#' 
#' @param include.order,decreasing, logical. sortiern aber bei Plots geht das nicht so
#' @param grouping list. aufsplittten in SubTabel
#' @param use.level  logical. fuer Multiresponse
#' @param reverse.levels,include.total,exclude.levels,reorder.levels logical. 
#' fuer wie in Tbll_likert
#' 
#'
#' @return long tibble mit Item, levels,  Freq
#' @export
#'
#' @examples
#' 
#' Likert <- dummy_likert_data(245)
#' 
#' Likert |> Summarise_likert_long(q1, q2, q3) 
#' Likert |> Summarise_likert_wide(q1, q2, q3) 
#' 
#' 
#' Multi <- dummy_multi_data(245)
#' 
#' Multi |> Summarise_multi_long(q1, q2, q3) 
#' Multi |> Summarise_multi_wide(q1, q2, q3) 
#' 
#' 
#' Likert |>
#'   Summarise_likert(q1, q2, q3, q4, q5, q6, q7, q8, q9,
#'                    by =~ Sex + Age,
#'                    grouping = list(
#'                      FC.2 = c("q1", "q2"),
#'                      FC.3 = c("q4", "q5", "q6"),
#'                      FC.4 = c("q7", "q8", "q9")
#'                    )
#'   ) |> attr("tbl")  
Summarise_likert <-
  function(x,
           ...,
           grouping = NULL,
           include.total = FALSE,
           include.order = FALSE,  decreasing = FALSE,
           reverse.levels = FALSE,
           exclude.levels = NULL,
           reorder.levels = NA,
           type.wide =TRUE,
           use.level = NULL) {

 if (!is.null(exclude.levels)) {
      if (!is.na(reorder.levels))
        stop(" reorder.levels in kombination mit exclude.levels geht nicht!")
      reorder.levels <- exclude.levels * (-1)
    }    
    
 lkt <- likert_data_sum(
        x,...,
        use.level = use.level,
        reverse.levels = reverse.levels,
        include.total = include.total,
        reorder.levels = reorder.levels)
    
 lkt$mean <- likert_data_mean(
      x,...,
      use.level = use.level,
      reverse.levels = reverse.levels,
      include.total = include.total,
      reorder.levels = reorder.levels)

 
  
 if (!is.null(grouping)) {
     lkt$grouping <- TRUE
        # Workaraund um die Namen zu extrahieren
        itm <- grp <-
          likert_data_sum(
            x, ...,
            include.label = FALSE,
            use.level = use.level, 
            reverse.levels = reverse.levels,
            include.total =include.total
          )$freq$Item
      
        # Schleife duch die unterschiedlich langen labels
        for (i in seq_along(grouping)) {
          new_lvl <- names(grouping)[i]
          for (k in grouping[[i]]) 
            levels(grp)[levels(grp) == k] <- new_lvl
        }
        
        lkt$freq <-
          dplyr::bind_cols(`.grouping` = grp, lkt$freq)
        
        # um konsistent mit Tbll_Likert zu bleiben
        lkt$lhs <- seq_len(length(lkt$lhs) + 1)
        if (length (all.names(lkt$formula[3])) == 1)
          lkt$formula <- Item ~ . | .grouping
        else
          lkt$formula <- formula(paste("Item~.|", paste(
            c(all.vars(lkt$formula)[-c(1:2)],  ".grouping"), collapse = "+"
          )))
 }
 else{  lkt$grouping<- FALSE}
    
 dat_long  <- dat_wide <- lkt$freq
 lkt$names <- lkt$freq[lkt$lhs]
 lkt$freq  <- lkt$freq[-lkt$lhs]
 lkt$rhs   <- seq_len(ncol(lkt$freq)) + ncol(lkt$names)
 rhs_names <- paste0("LBL_", seq_along(lkt$rhs))
 names(dat_long)[lkt$rhs] <- rhs_names
 

 lkt$measure = names(lkt$freq) 
 lkt$items =  "Item"
 lkt$groups =  setdiff(names(lkt$names), "Item")
 lkt$columns =  setdiff(lkt$groups, ".grouping")

 
 
 dat_long <-
        tidyr::pivot_longer(
          dat_long,
          cols = all_of(lkt$rhs),
          names_to = "levels",
          values_to = "Freq")
 dat_long$levels <- factor(dat_long$levels, labels = lkt$levels)      
      
 if(include.order){warnings("Nicht implemintiert")}

 if(type.wide) {
    attr(dat_wide, "tbll") <- lkt
    attr(dat_wide, "data_long") <- dat_long
    attr(dat_wide, "likert") <- "wide"
    dat_wide
}
 else {
    attr(dat_long, "tbll") <- lkt
    attr(dat_long, "data_wide") <- dat_wide
    attr(dat_long, "likert") <- "long"
    dat_long
 }

  }


#' @export
#' @rdname Summarise_likert
Summarise_likert_long <- function(...){
  Summarise_likert(..., type.wide = FALSE)
}


#' @export
#' @rdname Summarise_likert
Summarise_likert_wide <- function(...){
  Summarise_likert(..., type.wide = TRUE)
}


#' @export
#' @rdname Summarise_likert
Summarise_multi_wide <-
  function(..., 
           type.wide = TRUE,
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA ) {
    Summarise_likert(..., type.wide = FALSE, 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
  }


#' @export
#' @rdname Summarise_likert
Summarise_multi_long <-
  function(...,
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA) {
    Summarise_likert(..., type.wide = FALSE, 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
  }

#' @export
#' @rdname Summarise_likert
Summarise_multi <-
  function(...,
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA 
           ) {
    Summarise_likert(..., 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
  }

likert_data_sum  <-
  function(...,
           include.total = FALSE,
           use.level = NULL,
           reverse.levels = FALSE,
           reorder.levels = NA) {
  rslt <-
      stp25stat2::Summarise(
        ...,
        fun = function(x) {
          
          if (is.logical(x)) {
            x <- factor(x, c(TRUE, FALSE))
          }
          else if (is.numeric(x)) {
            
            if (any(max(x, na.rm=TRUE) > 1))
              stop(
                "\nWenn Zahlen uebergeben werden duerfen die nur im Wertebereich von 0 bis 1 liegen!\n"
              )
            x <- factor(x, 1:0)
          }
          
          if (!is.null(use.level)) {
            x <- factor(x == levels(x)[use.level], c(TRUE, FALSE))
          }
          else if (reverse.levels) {
            x <- rev(x)
          }
          else if(!is.na(reorder.levels)) {
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)
          }
          table(x, useNA = "no")
        },
        key = "Item",
        margins = include.total
        #value = "value",
        #na.action = na.pass,
        # formula = NULL,
        # margins_name = "Total",
       # include.label = TRUE
      )
    
    ncl <- ncol(rslt)
    col_names <- names(rslt)
    pos_col_names <- grep("Item", col_names)
    lhs <- c(1:pos_col_names)

    list(
      freq = rslt,
      mean = NULL,
      nlevels = length(col_names[-lhs]),
      levels =col_names[-lhs],
      lhs =lhs,
      N =NULL,
      
      formula =  if (pos_col_names == 1) Item ~ .
                 else {formula(
                       paste("Item ~ .|", 
                             paste(col_names[1:(pos_col_names - 1)],
                                   collapse = "+")))}
      )
  }

likert_data_mean  <-
  function(...,
           include.total = FALSE,
           use.level = NULL,
           reverse.levels = FALSE,
           reorder.levels = NA) {
 
      stp25stat2::Summarise(
        ...,
        fun = function(x) {
          if (is.logical(x)) {
            x <- factor(x, c(TRUE, FALSE))
          }
          else if (is.numeric(x)) {
            if (any(max(x, na.rm=TRUE) > 1))
              stop(
                "\nWenn Zahlen uebergeben werden duerfen die nur im Wertebereich von 0 bis 1 liegen!\n"
              )
            x <- factor(x, 1:0)
          }
          
          if (!is.null(use.level)) {
            x <- factor(x == levels(x)[use.level], c(TRUE, FALSE))
          }
          else if (reverse.levels) {
            x <- rev(x)
          } else if(!is.na(reorder.levels)) {
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)
          }
        
         n<- length(x)
          x <-  na.omit(as.numeric(x)  )
        c(m = mean(x, na.rm=TRUE), 
          sd =sd(x, na.rm=TRUE), 
          n= length(x),
          missing = n - length(x)
          )
        },
        key = "Item",
        margins = include.total,
        #value = "value",
        #na.action = na.pass,
        # formula = NULL,
        # margins_name = "Total",
        include.label = TRUE
      )
    
 
 
  }



