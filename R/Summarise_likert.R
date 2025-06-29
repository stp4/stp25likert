#' Summarise  Likert-type and Multi-Response Items.
#' 
#' Summarise_likert: Summe der Items als Wide-data.frame.
#' 
#' Summarise_likert_wide = Summarise_likert
#' 
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
#'   
Summarise_likert <-
  function(x,
           ...,
           grouping = NULL,
           include.total = FALSE,
           include.order = FALSE,  
           decreasing = FALSE,
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
    
 tbll <- likert_data_sum(
        x,...,
        use.level = use.level,
        reverse.levels = reverse.levels,
        include.total = include.total,
        reorder.levels = reorder.levels)
    
 tbll$mean <- likert_data_mean(
      x,...,
      use.level = use.level,
      reverse.levels = reverse.levels,
      include.total = include.total,
      reorder.levels = reorder.levels)

 
 if (!is.null(grouping)) {
     tbll$grouping <- TRUE
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
        
        tbll$freq <-
          dplyr::bind_cols(`.grouping` = grp, tbll$freq)

        # um konsistent mit Tbll_Likert zu bleiben
        tbll$lhs <- seq_len(length(tbll$lhs) + 1)
        if (length (all.names(tbll$formula[3])) == 1) {
          tbll$formula <- Item ~ . | .grouping
          tbll$facet_formula <- ~ .grouping
        }
        else{
          facet_formula <- paste(c(all.vars(tbll$formula)[-c(1:2)], ".grouping"), collapse = "+")
          tbll$formula <- formula(paste("Item~.|", facet_formula))
          facet_formula <- formula(paste("~", facet_formula))
        }
 }
 else{  
   tbll$grouping<- FALSE
   }
    
 dat_long  <- dat_wide <- tbll$freq
 tbll$names <- tbll$freq[tbll$lhs]
 tbll$freq  <- tbll$freq[-tbll$lhs]
 tbll$rhs   <- seq_len(ncol(tbll$freq)) + ncol(tbll$names)
 rhs_names <- paste0("LBL_", seq_along(tbll$rhs))
 names(dat_long)[tbll$rhs] <- rhs_names
 
 tbll$measure = names(tbll$freq) 
 tbll$items =  "Item"
 tbll$groups =  setdiff(names(tbll$names), "Item")
 tbll$columns =  setdiff(tbll$groups, ".grouping")

 dat_long <-
        tidyr::pivot_longer(
          dat_long,
          cols = all_of(tbll$rhs),
          names_to = "levels",
          values_to = "Freq")
 dat_long$levels <- factor(dat_long$levels, labels = tbll$levels)      
      
 if(include.order){warning("Nicht implemintiert")}

 if (type.wide) {
   attr(dat_wide, "tbll") <- tbll
   attr(dat_wide, "data_long") <- dat_long
   attr(dat_wide, "likert") <- "wide"
   return(dat_wide)
 }
 else {
   attr(dat_long, "tbll") <- tbll
   attr(dat_long, "data_wide") <- dat_wide
   attr(dat_long, "likert") <- "long"
   return(dat_long)
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
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA ) {
    Summarise_likert(..., 
                     type.wide = TRUE, 
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
    Summarise_likert(..., 
                     type.wide = FALSE, 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
  }

#' @export
#' @rdname Summarise_likert
Summarise_multi <-
  function(...,
           type.wide = FALSE, 
           use.level = 1,
           reverse.levels = FALSE,
           reorder.levels = NA 
           ) {
    Summarise_likert(..., 
                     type.wide = type.wide, 
                     use.level = use.level, 
                     reverse.levels = reverse.levels)
  }

likert_data_sum  <- function(...,
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
          if (any(max(x, na.rm = TRUE) > 1))
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
        else if (!is.na(reorder.levels)) {
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
  
  
  if (pos_col_names == 1) {
    fm <- Item ~ .
    facet_fm <- NULL
  }
  else {
    facet_fm <- paste(col_names[1:(pos_col_names - 1)], collapse = "+")
    fm <- formula(paste("Item ~ .|", facet_fm))
    facet_fm <- formula(paste("~", facet_fm))
  }
  
  list(
    freq = rslt,
    mean = NULL,
    nlevels = length(col_names[-lhs]),
    levels = col_names[-lhs],
    lhs = lhs,
    N = NULL,
    facet_formula = facet_fm,
    formula = fm
  )
}

likert_data_mean  <- function(...,
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
        if (any(max(x, na.rm = TRUE) > 1))
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
      } else if (!is.na(reorder.levels)) {
        if (is.numeric(reorder.levels))
          x <- factor(x, levels(x)[reorder.levels])
        else
          x <- factor(x, reorder.levels)
      }
      
      n <- length(x)
      x <-  na.omit(as.numeric(x))
      c(
        m = mean(x, na.rm = TRUE),
        sd = sd(x, na.rm = TRUE),
        n = length(x),
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



