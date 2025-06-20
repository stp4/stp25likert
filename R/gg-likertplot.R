#' Barcharts for Likert
#' 
#' 
#' 
#' @param reverse_fill  logical.
#' @param ... not used
#' @param data  data.frame im long Format
#' @param include.reference,cutoff numeric. Referenzline
#' @param include.order,decreasing character. include.order = c("right", "left")  die Sortierung
#' @param as.percent  logical. ggstats
#' @param exclude_fill_values  character. ggstats
#' @param reverse_likert logical.  ggstats nicht zu Verwenden Balken umdrehen gethaber nicht für die Beschriftung
#' @param width numeric.  ggstats breite der Balken 
#' @param facet_formula formula. ggplot
#' @param y_label_wrap,facet_label_wrap ggstats numeric. Umbrechen der Labels
#' @param palette,direction scharacter. ggstats  scale_fill_brewer  direction = 1 or -1
#'
#' @import ggplot2
#' @import ggstats
#' @export
#' @examples
#'  
#' Likert <- dummy_likert_data()
#'  
#' Likert |>
#'   Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9,
#'                         by =~ Sex,
#'                         grouping = list(
#'                           FC.2 = c("q1", "q2"),
#'                           FC.3 = c("q3","q4", "q5", "q6"),
#'                           FC.4 = c("q7", "q8", "q9")
#'                         )
#'   ) |>
#'   gg_likertplot()
#' 
gg_likertplot <-
  function(data, x, 
           include.order =NULL, decreasing=FALSE, 
           include.reference =NULL, 
           
           #variable_labels = NULL,
           as.percent = TRUE,
           exclude_fill_values = NULL,
           y_label_wrap = 50,
           reverse_likert = FALSE,
           width = .90,
           facet_formula = NULL,
           facet_label_wrap = 50,
           
          
           cutoff  =include.reference,
           palette = "RdBu", # scale_fill_brewer"BrBG"
           direction = 1,...
  ) {

    if(!is.null( attr(data, "tbll_likert") )) {
      stop("\nDas habe ich noch nicht fertig verwende einfach Summarise_likert(!!!\n")
    }
    
    if (is.null(attr(data, "tbl")))
      data <- Summarise_likert_long(data, x, ...)
    
    col_wrap <- attr(data, "tbll")$columns
    grouping <- attr(data, "tbll")$grouping
    fm <- attr(data, "tbll")$formula
    
    if (!is.null(include.order))
      data <- order_weighted_mean(
        data,
        ReferenceZero = include.reference,
        include.order = include.order,
        decreasing = decreasing
      )
    
    if (attr(data, "likert") == "wide")
      data <- attr(data, "data_long")
    

    if (is.null(facet_formula)) {
      if (grouping) {
        if (length(col_wrap) == 0) {
          facet_formula <- .grouping ~ .
        }
        else if (length(col_wrap) == 1) {
          facet_formula <-  formula(paste(".grouping ~", col_wrap))
        }
        else if (length(col_wrap) == 2) {
          facet_formula <- formula(paste(".grouping ~", 
                                         paste(col_wrap[1:2], collapse ="+")))
        }
        else{
          
          stop("Das ist zu lang! Hier musst du facet_formula = .grouping ~ a + b + c selbst zusammen stellen!")
        }
      }
      else{
        if (length(col_wrap) == 0) {
          facet_formula <- NULL
        }
        else if (length(col_wrap) == 1) {
          facet_formula <- formula(paste(".~ ", col_wrap))
        }
        else if (length(col_wrap) == 2)
          facet_formula <- formula(paste(col_wrap[2], "~", col_wrap[1]))
        else {
          print(fm)
          stop("Hier muss facet_formula = a ~ b + c mit uebergeben werden!")
        }
      }
    }
    
    p <-
      data |>
      ggplot2::ggplot(ggplot2::aes(
        y = Item,
        fill = levels,
        by = Item,
        weight = Freq
      ))
    
    if (as.percent) {
      p <- p +
        ggplot2::geom_bar(
          position = ggstats::position_likert(
            reverse = reverse_likert, 
            exclude_fill_values = exclude_fill_values,
            cutoff =cutoff
            ),
          stat = ggstats::StatProp,
          complete = "fill",
          width = width
        ) +
        labs(x = NULL, y = NULL, fill = NULL) +
        scale_x_continuous(labels = label_percent_abs())
    }
    else  {
      p <- p +
        ggplot2::geom_bar(
          position = 
            ggstats::position_likert_count(
              reverse = reverse_likert, 
              exclude_fill_values = exclude_fill_values,
              cutoff =cutoff
              ),
          stat = ggstats::StatProp,
          complete = "fill",
          width = width
        ) +
        scale_x_continuous(label = label_number_abs())
      
    }
    
   # if (nlevels(data$levels) <= 11) 
    p <- p + scale_fill_brewer(palette = palette, direction = direction)

    p <- p +
      scale_y_discrete(labels = scales::label_wrap(y_label_wrap)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      theme_light() +
      theme(legend.position = "bottom",
            panel.grid.major.y = element_blank())
    
    if (!is.null(facet_formula))
      p <- p + 
      facet_grid(
        facet_formula,
        scales = "free",
        space = "free",
        labeller = ggplot2::label_wrap_gen(facet_label_wrap)
      )
    p
  }

#' @rdname gg_likertplot
#'
#' @param add_labels logical. gg_likert_stacked
#' @param labels_size numeric. gg_likert_stacked
#' @param labels_color character. gg_likert_stacked
#' @param labels_accuracy numeric. gg_likert_stacked
#' @param labels_hide_below numeric. gg_likert_stacked
#'
#' @return ggplot
#' @export
#'
#' @examples
#' 
#' 1+1
#' 
gg_likert_stacked <- 
  function(data, x, 
           include.order = FALSE, decreasing=FALSE,
           
           include.value = TRUE,
           facet_formula = x,
           facet_label_wrap = 50,
           # include = dplyr::everything(),
           # weights = NULL,
           # y = ".question",
           # variable_labels = NULL,
           # sort = c("none", "ascending", "descending"),
           # sort_method = c("prop", "mean", "median"),
           # sort_prop_include_center = FALSE,
           #  add_labels = TRUE,
           labels_size = 3.5,
           labels_color = "black",
           labels_accuracy = 1,
           labels_hide_below = .05,
           # add_median_line = FALSE,
           # y_reverse = TRUE,
           y_label_wrap = 50,
           reverse_fill = TRUE,
           width = .9,
           ...
           
  ) {
    
    if(!is.null( attr(data, "tbll_likert") )) {
      stop("\nDas habe ich noch nicht fertig verwende einfach Summarise_likert(!!!\n")
    }
    
    
    if (is.null(attr(data, "tbl")))
      data <- Summarise_likert_long(data, x, ...)
    
    col_wrap <- attr(data, "tbll")$columns
    grouping <- attr(data, "tbll")$grouping
    fm <- attr(data, "tbll")$formula
    
    if (!is.null(include.order))
      data <- order_weighted_mean(
        data,
        ReferenceZero = NULL,
        include.order = include.order,
        decreasing = decreasing
      )
    
    if (attr(data, "likert") == "wide")
      data <- attr(data, "data_long")
    
    
    p <- 
      ggplot(data) +
      aes(
        x = Item,
        fill = levels,
        weight = Freq,
        by = Item
      ) +
      geom_bar(
        position = position_fill(reverse = reverse_fill),
        stat = StatProp,
        complete = "fill",
        width = width
      )+
      labs(x = NULL, y = NULL, fill = NULL) +
      scale_y_continuous(labels = label_percent_abs())
    
    if (include.value) {
      p <- p +
        geom_text(
          mapping = aes(
            label = label_percent_abs(
              hide_below = labels_hide_below,
              accuracy = labels_accuracy
            )(after_stat(prop))
          ),
          stat = StatProp,
          complete = "fill",
          position = position_fill(
            vjust = .5,
            reverse = reverse_fill
          ),
          size = labels_size,
          color = labels_color
        )
    }
    # 
    # if (add_median_line) {
    #   p <- p +
    #     ggplot2::geom_vline(xintercept = .5)
    # }
    # 
    
    # scale_x_continuous(labels = ggstats::label_percent_abs()) +
    #  scale_y_discrete(
    #labels = scales::label_wrap(y_label_wrap)
    #                  ) +
    
    
    #  print(facet_formula)
    if (!is.null(facet_formula))
      p <-  p + facet_grid(
        facet_formula,
        scales = "free",
        space = "free",
        labeller = ggplot2::label_wrap_gen(facet_label_wrap)
      )
    
    p<- p +  theme_light() +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank()
      )
    # 
    # if (length(levels(data$.answer)) <= 11) {
    p <- p + scale_fill_brewer(palette = "BrBG")
    # }
    
    p + coord_flip()
  }




 
