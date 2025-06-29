#' ggplot-Barcharts for Likert and Multi-Response
#' 
#' @param data  data.frame im long Format
#' @param ... to Summarise_likert()
#' @param facet_formula formula. ggplot NULL or formula Item ~ .
#' @param include.reference numeric. Referenzline
#' @param include.order,decreasing character. include.order = c("right", "left")  die Sortierung
#' @param include.percent  logical. 
#' @param exclude_fill_values  character. ggstats ausgeschlossene Level
#' @param reverse_likert logical.  ggstats nicht zu Verwenden Balken umdrehen gethaber nicht für die Beschriftung
#' @param width numeric.  ggstats breite der Balken 
#' @param y_label_wrap,facet_label_wrap ggstats numeric. Umbrechen der Labels
#' @param palette,direction scharacter. ggstats  scale_fill_brewer  direction = 1 or -1

#' 
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip scale_x_continuous after_stat scale_fill_brewer scale_y_discrete theme_light theme labs element_blank facet_grid label_wrap_gen position_fill scale_y_continuous geom_text
#' @importFrom ggstats StatProp position_likert PositionLikert label_percent_abs label_number_abs 
#' @importFrom scales label_wrap
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
  function(data, 
           ...,
           facet_formula = NULL,
           include.order = NULL, decreasing = FALSE, 
           include.reference = NULL, 
           include.percent =  TRUE,
           exclude_fill_values = NULL,
           reverse_likert = FALSE,
          # include.value = FALSE
           facet_label_wrap = 50,
           y_label_wrap = 50,
           palette = "RdBu", 
           direction = 1,
           width = .90
          ) {

    if(!is.null( attr(data, "tbll_likert") )) {
      stop("\nDas habe ich noch nicht fertig verwende einfach Summarise_likert(!!!\n")
    }
 
    # data muss in folge ein likert-long- data.frame sein
    if (is.null(attr(data, "tbl")))
      data <- Summarise_likert_long(data, ...)
    
    col_wrap <- attr(data, "tbll")$columns
    grouping <- attr(data, "tbll")$grouping
    fm <- attr(data, "tbll")$formula
    
    if (!is.null(include.order)){
      data <- order_weighted_mean(
        data,
        include.reference = include.reference,
        include.order = include.order,
        decreasing = decreasing
      )}
    
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
    
    if (attr(data, "likert") == "wide") {
      data <- attr(data, "data_long")
     # attr(data, "likert") <- "long"
    }
    
    

    p <-
      data |>
      ggplot2::ggplot(ggplot2::aes(
        y = Item,
        fill = levels,
        by = Item,
        weight = Freq
      ))
    
    if (include.percent) {
      p <- p +
        ggplot2::geom_bar(
          position = ggstats::position_likert(
            reverse = reverse_likert, 
            exclude_fill_values = exclude_fill_values,
            cutoff = include.reference
            ),
          stat = ggstats::StatProp,
          complete = "fill",
          width = width
        ) +
       ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
        ggplot2::scale_x_continuous(labels = ggstats::label_percent_abs())
    }
    else {
      p <- p +
        ggplot2::geom_bar(
          position = 
            position_likert_count(
              reverse = reverse_likert, 
              exclude_fill_values = exclude_fill_values,
              cutoff = include.reference
              ),
          width = width
        ) +
        ggplot2::scale_x_continuous(label = ggstats::label_number_abs())
    }
    
   # if (nlevels(data$levels) <= 11) 
    if(!is.null(palette))
    p <- p + 
      ggplot2::scale_fill_brewer(palette = palette, direction = direction)

    p <- p +
      ggplot2::scale_y_discrete(labels = scales::label_wrap(y_label_wrap)) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::theme_light() +
      ggplot2::theme(legend.position = "bottom",
            panel.grid.major.y = ggplot2::element_blank())
    
    if (!is.null(facet_formula))
      p <- p + 
      ggplot2::facet_grid(
        facet_formula,
        scales = "free",
        space = "free",
        labeller = ggplot2::label_wrap_gen(facet_label_wrap)
      )
    p
  }

#' @noRd
#' @rdname position_likert
#' @format NULL
#' @usage NULL
PositionLikertCount <- 
  ggplot2::ggproto("PositionLikertCount",
                   ggstats::PositionLikert,
                   fill = FALSE,
                   cutoff = NULL,
                   reverse= FALSE
  )


#' @noRd
#' @rdname position_likert
position_likert_count <- function(vjust = 1,
                                  reverse = FALSE,
                                  exclude_fill_values = NULL,
                                  cutoff = NULL
                                  ) {
  ggplot2::ggproto(
    NULL,
    PositionLikertCount,
    vjust = vjust,
    reverse = reverse,
    exclude_fill_values = exclude_fill_values,
    cutoff = cutoff
  )
}





#' @rdname gg_likertplot
#' 
#' @param labels_size numeric. Schriftgrösse
#' @param labels_color character.   Zweifarbig  labels_color = c("blue","gray")
#' @param labels_hide_below numeric. minimum der Beschriftung
#' @param include.value  logical. Prozent anzeigen
#' @return ggplot
#' @export
#'
#' @examples
#' 
#' Multi <- stp25likert::dummy_multi_data()
#' 
#' Multi |>
#'   stp25likert::Tbll_multi(q1, q2, q3, q4, q5, q6, q7, by = ~ Sex) |>
#'   gg_likert_stacked(
#'     include.order = "l",
#'     palette = "BrBG", direction = 1,
#'     
#'     labels_size = 4.5,
#'     labels_color = c("blue","gray"),
#'     width = .5)
#' 
gg_likert_stacked <- 
  function(  
    data, ..., 
    facet_formula = NULL, 
    include.order = FALSE, decreasing = FALSE,
    palette = "BrBG", direction = 1,
    
    facet_label_wrap = 50,
    include.value = TRUE,
    include.percent =  TRUE,
    labels_size = 3.5,
    labels_color = "black",
    labels_hide_below = .05,
    y_label_wrap = 50,
    width = .90
  ) {
    
    
    if (!is.null(attr(data, "tbll_likert"))) 
      data <-  attr(data, "tbll_likert")
    
    if (is.null(attr(data, "tbl")))
      data <- Summarise_likert_long(data, x, ...)
    
    col_wrap <- attr(data, "tbll")$columns
    grouping <- attr(data, "tbll")$grouping
    
    if (is.null(facet_formula))
      facet_formula <- attr(data, "tbll")$facet_formula
    
    if (!is.null(include.order)){
      data <- order_weighted_mean(
        data,
        include.reference = NULL,
        include.order = include.order,
        decreasing = decreasing
      )}
    
    if (attr(data, "likert") == "wide")
      data <- attr(data, "data_long")
    
    
    if (length(labels_color) != 1) {
      if (length(labels_color) == 0)
        labels_color <-  "black"
      else
        labels_color <- rep_len(labels_color, nrow(data))
    }
    p <- 
      ggplot2::ggplot(data) +
      ggplot2::aes(
        x = Item,
        fill = levels,
        weight = Freq,
        by = Item
      ) +
      ggplot2::geom_bar(
        position = ggplot2::position_fill(reverse = TRUE),
        stat = ggstats::StatProp,
        complete = "fill",
        width = width
      ) +
      ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
      ggplot2::scale_y_continuous(labels = ggstats::label_percent_abs())
    
    if (include.value) {
      p <- p +
        ggplot2::geom_text(
          mapping = 
            ggplot2::aes(
              label = ggstats::label_percent_abs(
                hide_below = labels_hide_below, 
                accuracy = 1)(ggplot2::after_stat(prop))
            ),
          stat = ggstats::StatProp,
          complete = "fill",
          position = ggplot2::position_fill(vjust = .5, reverse = TRUE),
          size = labels_size,
          color = labels_color
        )
    }
    
    
    if (!is.null(facet_formula))
      p <- p + ggplot2::facet_grid(
        facet_formula,
        scales = "free",
        space = "free",
        labeller = ggplot2::label_wrap_gen(facet_label_wrap)
      )
    
    p <- p + 
      ggplot2::theme_light() +
      ggplot2:: theme(
        legend.position = "bottom",
        panel.grid.major.y = ggplot2::element_blank()
      )
    
    
    if(!is.null(palette))
      p <- p +
      ggplot2::scale_fill_brewer(palette = palette,
                                 direction = direction)
    
    p + ggplot2::coord_flip()
  }


    
