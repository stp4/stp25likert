#' Sortieren von Likert-Data
#'
#'
#' Sortieren der Items nach Mittelwert. Items werden als factor() geordnet.
#'
#' @param data .data.frame Summarise_likert -Objekt tibble mit attribut "tbll"
#' @param x formula. wen nicht mit Summarise_likert erstellt dann die Formel
#' @param measure null. or character  internal
#' @param items null. or character  internal
#' @param groups null. or character internal
#' @param nlevels  null. or character internal
#' @param m_weight numeric. kann verändert werden internal
#' @param include.order .logical or character richtung der sortierung
#' @param decreasing logical. zu or abnahme
#' @param include.reference,ReferenceZero numeric.neutraler Wert
#' @param dat_attr list. internal
#'
#' @return  tibble
#' @export
#'
#' @examples
#'
#' Likert <- dummy_likert_data()
#'
#' Likert |>
#'   Summarise_likert_long(q1,q2,q3,q4,q5,q6,q7,q8,q9,
#'                         grouping = list(
#'                           FC.2 = c("q1", "q9", "q2", "q5"),
#'                           FC.3 = c("q3", "q4", "q6"),
#'                           FC.4 = c("q7", "q8")
#'                         )) |>
#'  order_weighted_mean(include.reference=4, include.order = "l")
#'
order_weighted_mean <- function(data,
                                x,
                                include.reference = NULL,
                                m_weight = NULL,
                                measure = setdiff(names(data), all.vars(x)),
                                items = intersect(all.vars(x[[2]]), names(data)),
                                groups = intersect(all.vars(x[[3]]), names(data)),
                                nlevels = ncol(data) - length(all.vars(x)) + 1,
                                ReferenceZero = include.reference,
                                include.order = FALSE,
                                decreasing = TRUE,
                                dat_attr = attr(data, "tbll")) {
  # cat("Aha du willst also sortieren.\n")
  include.weight <- TRUE
  orientatio_left <- FALSE
  is_wide <- TRUE
  data_long <- NULL
  data_wide <- NULL
  
  if (is.character(include.order)) {
    orrr <- 
      include.order <-
            match.arg(include.order, c("right", "left"))
    if (include.order == "right") {
      decreasing <-  FALSE
      orientatio_left <- TRUE
      include.order <- TRUE
    }
    else if (include.order == "left") {
      include.order <- TRUE
      orientatio_left <- FALSE
      decreasing <- TRUE
    }
  }
  else if (!include.order) {
    return(data)
  }
  else if (include.order & !decreasing) {
    orientatio_left <- TRUE
  }
  
  
  if (!is.null(dat_attr)) {
    x <- dat_attr$formula
    measure <- dat_attr$measure
    items <- dat_attr$items
    groups <- dat_attr$groups
    nlevels <- dat_attr$nlevels
    
    if (attr(data, "likert") == "long") {
      data_long <- data
      data_wide <- attr(data, "data_wide")
      is_wide <- FALSE
    }
    else{
      data_long <- attr(data, "data_long")
      data_wide <- data
      is_wide <- TRUE
      
    }
  }
  else{
    # hier kommt eine wide Objekt aber kein Likert
    data_long <- data_wide <-  data
  }
  
  
  if (is.null(m_weight)) {
    if (orientatio_left)
      m_weight <- rev(seq_len(nlevels))
    else
      m_weight <- seq_len(nlevels)
  }
  
  if (!is.null(ReferenceZero)) {
    if (orientatio_left)
      m_weight[ceiling(ReferenceZero):nlevels] <-
        (nlevels:ceiling(ReferenceZero)) /
        nlevels
    else
      m_weight[1:(ceiling(ReferenceZero - 1))] <-
        1:(ceiling(ReferenceZero - 1)) / ceiling(ReferenceZero - 1)
  }
  
  m_weight <- colSums(t(data_wide[measure]) * m_weight, na.rm = TRUE)
  
  if (length(groups) > 0) {
    m_weight <- tapply(
      m_weight,
      data_wide[[items]],
      FUN = function(x) {
        mean(x, na.rm = TRUE)
      }
    )
  }
  else{
    names(m_weight) <- as.character(data_wide[[items]])
  }
  
  ord <- order(m_weight, decreasing  = TRUE)
  data_wide[[items]] <- factor(data_wide[[items]], names(m_weight)[ord])
  data_wide$ordr_weight <- as.numeric(data_wide[[items]])
  data_long[[items]] <- factor(data_long[[items]], names(m_weight)[ord])
  data_long$ordr_weight <- as.numeric(data_long[[items]])
  
  if (is_wide) {
    
    if(is.null(data_long)) return(data_wide)
    data <- data_wide
    attr(data, "tbll") <- dat_attr
    attr(data, "data_long") <-
      dplyr::arrange(data_long, desc(ordr_weight))[-ncol(data_long)]
    attr(data, "likert") <- "wide"
    return(dplyr::arrange(data, desc(ordr_weight))[-ncol(data)])
    
    
  }
  else{
    data <- data_long
    attr(data, "tbll") <- dat_attr
    attr(data, "data_long") <-
      dplyr::arrange(data_wide, desc(ordr_weight))[-ncol(data_long)]
    attr(data, "likert") <- "long"
    return(dplyr::arrange(data, desc(ordr_weight))[-ncol(data)])
    
    
    
  }
}


#
# dat13 <- get_data(
# "
# Sex     Question 	                      Platz.1	Platz.2	Platz.3	Platz.4	Platz.5
# male   'Relaparotomie'                  0.170    0.383   	 0.170   	 0.149   	 0.128
# male   'Beiziehen des Palliativteams'   0     	 0.319   	 0.532   	 0.149   	 0
# male   'Sterben zulassen'         	    0     	 0.064   	 0.149   	 0.447   	 0.340
# male   'Wille der Patientin erheben'		0.830    0.128   	 0.021   	 0.021   	 0
# male   'Einholen eines Ethikvotums'		  0     	 0.106   	 0.128   	 0.234   	 0.532
# female 'Relaparotomie'                  0.180    0.373   	 0.170   	 0.149   	 0.128
# female 'Beiziehen des Palliativteams'   0.1   	 0.219   	 0.442   	 0.139   	 0.1
# female 'Sterben zulassen'         	    0     	 0.064   	 0.149   	 0.447   	 0.340
# female 'Wille der Patientin erheben'		0.830    0.028   	 0.021   	 0.021   	 0.1
# female 'Einholen eines Ethikvotums'		  0.1    	 0.06   	 0.128   	 0.234   	 0.532
# "
# )
#
# # order_weighted_mean(Question ~ . ,
# #                     dat13,
# #                     ReferenceZero = 2.5,
# #                     include.order = "l")
#
#
# order_weighted_mean(Question ~ . , dat13[1:5, -1])
# order_weighted_mean(Question ~ . |Sex ,
#                     dat13,
#                     ReferenceZero = 2.5,
#                     include.order = "r")
#
#
#
# order_weighted_mean(Question ~ . , dat13[1:5, -1],ReferenceZero = 2.5,include.order = "l")
# order_weighted_mean(Question ~ . , dat13[1:5, -1],ReferenceZero = 2.5,include.order = "r")

# require(tidyverse)
#
# require(stp25likert)
#
# Multi<- dummy_multi_data()
# Likert<- dummy_likert_data()
#
# Likert |>
#   Summarise_likert_long(q1,q2,q3,q4,q5,q6,q7,q8,q9,
#                        # by =~ sex,
#                         grouping = list(
#                           FC.2 = c("q1", "q9", "q2", "q5"),
#                           FC.3 = c("q3", "q4", "q6"),
#                           FC.4 = c("q7", "q8")
#                         )) |> order_weighted_mean(include.reference=4, include.order = "l")|>
#   gg_likertplot(include.reference=3)
