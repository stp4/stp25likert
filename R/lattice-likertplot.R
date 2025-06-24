#' Barcharts for Likert
#' 
#' copie from HH:::plot.likert.formula
#' Constructs and plots diverging stacked barcharts for Likert
#'
#' Die orginale Funktion hat bei der Sortierung (positive.order) einen Fehler.
#'
#' @param formula formula   Item ~ . | .grouping
#' @param data daten
#' Format tibble (wide)
#' sex Item Strongly Neither Agree
#' @param main,ylab,sub,xlab Beschriftung
#' @param col HH::brewer.pal.likert
#' @param wrap Zeien Umbrechen 
#' @param rightAxis,as.percent,ReferenceZero,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param include.order,decreasing logical or character, Sortieren der Items
#' @param positive.order das nicht verwenden!! - wird ueber Tbll_likert oder include.order = TRUE gesteuert.
#' @param auto.key,columns,space  columns = 2,
#' @param ... HH:::plot.likert.formula  between=list(x=0))
#'
#' @return  lattice barchart
#' @export
#'
#' @examples
#'   
#' Likert <- dummy_likert_data(245)
#' 
#' Likert |>
#'   Summarise_likert(q1,q2,q3,q4,q5,q6,q7,q8,q9,
#'                    grouping = list(
#'                      FC.2 = c("q1", "q9", "q2", "q5"),
#'                      FC.3 = c("q3", "q4", "q6"),
#'                      FC.4 = c("q7", "q8")
#'                    )) |>
#'   transform(Item= as.character(Item) )|>
#'   plyr::arrange(.grouping)  |>
#'   likertplot(
#'     Item ~ . | .grouping,
#'     scales = list(y = list(relation = "free")),
#'     layout = c(1, 3),
#'     between = list(y = 0),
#'     strip = FALSE,
#'     strip.left = lattice::strip.custom(bg = "gray97"),
#'     par.strip.text = list(cex = .8, lines = 5),
#'     main = "Ernärung",
#'     ylab = NULL,
#'     wrap = FALSE
#'   )
#' 
#' Likert |>
#'   Summarise_likert_long(q1,q2,q3,q4,q5,q6,q7,q8,q9,
#'                         grouping = list(
#'                           FC.2 = c("q1", "q9", "q2", "q5"),
#'                           FC.3 = c("q3", "q4", "q6"),
#'                           FC.4 = c("q7", "q8")
#'                         )) |>
#'   transform(Item = as.character(Item)) |>
#'   plyr::arrange(.grouping)  -> dat
#' dat
#' HH::likert(
#'   Item ~ levels | .grouping ,
#'   value = "Freq",
#'   data = dat,
#'   ylab = NULL,
#'   scales = list(y = list(relation = "free")),
#'   layout = c(1, 3)
#' )
#' 
#' 
#' 
#' # p <-
#' # Likert |>
#' #   Summarise_likert(q1,q2,q3,q4,q5,q6,q7,q8,q9,
#' #                  by =~ Sex,
#' #                  grouping = list(
#' #                    FC.2 = c("q1", "q9", "q2", "q5"),
#' #                    FC.3 = c("q3", "q4", "q6"),
#' #                    FC.4 = c("q7", "q8")
#' #                  )) |>
#' #   transform(Item= as.character(Item) )|>
#' #   plyr::arrange(.grouping)  |>
#' #   stp25likert::likertplot(
#' #     Item ~ . |  Sex + .grouping ,
#' #     scales = list(y = list(relation = "free")),
#' #     layout = c(1, 3),
#' #     between = list(y = 0),
#' #     #   strip = FALSE,
#' #     #  strip.top = strip.custom(bg = "gray97"),
#' #     #  strip.left = strip.custom(bg = "gray97"),
#' #     #  par.strip.text = list(cex = .8, lines = 5),
#' #     main = "Ernärung",
#' #     ylab = NULL,
#' #     wrap = FALSE
#' #   )
#' # 
#' # require(latticeExtra)
#' # useOuterStrips(
#' #  p,
#' #  strip.left = strip.custom(horizontal = FALSE),
#' #  strip.lines = 2,
#' #  strip.left.lines = 1
#' # )
 
likertplot <- function(...){
  
  UseMethod("likertplot")
  
}



#prepare_likert <- function()


#' @rdname likertplot
#' @export
likertplot.data.frame <- function(
    data, 
    formula = NULL, 
    include.reference = NULL,
    include.total = FALSE,
    use.level = NULL,
    reverse.levels = FALSE, #nur mit Langen Daten
    nlevels = NULL,
    m_weight =NULL,
    groups = NULL, 
    ReferenceZero = include.reference,
    ...
){
  
  
  if(!is.null( attr(data, "tbll_likert") )) return(
    
   likertplot(attr(data, "tbll_likert"),
      formula = formula, 
      include.reference = include.reference,
      include.total = include.total,
      use.level = use.level,
      reverse.levels = reverse.levels,  
      nlevels = nlevels,
      m_weight = m_weight,
      groups = groups, 
      ReferenceZero = ReferenceZero,
      ...
    )
  )
  
# cat("\n in likertplot.data.frame\n")
# test data.format
  if (!is.null(attr(data, "likert"))) {
    if (!is.null(formula) & !attr(data, "likert") == "wide") {
      if (length(all.vars(formula[-3])) > 1) {
        data <- Summarise_likert(
          formula,
          data,
          type.wide = TRUE,
          include.total = include.total,
          use.level = use.level,
          reverse.levels = reverse.levels
        )
        formula <- NULL
      }
    }
    
    if (attr(data, "likert") == "wide") {
      # print(names(data))
      if (names(data)[1] == ".grouping") {
        if (is.null(formula))
          stop("\n Bei .grouping musst du mir die formula geben!\n")
      }
      groups <- setdiff(names(data)[attr(data, "tbll")$lhs], "Item")
      
      if (is.null(formula)) {
        formula <- "Item ~ ."
        if (length(groups) > 0)
          formula <- paste(formula, "|" , paste(groups, sep = "+"))
        
        formula <- formula(formula)
      }
      nlevels <- attr(data, "tbll")$nlevels
      m_weight  <- attr(data, "tbll")$m$m
      if (!is.null(attr(data, "tbll")$ReferenceZero))
        ReferenceZero <- attr(data, "tbll")$ReferenceZero
    }
    else {
      if (is.null(formula))
        stop("Ohne Formel gets nit!\n")
      my_vars <- all.vars(formula)
      if (!("."  %in% my_vars))
        stop(
          "Ich kann nur mit aufbereiteten Daten und Item ~. arbeiten! Bzw mit q1 + q2 + q3 + q4 + q5 + q6 ~ sex + data\n"
        )
      
      measure <-  setdiff(names(data), my_vars)
      m_weight <- NULL
      nlevels <- length(measure)
      items <- all.vars(formula)[1]
      groups <- setdiff(names(data), c(items, measure))
    }
    groups <-   if (length(groups) > 0)
      groups
    else
      NULL
  }
  else {
    if (is.null(nlevels))
      nlevels <- ncol(data) - length(all.vars(formula)) + 1
  }
 
  
  likertplot.default(
    x = formula,
    data = data,
    items = all.vars(formula)[1],
    measure = setdiff(names(data), all.vars(formula)),
    groups =  groups,
    grouping = attr(data, "tbll")$grouping,
    ReferenceZero = ReferenceZero,
    nlevels =  nlevels,
   # m_weight = m_weight,

    ...
  )
}



#' @rdname likertplot
#' @export
likertplot.formula <- function(formula, data = NULL, ...) {
  if (is.null(data))
    stop("Ich brauch schon Daten um ine Grafik zu erstellen - du Koffer!\n")
  likertplot.data.frame(data, formula, ...)
}

 
# likertplot.list<- function(
#     x,
#     formula=NULL,
#     ReferenceZero=NULL,
#     ...
# ){
#  if(is.null(formula)) formula <- x$formula
#  
#  nms<- names(x$results)
#  measure <- setdiff(nms, all.vars(formula))
#  items <- all.vars(formula)[1]
#  groups <- setdiff(nms, c(items, measure))
#   
#  likertplot.default(
#   x = formula,
#   data = x$results,
#   items=items,
#   measure=measure,
#   groups= if( length(groups) >0) groups else NULL,
#   ReferenceZero = ReferenceZero ,
#   nlevels =  x$nlevels,
#   m_weight = x$m
#   
#   )
#   
# }

#' @rdname likertplot
#' @export
#' 
#'  
#' @seealso  Orginale Funktion von R. Heiberger \code{\link[HH]{plot.likert}}
#' 
#' 
likertplot.default <-
  function(x,
           data,
           items,
           measure,
           groups,
           grouping=FALSE,
           nlevels,
          
           ReferenceZero = NULL,
           include.order = NULL,
           decreasing =  TRUE,
           
           ## An HH::likert
           main = '',
           ylab = "",
           sub = "",
           xlab = if (horizontal) { if (as.percent)"Prozent" else "Anzahl" } else "",
           ylim =NULL, xlim=NULL,
           col = NULL,
           rightAxis = FALSE,
           include.percent = TRUE,
           as.percent = include.percent,
           columns = 2,
           space = "top",
           between = list(x = 1 + (horizontal), y = 0.5 + 2 * (!horizontal)),
           auto.key = list(space = space, columns = columns, between = 1),
         
           reference.line.col = "gray65",
           col.strip.background = "gray97",
           horizontal = TRUE,
           par.settings  = NULL,
           wrap = TRUE,
           # obsolet
           positive.order = NULL,
     
           ...) {
    
    
#cat("\n  jetzt gehts an die erstellung der Grafik ...")
  if(!is.null(positive.order))
    stop("positive.order geht nicht mehr\n\n Neu ist include.order aber die Ergebnisse im plot sind anderst!!\n")

  if (is.null(col)) {
    col <- if (is.null(ReferenceZero)) likert_col(n=nlevels)
           else likert_col(n=nlevels, middle = ReferenceZero)
  }
    
  if (!is.null(include.order)) {
      data <- order_weighted_mean(
        data, x,
        measure = measure,
        items = items,
        groups = groups,
        nlevels = nlevels,
        ReferenceZero = ReferenceZero,
        include.order = include.order,
        decreasing = decreasing
      )
      
  data[[items]] <- as.character(data[[items]])   
  
#  cat("\n grouping", grouping)
   if(  grouping  )   data <- plyr::arrange(data, .grouping) 
  } 
    
    if (is.logical(wrap))
      if (wrap)
        wrap <- 35
    
    # Bedarf noch einer ueberarbeitung da Factoren oder character fehler verursachen
    if (is.numeric(wrap)) 
      data[[items]] <-
        stp25tools::wrap_string(as.character(data[[items]]), wrap)
  

  lattice_plot <-
    HH:::plot.likert.formula(
      x = x,
      data = data,
      main = main,
      ylab = ylab,
      sub = sub,
      xlab = xlab,
      col = col,
      rightAxis = rightAxis,
      positive.order = FALSE,
      as.percent = as.percent,
      auto.key = auto.key,
      ReferenceZero =  ReferenceZero,
      reference.line.col = reference.line.col,
      col.strip.background = col.strip.background,
      between = between,
      horizontal = horizontal,
      par.settings.in = par.settings,
      ...
    )

  if (horizontal) {
    if (!is.null(xlim))
      lattice_plot <-
        lattice:::update.trellis(lattice_plot, xlim = xlim)
    if (!is.null(ylim))
      lattice_plot <-
        lattice:::update.trellis(lattice_plot, ylim = ylim)

  }
  else  {
    if (!is.null(xlim))
      lattice_plot <-
        lattice:::update.trellis(lattice_plot, ylim = xlim)
    if (!is.null(ylim))
      lattice_plot <-
        lattice:::update.trellis(lattice_plot, xlim = ylim)
  }

  lattice_plot
 }





#' @rdname gg_likertplot
#' 
#' @return lattice barchart
#' @import lattice
#' @export
#' 
#' @param include.percent,margin Prozent darstellen margin = 1 bzw bei Gruppen  margin = 1:2
#' @param x formula  ~ Freq | Group
likert_stacked <-
  function(data,
           x = NULL,
           include.order = FALSE,
           include.percent = TRUE,
           margin = 1,
           decreasing = FALSE,
           main = '',
           ylab = "",
           sub = "",
           xlab =   "Freq" ,
           ylim = NULL,
           xlim = NULL,
           col = NULL,
           rightAxis = FALSE,
           # as.percent = TRUE,
           columns = 2,
           space = "top",
           # between = list(x = 1 + (horizontal), y = 0.5 + 2 * (!horizontal)),
           auto.key = list(space = space,
                           columns = columns,
                           between = 1),
           # reference.line.col = "gray65",
           #  col.strip.background = "gray97",
           #  horizontal = TRUE,
           par.settings  = NULL,
           ...) {
    if (!is.null(attr(data, "tbll_likert")))
      data <-  attr(data, "tbll_likert")
    
    if (is.null(attr(data, "tbl")))
      data <- Summarise_likert_long(data, x, ...)
    
    col_wrap <- attr(data, "tbll")$columns
    grouping <- attr(data, "tbll")$grouping
    nlevels  <- attr(data, "tbll")$nlevels
    
    if (!is.null(include.order))
      data <- order_weighted_mean(
        data,
        ReferenceZero = NULL,
        include.order = include.order,
        decreasing = decreasing
      )
    
    if (attr(data, "likert") == "wide")
      data <- attr(data, "data_long")
    
    if (is.null(x))
      x <-  attr(data, "tbll")$formula
    
    
    if (include.percent) {
      fm <- all.vars(x)
      fm <-  paste(fm[-which(fm == ".")], collapse = "+")
      fm <- formula(paste("Freq ~" , fm, "+ levels"))
      data <- as.data.frame(prop.table(xtabs(fm, data), margin = 1))
      data$Freq <- data$Freq * 100
      xlab <- "Percent"
    }
    if (length(all.names(x)) == 3) {
      x <- reformulate("Freq", x[[2]])
    } else{
      rhs <-   paste(all.vars(x[[3]])[-1], collapse = "+")
      x <-  as.formula(paste(x[[2]], "~Freq|", rhs))
    }
    if (nlevels < 3)
      nlevels <- 3
    
    
    if (!grouping) {
      barchart(
        x,
        data,
        groups = levels,
        main = main,
        xlab = xlab,
        stack = TRUE,
        border = NULL,
        col = RColorBrewer::brewer.pal(nlevels, "BrBG")
      )
    }
    else {
      ngr <-  length(attr(data, "tbll")$groups)
      ngr2 <- length(unique(data$.grouping))
      barchart(
        x,
        data,
        groups = levels,
        main = main,
        xlab = xlab,
        stack = TRUE,
        border = NULL,
        col =  if (is.null(col))
          RColorBrewer::brewer.pal(nlevels, "BrBG")
        else
          col,
        scales = list(y = list(relation = "free")),
        layout = c(ngr, ngr2),
        between = list(y = 0)
      )
    }
  }
 
#' likert_col
#'
#' Farben:
#'   Greens,  Blues,  Reds,  Greys, Oranges, Purples
#'
#' @param n Number of different colors in the palette
#' @param name A palette name from the lists below "RdBl"  ist   RdBl = c("Reds", "Blues")
#' @param middle,middle.color reference   "gray65"
#'
#' @return character
#' @export
#'
#' @examples
#'
#' par(mfrow=c(1,3))
#' barplot(cbind(1:5, rep(3,5)),  horiz = TRUE,  col=likert_col(5 , "RdBl"))
#' barplot(cbind(1:3, rep(3,3)),  horiz = TRUE,  col=likert_col(3 , "RdBl"))
#' barplot(cbind(1:8, rep(3,8)),  horiz = TRUE,  col=likert_col(8 , "RdBl"))
#'
#'
#'
likert_col <- function(n = 5,
                       name =  "RdBl" ,
                       # c("RdBl", "BlRd", "RdGr", "GrRd","GrBl", "BlGr","Bw"),
                       middle = mean(1:n),
                       middle.color =  "gray90") {
  
  # print(list(n = n,
  #            name = name,
  #            middle = middle,
  #            middle.color = middle.color))
  stp25settings:::likert_col(
    n = n,
    name = name,
    middle = middle,
    middle.color = middle.color
  )
}

 
# likert_plot <-
#   function(...,
#            main = '',
#            ylab = "",
#            sub = "",
#            xlab = if (as.percent) "Prozent" else "Anzahl",
#            ylim =NULL, xlim=NULL,
#            type = 1,
#            col = NULL,
#            rightAxis = FALSE,
#            as.percent = TRUE,
#            auto.key = list(space = space, columns = columns, between = 1),
#            ReferenceZero = include.reference,
#            reference.line.col = "gray65",
#            col.strip.background = "gray97",
#            wrap = TRUE,
#            columns = 2,
#            space = "top",
#            horizontal = TRUE,
#            #as.table = TRUE,
#            positive.order = NULL,
#           # reverse = ifelse(horizontal, as.table, FALSE),
#            between = list(x = 1 + (horizontal), y = 0.5 +2 * (!horizontal)),
#            par.strip.text = list(lines = 1, cex = .8),
#            par.settings = NULL,
#            include.reference = NULL,
#            include.total = FALSE,
#            relevel = NULL, relabel =NULL,
#            include.order = NULL,
#            decreasing =  TRUE,
# 
#            caption = "",
#            include.table = FALSE,
#            include.mean = TRUE,
#            include.n = FALSE,
#            include.na = FALSE,
#            include.percent = TRUE,
#            include.count = TRUE)
# {
# 
#   if(!is.null(positive.order))
#     stop("positive.order geht nicht mehr\n\n Neu ist include.order aber die Ergebnisse im plot sind anderst!!\n")
# 
#   if (is.null(relevel) & is.null(relabel)  ){
#     X <- Likert(..., include.total=include.total)
#    }
#   else if (!is.null(relevel)){
#     X_old <-  stp25tools::prepare_data2(...)
# 
#     X_old$data[X_old$measure.vars] <-
#       stp25tools::dapply2(
#         X_old$data[X_old$measure.vars],
#         fun = function(x) {
#           if (nlevels(x) == length(relevel))
#             levels(x) <- relevel
#           else
#             stop("\nDie relevel stimmen in der laenge nicht überein!\n")
#           x
#         }
#       )
#     X <- Likert(X_old$formula,  X_old$data, include.total=include.total)
#   }
#   else if (!is.null(relabel)){
#     X_old <-  stp25tools::prepare_data2(...)
#     
#     X_old$data[X_old$measure.vars] <-
#       stp25tools::dapply2(
#         X_old$data[X_old$measure.vars],
#         fun = function(x) {
#              factor(x, levels = relabel )
#         }
#       )
#     X <- Likert(X_old$formula,  X_old$data, include.total=include.total)
#   }
#   else {"Hier sollte ich nie landen"}
#   
#   
#  if (!is.null(include.order)) {
#     X$results <-  re_order_mean(X$results, X$m, decreasing, include.order)
#   }
# 
#  if(include.table){
#     stp25output2::Output(
#       Tbll_likert(X,
#                               include.reference = ReferenceZero,
#                               include.mean = include.mean,
#                               include.n = include.n,
#                               include.na = include.na,
#                               include.percent = include.percent,
#                               include.count = include.count
#                               ),
#       caption = caption
#     )}
# 
#  if( type !=1 ){
#   fm <- X$formula
#   x_in <- all.names(fm)
# 
#   if (length(x_in) == 5) {
#     X$formula <-  formula(paste(x_in[5], x_in[1], x_in[4], x_in[3], x_in[2]))
#   } else if (length(x_in) == 7) {
#     X$formula <-
#       formula(paste(x_in[6],  x_in[1], x_in[4], x_in[3], x_in[2], x_in[5], x_in[7]))
#   }
# }
# 
#  likertplot(
#     X,
#     main = main,
#     ylab = ylab,
#     sub = sub,
#     xlab = xlab,
#     ylim = ylim, xlim = xlim,
#     col = col,
#     rightAxis = rightAxis,
#   #  positive.order = positive.order,
#     as.percent = as.percent,
#     auto.key = auto.key,
#     ReferenceZero = ReferenceZero,
#     reference.line.col = reference.line.col,
#     col.strip.background = col.strip.background,
#     wrap = wrap,
#     horizontal = horizontal,
#   #  as.table = as.table,
#   #  reverse =reverse,
#     between = between,
#     par.strip.text = par.strip.text,
#   par.settings =par.settings
# 
# 
#   )
# 
# }

 
# re_order_mean <-
#   function(data,
#            m, # mittelwerte
#            decreasing = TRUE,
#            include.order,
#            item = "Item") {
#   if (is.logical(include.order) & include.order) {
#  
#       my_order <-
#         tapply(
#           m, data[[item]],
#           FUN = function(x) {mean(x, na.rm = TRUE)}
#         )
#   #  print(levels(data[[item]]))
# #print(my_order)
#       data[[item]] <-
#         factor(data[[item]], names(my_order)[order(my_order, decreasing = decreasing)])
#     }
#     else  if (is.numeric(include.order)) {
#       # Das ist glaube ich nicht mehr zu verwenden ??
#       positive.order <- FALSE
#       ny_levels <- levels(data[[item]])
#       if (length(ny_levels) != length(include.order))
#         stop(
#           "include.order ist die Reihenfolge der Items - muss also exakt gleich lang sein wie die Items!"
#         )
#       data[[item]] <-
#         factor(data[[item]], ny_levels[include.order])
#     }
#     
#     data
#   }










# likertplot(dat16, Question ~.)
#likertplot(Question ~., dat16, include.order = c(5,4,3,1,2))
# likertplot(Question ~., dat16, include.order = "l")
# likertplot(Question ~.|Sex, dat, include.order = TRUE,
#            include.total=TRUE)
# 
# 
# DF |>
#   Summarise_likert(  
#     q1,q2,q3,q4,q5,q6,
#     by = ~ sex,
#     include.total = TRUE,
#     include.order = TRUE) |>
#   likertplot( )

# DF |>
#   Summarise_likert(
#     q1,q2,q3,q4,q5,q6,
#     by = ~ sex,
#     include.total = TRUE,
#     include.order = TRUE) |>
#   likertplot()
# 
# 
# DF |>
#   likertplot(
#     q1+q2+q3+q4+q5+q6~ sex
#   ) 


