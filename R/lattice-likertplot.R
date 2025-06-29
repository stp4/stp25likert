#' Lattice-Barcharts for Likert and Multi-Response
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
#' @param rightAxis,as.percent,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param include.order,decreasing logical or character, Sortieren der Items
#' @param positive.order das nicht verwenden!! - wird ueber Tbll_likert oder include.order = TRUE gesteuert.
#' @param auto.key,columns,space  columns = 2,
#' @param ... HH:::plot.likert.formula  between=list(x=0))
#' 
#' @param nlevels,m_weight,groups,items,measure,grouping Internal
#' @param  include.reference,reverse.levels,include.total logical Likert
#' @param use.level numeric Multi
#' @param ylim,xlim,between,horizontal,par.settings,stack,border to lattice
#'
#'
#' @return  lattice barchart
#' @export
#' @importFrom HH plot.likert 
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
      if (!is.null(attr(data, "tbll")$include.reference))
        include.reference <- attr(data, "tbll")$include.reference
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
    include.reference = include.reference,
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
          
           include.reference = NULL,
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
    col <- if (is.null(include.reference)) likert_col(n=nlevels)
           else likert_col(n=nlevels, middle = include.reference)
  }
    
  if (!is.null(include.order)) {
      data <- order_weighted_mean(
        data, x,
        measure = measure,
        items = items,
        groups = groups,
        nlevels = nlevels,
        include.reference = include.reference,
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
  #  HH:::plot.likert.formula(
    HH::plot.likert(
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
      
      ReferenceZero =  include.reference,
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




#' @rdname likertplot
#' 
#' @return lattice barchart
#' @import lattice
#' @export
#' 
#' @param include.percent,margin Prozent darstellen margin = 1 bzw bei Gruppen  margin = 1:2
#' 
#' col = RColorBrewer::brewer.pal(2, "BrBG")
#' 
#' @param x formula  ~ Freq | Group
#' 
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
           
           stack = TRUE,
           border = "gray90",
           col = NULL,
           # rightAxis = FALSE,
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
    if (nlevels < 3) nlevels <- 3
    
    if (!is.null(include.order))
      data <- order_weighted_mean(
        data,
        include.reference = NULL,
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
      data <- as.data.frame(prop.table(
        xtabs(fm, data), 
        margin = margin))
      data$Freq <- data$Freq * 100
      xlab <- "Percent"
    }
    if (length(all.names(x)) == 3) {
      x <- reformulate("Freq", x[[2]])
    } else{
      rhs <-   paste(all.vars(x[[3]])[-1], collapse = "+")
      x <-  as.formula(paste(x[[2]], "~Freq|", rhs))
    }
    
    
    if (!grouping) {
      lattice::barchart(
        x,
        data,
        groups = levels,
        main = main,
        xlab = xlab,
        stack = stack,
        border = border,
        col =  if (is.null(col))
          RColorBrewer::brewer.pal(nlevels, "BrBG")
        else
          col,
        par.settings  = par.settings
      )
    }
    else {
      ngr <-  length(attr(data, "tbll")$groups)
      ngr2 <- length(unique(data$.grouping))
      lattice::barchart(
        x,
        data,
        groups = levels,
        main = main,
        xlab = xlab,
        stack = stack,
        border = border,
        col =  if (is.null(col))
          RColorBrewer::brewer.pal(nlevels, "BrBG")
        else
          col,
        scales = list(y = list(relation = "free")),
        layout = c(ngr, ngr2),
        between = list(y = 0),
        par.settings  = par.settings
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
  stp25settings:::likert_col(
    n = n,
    name = name,
    middle = middle,
    middle.color = middle.color
  )
}


