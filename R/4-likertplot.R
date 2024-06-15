#' Barcharts for Likert
#'
#' Constructs and plots diverging stacked barcharts for Likert (copie from HH:::plot.likert.formula)
#'
#' Die orginale Funktion hat bei der Sortierung (positive.order) einen Fehler.
#'
#' @param x formula
#' @param data daten
#' @param main,ylab,sub,xlab Beschriftung
#' @param col HH::brewer.pal.likert
#' @param wrap Zeien Umbrechen
#' farbe("likert.blue.red", data$nlevels, middle = ReferenceZero)
#' @param rightAxis,as.percent,ReferenceZero,reference.line.col,col.strip.background
#' an HH:::plot.likert.formula
#' @param include.order,decreasing Sortieren der Items
#' @param positive.order das nicht verwenden!! - wird ueber Tbll_likert oder include.order = TRUE gesteuert.
#' @param auto.key,columns,space  columns = 2,
#' @param ... HH:::plot.likert.formula
#'    between=list(x=0))
#'
#' @return lattice Plot
#' @export
#'
#' @examples
#'
#' #require(stp25plot)
#' require(stp25stat2)
#' set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = gl(length(lvs), 1, n, lvs),
#'   Comic.books = gl(length(lvs), 2, n, lvs),
#'   Fiction = gl(length(lvs), 3, n, lvs),
#'   Newspapers = gl(length(lvs), 5, n, lvs)
#' )
#'
#'
#'
#' DF2$Comic.books[sample.int(n / 2)] <- lvs[length(lvs)]
#' DF2$Newspapers[sample.int(n / 2)] <- lvs[1]
#' DF2$Magazines[sample.int(n / 2)] <- lvs[2]
#'
#' DF2 <- transform(DF2, Geschlecht = cut(rnorm(n), 2, Hmisc::Cs(m, f)))
#' Res1 <- Tbll_likert( ~ ., DF2[, -5])
#' Res2 <- Tbll_likert(. ~ Geschlecht, DF2)
#'
#' # require(HH)  # ?likertplot
#' # class(Res2)
#' # windows(7, 3)
#' # attr(Res2, "plot")$results
#'
#' likertplot(Item ~ . | Geschlecht,
#'            data = Res2,
#'             between=list(x=0))
#'
#' # col = likert_col(attr(data, "plot")$nlevels, middle = ReferenceZero)
#'
#' DF2 %>% likert_plot(Magazines, Comic.books, Fiction, Newspapers,
#'                     relevel = letters[1:5],
#'                     ReferenceZero = 1.5,
#'                     columns=5)
#'
#' # DF2 %>%
#' #   Likert(Magazines, Comic.books, Fiction, Newspapers) %>%
#' #   likertplot()
#'
#' sim_factor <-
#' function(n = 10,
#'          labels = c("--", "-", "o", "+", "++"),
#'          levels = seq_along(labels)) {
#'   factor(sample(levels, n, replace = TRUE), levels, labels)
#' }
#'
#'
#' DF <- data.frame(
#'   Employment.sector =
#'     sim_factor(
#'       n,
#'       c(
#'         "Academic (nonstudent)",
#'         "Business and industry",
#'         "Federal, state, and local government",
#'         "Private consultant/self-employed",
#'         "Other (including retired, students, not employed, etc.)")),
#'   Race = sim_factor(n, c("White","Asian","Black or African American","Other")),
#'   Education = sim_factor(n, c("Associate's and Bachelor's","Master's and Above")),
#'   Gender = sim_factor(n, c("Male","Female")),
#'   Prof.Recog = sim_factor(n, c("Not Important", "Important")),
#'   Question = sim_factor(n,
#'                         c(
#'                           "Strongly Disagree",
#'                           "Disagree" ,
#'                           "No Opinion",
#'                           "Agree Strongly", "Agree"
#'                         ))
#'
#' )
#' All <- DF |> Summarise(Question, fun =table)
#' Employment.sector <- DF |> Summarise(Question~ Employment.sector, fun =table)
#' Race <- DF |> Summarise(Question~ Race, fun =table)
#' Gender <- DF |> Summarise(Question~ Gender, fun =table)
#' Education <- DF |> Summarise(Question~ Education, fun =table)
#' Prof.Recog <- DF |> Summarise(Question~ Prof.Recog, fun =table)
#'
#'
#'
#' DF2 <-
#'   Rbind2(All, Employment.sector, Race, Gender, Education, Prof.Recog,
#'          .id = "Subtable") |>
#'   mutate(
#'     Question = dplyr::coalesce(Employment.sector, Race, Gender, Education, Prof.Recog ),
#'     Question = as.character(Question),
#'     Subtable = as.character(Subtable)
#'   ) |>
#'   select(Subtable, Question, `Strongly Disagree`, Disagree, `No Opinion`, `Agree Strongly`, Agree)
#'
#'
#' DF2$Question[1] <- " All Survey Responses"
#' # DF2$Question = factor(DF2$Question)
#' # DF2$Subtable = factor(DF2$Subtable)
#' stp25plot::likertplot(Question ~ . | Subtable, DF2,
#'                       scales=list(y=list(relation="free")), layout=c(1,6),
#'                       #  positive.order=TRUE,
#'                       between=list(y=0),
#'                       strip=FALSE, strip.left=strip.custom(bg="gray97"),
#'                       par.strip.text=list(cex=.6, lines=5),
#'                       main="Is your job professionally challenging?",
#'                       ylab=NULL,
#'                       wrap =FALSE,
#'                       sub="This looks better in a 10inx7in window")
likertplot <-
  function(x = Item   ~ . ,
           data = NULL,
           main = '',
           ylab = "",
           sub = "",
           xlab = if (horizontal) { if (as.percent)"Prozent" else "Anzahl" } else "",
           ylim =NULL, xlim=NULL,
           col = NULL,
           rightAxis = FALSE,
           positive.order = NULL,
           include.order = NULL,
           decreasing =  TRUE,
           as.percent = TRUE,
           auto.key = list(space = space, columns = columns, between = 1),
           ReferenceZero = NULL,
         #  include.neutral = TRUE,
           reference.line.col = "gray65",
           col.strip.background = "gray97",
           wrap = TRUE,
           columns = 2,
           space = "top",
           horizontal = TRUE,
           between = list(x = 1 + (horizontal), y = 0.5 + 2 * (!horizontal)),
           par.settings  = NULL,
           ...) {

  if(!is.null(positive.order))
    stop("positive.order geht nicht mehr\n\n Neu ist include.order aber die Ergebnisse im plot sind anderst!!\n")

  name_item <- "Item"
  x_mean    <- NULL
#cat("\nReferenceZero:", ReferenceZero )


  if(is.null(data)){
    if(is.data.frame(x) & ("plot" %in% names(attributes(x))) ){
      formula <- attr(x, "plot")$formula
      nlevels <- attr(x, "plot")$nlevels
      x_mean  <- attr(x, "plot")$m
      if(!is.null( attr(x, "plot")$ReferenceZero) )
        ReferenceZero <- attr(x, "plot")$ReferenceZero
      data    <- attr(x, "plot")$results

    }
    else if(inherits(x, "likert")){
    #  cat ("\n 1 in likert")
      formula <- x$formula
      nlevels <- x$nlevels
      data    <- x$results
      x_mean  <- x$m
    }
    else{ stop("No data.frame !") }
  }
  else if (plyr::is.formula(x)) {
   if (is.data.frame(data) ) {
     if("plot" %in% names(attributes(data))){

       if(!is.null( attr(data, "plot")$ReferenceZero) )
         ReferenceZero <- attr(data, "plot")$ReferenceZero
       formula <- x
       nlevels <- attr(data, "plot")$nlevels
       x_mean  <- attr(data, "plot")$m

       data    <- attr(data, "plot")$results


     }
     else{
       formula <- x
       name_item <- all.vars(x)[1]
       nlevels<- length(names(data)) - length(all.vars(x)) + 1
     }
   }
    else if( inherits(x, "likert") ){
       formula <- x
       nlevels <- data$nlevels
       data    <- data$results
       x_mean  <- x$m
      }
  }


  if (is.null(col)) {
    col <- if (is.null(ReferenceZero)) likert_col(nlevels)
           else likert_col(nlevels, middle = ReferenceZero)
  }


  if (is.logical(wrap)) {
    if (wrap) {
      data[[name_item]] <-
        stp25tools::wrap_factor(data[[name_item]], 35)
    }
  }
  else if (is.numeric(wrap)) {
    data[[name_item]] <-
      stp25tools::wrap_factor(data[[name_item]], wrap)
  }

  if (!is.null(include.order)) {
   data <- re_order_mean(data, x_mean, decreasing, include.order)
  }


  lattice_plot <-
    HH:::plot.likert.formula(
      x = formula,
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



#' @rdname likertplot
#'
#' @param ... an stp25stat2::Likert
#' @param include.table,include.mean,include.n,include.percent,include.count,include.na,caption Tabelle ausgeben (output ist eine Altlast)
#' @param type  nur in likert_plot Bei Gruppen Items als Zeilen => 1, oder Gruppen als Zeilen => 2
#' @param include.total an stp25stat2:::Likert
#' @param include.order sortiere muss die länge der Items entsprechen
#' @param relevel  Uberschreibt die levels levels(x) <- relevel ist nur nur in likert_plot vorhanden
#' @param relabel ändert die Sortierung
#' @param par.strip.text an  HH:::plot.likert.formula
#' @return HH likertplot (lattice-Plot)
#' @export
#'
#' @examples
#'
#' DF2 %>% likert_plot(Magazines, Comic.books, Fiction, Newspapers)
#'
likert_plot <-
  function(...,
           main = '',
           ylab = "",
           sub = "",
           xlab = if (as.percent) "Prozent" else "Anzahl",
           ylim =NULL, xlim=NULL,
           type = 1,
           col = NULL,
           rightAxis = FALSE,
           as.percent = TRUE,
           auto.key = list(space = space, columns = columns, between = 1),
           ReferenceZero = include.reference,
           reference.line.col = "gray65",
           col.strip.background = "gray97",
           wrap = TRUE,
           columns = 2,
           space = "top",
           horizontal = TRUE,
           #as.table = TRUE,
           positive.order = NULL,
          # reverse = ifelse(horizontal, as.table, FALSE),
           between = list(x = 1 + (horizontal), y = 0.5 +2 * (!horizontal)),
           par.strip.text = list(lines = 1, cex = .8),
           par.settings = NULL,
           include.reference = NULL,
           include.total = FALSE,
           relevel = NULL, relabel =NULL,
           include.order = NULL,
           decreasing =  TRUE,

           caption = "",
           include.table = FALSE,
           include.mean = TRUE,
           include.n = FALSE,
           include.na = FALSE,
           include.percent = TRUE,
           include.count = TRUE)
{

  if(!is.null(positive.order))
    stop("positive.order geht nicht mehr\n\n Neu ist include.order aber die Ergebnisse im plot sind anderst!!\n")

  if (is.null(relevel) | is.null(relabel)  ){
    X <- stp25stat2:::Likert(..., include.total=include.total)
   }
  else if (!is.null(relevel)){
    X_old <-  stp25tools::prepare_data2(...)

    X_old$data[X_old$measure.vars] <-
      stp25tools::dapply2(
        X_old$data[X_old$measure.vars],
        fun = function(x) {
          if (nlevels(x) == length(relevel))
            levels(x) <- relevel
          else
            stop("\nDie relevel stimmen in der laenge nicht überein!\n")
          x
        }
      )
    X <- stp25stat2:::Likert(X_old$formula,  X_old$data, include.total=include.total)
  }
  else if (!is.null(relabel)){
    X_old <-  stp25tools::prepare_data2(...)
    
    X_old$data[X_old$measure.vars] <-
      stp25tools::dapply2(
        X_old$data[X_old$measure.vars],
        fun = function(x) {
             factor(x, levels = relabel )
        }
      )
    X <- stp25stat2:::Likert(X_old$formula,  X_old$data, include.total=include.total)
  }
  else {"Hier sollte ich nie landen"}
  
  
  
  
  
  


 if (!is.null(include.order)) {
    X$results <-  re_order_mean(X$results, X$m, decreasing, include.order)
  }

 if(include.table){
    stp25output2::Output(
      stp25stat2::Tbll_likert(X,
                              include.reference = ReferenceZero,
                              include.mean = include.mean,
                              include.n = include.n,
                              include.na = include.na,
                              include.percent = include.percent,
                              include.count = include.count
                              ),
      caption = caption
    )}

 if( type !=1 ){
  fm <- X$formula
  x_in <- all.names(fm)

  if (length(x_in) == 5) {
    X$formula <-  formula(paste(x_in[5], x_in[1], x_in[4], x_in[3], x_in[2]))
  } else if (length(x_in) == 7) {
    X$formula <-
      formula(paste(x_in[6],  x_in[1], x_in[4], x_in[3], x_in[2], x_in[5], x_in[7]))
  }
}

 likertplot(
    X,
    main = main,
    ylab = ylab,
    sub = sub,
    xlab = xlab,
    ylim = ylim, xlim = xlim,
    col = col,
    rightAxis = rightAxis,
  #  positive.order = positive.order,
    as.percent = as.percent,
    auto.key = auto.key,
    ReferenceZero = ReferenceZero,
    reference.line.col = reference.line.col,
    col.strip.background = col.strip.background,
    wrap = wrap,
    horizontal = horizontal,
  #  as.table = as.table,
  #  reverse =reverse,
    between = between,
    par.strip.text = par.strip.text,
  par.settings =par.settings


  )

}

re_order_mean <-
  function(data, m, decreasing = TRUE, include.order) {
    if (is.logical(include.order) & include.order) {
      my_order <-
        tapply(
          m,
          data$Item,
          FUN = function(x)
            mean(x, na.rm = TRUE)
        )

      data$Item <-
        factor(data$Item ,
               names(my_order)[order(my_order, decreasing = decreasing)])
    }
    else  if (is.numeric(include.order)) {
      positive.order <- FALSE
      ny_levels <- levels(X$results$Item)
      if (length(ny_levels) != length(include.order))
        stop(
          "include.order ist die Reihenfolge der Items - muss also exakt gleich lang sein wie die Items!"
        )
      X$results$Item <-
        factor(X$results$Item, ny_levels[include.order])
    }



    data
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

