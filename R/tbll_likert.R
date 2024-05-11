#' Tbll_likert
#'
#' @param ...  Likert-Objekt oder data+Formula
#'
#' @return data.frame mit attributen fuer plotlikert
#' @export
#'
#' @examples
#'
#' #'set.seed(1)
#' n <- 100
#' lvs <- c("--", "-", "o", "+", "++")
#' DF2 <- data.frame(
#'   Magazines = cut(rnorm(n), 5, lvs),
#'   Comic.books = cut(rnorm(n), 5, lvs),
#'   Fiction = cut(rnorm(n), 5, lvs),
#'   Newspapers = cut(rnorm(n), 5, lvs),
#'   Geschlecht = cut(rnorm(n), 2, c("m", "f"))
#' )
#'
#' #x<-Tbll_likert(DF2, Magazines, Comic.books, Fiction, Newspapers, ReferenceZero=2)
#'
#' # stp25plot::likertplot(Item   ~ . , data = x)
#'
#' #
#' Tbll_likert(DF2, Magazines, Comic.books, Fiction, Newspapers,
#'             by=~Geschlecht,
#'             include.order=TRUE)
#'
Tbll_likert <- function(...){
  UseMethod("Tbll_likert")
}



#' @rdname Tbll_likert
#' @param exclude.levels position des zu excludierenden levels 
#' (zb 'exclude.levels = 5' ist das gleiche wie 'reorder.levels = -5')
#' @export
#'
Tbll_likert.default <- function(...,
                                include.reference = NULL,
                                include.mean = TRUE,
                                include.n = FALSE,
                                include.na = FALSE,
                                include.order = FALSE,
                                include.percent = TRUE,
                                include.count = TRUE,
                                include.total=FALSE,
                                exclude.levels = NULL,
                                decreasing = TRUE,
                                ReferenceZero = include.reference,
                                labels = c("low", "neutral", "high"),
                                reverse.levels = FALSE,
                                reorder.levels = NA) {
  rslt <-   Likert(
    ...,
    reverse.levels = reverse.levels,
    reorder.levels = reorder.levels,
    include.total = include.total,
    exclude.levels = exclude.levels
  )

  tbl <-  Tbll_likert.likert(
    rslt,
    ReferenceZero = ReferenceZero,
    include.mean = include.mean,
    include.n = include.n,
    include.na = include.na,
    include.order = include.order,
    include.percent = include.percent,
    include.count = include.count,
    labels = labels,
    decreasing = decreasing
  )

  attr(tbl, "plot") <- list(
    item = levels(rslt$results$Item),
    formula =  rslt$formula,
    results =  rslt$results,
    nlevels = rslt$nlevels,
    ReferenceZero = ReferenceZero,
    m = rslt$m
  )

tbl
}

#' @rdname Tbll_likert
#' @param x Likert - Objekt
#' @param include.reference,labels,ReferenceZero  numeric include.reference = 2 (drei Gruppen)
#' include.reference = 2.5 (zwei Gruppen)
#'  Neutrales Element in Kombination mit
#'  labels = c("low", "neutral", "high")
#' @param include.mean,include.n,include.na Zusatz Ergebnisse
#' @param include.order,decreasing sortierung nach mittelwert
#' @param include.percent,include.count Format Prozent/Anzahl
#
#' @param reverse.levels an Likert
#' @export
#'
Tbll_likert.likert <- function(x,
                               include.reference =NULL,
                               include.mean = TRUE,
                               include.n = FALSE,
                               include.na = FALSE,
                               include.order = FALSE,
                               include.percent = TRUE,
                               include.count = TRUE,
                               ReferenceZero = include.reference,
                               labels = c("low", "neutral", "high"),
                               decreasing = TRUE, ...) {


  note <- NULL # für include.reference
  if (!is.null(ReferenceZero)) {
    # x$freq und x$freq.na werden neu zudammengefasst
    if (is.character(ReferenceZero))
      ReferenceZero <- which(x$levels %in% ReferenceZero)
    else if (!is.numeric(ReferenceZero))
      ReferenceZero <- median(seq_len(x$nlevels))

    if (ceiling(ReferenceZero) == floor(ReferenceZero)) {
      lowrange <- seq_len((ReferenceZero - 1))
      neutral <- ReferenceZero
      highrange <- (ReferenceZero + 1):x$nlevels

      freq <- cbind(
        lowrange = RowSums2(x$freq[, lowrange]),
        neutral = x$freq[, neutral],
        highrange = RowSums2(x$freq[, highrange])
      )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "neutral:",
          paste(x$levels[neutral], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )

      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", ReferenceZero - 1, ")"),
          paste0(labels[2], "(", ReferenceZero, ")"),
          paste0(labels[3], "(", ReferenceZero + 1, ":", x$nlevels, ")")
        )
      x$freq <- freq

    } else{
      lowrange <- seq_len(floor(ReferenceZero))
      highrange <- ceiling(ReferenceZero):x$nlevels

      freq <-
        cbind(lowrange = RowSums2(x$freq[, lowrange]),
              highrange = RowSums2(x$freq[, highrange]))
      colnames(freq) <-
        c(
          paste0(labels[1], "(1:", floor(ReferenceZero), ")"),
          paste0(labels[3], "(", ceiling(ReferenceZero), ":", x$nlevels, ")")
        )
      if (is.null(note))
        note <-
        paste(
          "lowrange:",
          paste(x$levels[lowrange], "\n", collapse = "|"),
          "highrange:",
          paste(x$levels[highrange], collapse = "|")
        )
      x$freq <- freq
    }

    x$freq.na <- if (names(x$freq.na)[ncol(x$freq.na)] == "NA")
      cbind(freq, x$freq.na[ncol(x$freq.na)])
    else
      freq
  }




  if (include.na) {x$freq <- x$freq.na}

  if (include.percent) {
    if (include.count)
      x$freq <- stp25stat2:::rndr_percent(x$freq / x$n * 100, x$freq)
    else  x$freq <-
        stp25stat2:::rndr_percent(x$freq / x$n * 100)
  } else if (!include.count) { x$freq <- "" }

  if (include.n) {
    x$freq <- cbind(n = x$n, x$freq)
    }

  if (include.mean) {
    x$freq <- cbind(x$freq, 'M(SD)' = stp25stat2:::rndr_mean(x$m, x$sd))
    }

  ans <- cbind(x$names, x$freq)

  if (include.order) {
      ans <- ans[order(x$m, decreasing=decreasing),]
  }


  stp25stat2::prepare_output(ans,
                 caption = "Likert",
                 N = x$N)
}


#' @rdname Tbll_likert
#' @description
#' Likert: Auszählen der Häufigkeiten
#'
#' @param include.total logical oder string zB. include.total ="Alle"
#' @param reorder.levels integer factor(item, levels(item)[reorder.levels])
#' @param reverse.levels logical  rev(item)
#'
#' @return liste mit  results = data, sowie m, sd, n
#' @export
Likert <- function(...,
                   labels = NULL,
                   reverse.levels = FALSE,
                   reorder.levels = NA,
                   include.total = FALSE,
                   exclude.levels = NULL
                   ) {

  if(!is.null(exclude.levels )){
    if(!is.na( reorder.levels )) stop( " reorder.levels in kombination mit exclude.levels geht nicht!")
    reorder.levels <- exclude.levels * (-1)
  }
  
  if (!reverse.levels) {
    if (is.na(reorder.levels)) {
      results <-
        stp25stat2::Summarise(...,
          fun = function(x) {
            if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))
            table(x, useNA = "always")
            },
          key = "Item"
      )
      item_mean <-
        stp25stat2::Summarise(
          ...,
          fun = function(x)
            mean(as.numeric(x), na.rm = TRUE),
          key = "Item"
        )$value

      item_sd <-
        stp25stat2::Summarise(
          ...,
          fun = function(x)
            sd(as.numeric(x), na.rm = TRUE),
          key = "Item"
        )$value
    }
    else {
      results <-
        stp25stat2::Summarise(...,
        fun = function(x) {

          if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))

          if (is.numeric(reorder.levels))
            x <- factor(x, levels(x)[reorder.levels])
          else
            x <- factor(x, reorder.levels)

          table(x, useNA = "always")
        },
        key = "Item")

      item_mean <-
        stp25stat2::Summarise(
          ...,
          fun = function(x){
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)

            mean(as.numeric(x), na.rm = TRUE)
            },
          key = "Item")$value

      item_sd <-
        stp25stat2::Summarise(
          ...,
          fun = function(x){
            if (is.numeric(reorder.levels))
              x <- factor(x, levels(x)[reorder.levels])
            else
              x <- factor(x, reorder.levels)

            sd(as.numeric(x), na.rm = TRUE)
            },
          key = "Item")$value
    }
  }
  else { # reverse.levels
    results <-
      stp25stat2::Summarise(
        ...,
        fun = function(x) {

          if(is.logical(x)) x <- factor(x, c(FALSE, TRUE))
          x <- factor(x, rev(levels(x)))
          table(x, useNA = "always")
        },
        key = "Item")

    item_mean <-
      stp25stat2::Summarise(
        ...,
        fun = function(x) {
          mean(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
        },
        key = "Item"
      )$value

    item_sd <-
      stp25stat2::Summarise(...,
      fun = function(x) {
        sd(1 + nlevels(x) - as.numeric(x), na.rm = TRUE)
      },
      key = "Item" )$value
  }

  nms <- sapply(results, is.integer)
  ncl <- ncol(results)
  names(results)[ncl] <- "NA"
  col_names <- names(results[-ncl])
  pos_col_names <- grep("Item", col_names)
  str_total <- "Total"

  if (is.character(include.total)) {
    str_total <- include.total
    include.total <- TRUE
  }

  if (include.total) {
    dotts <- stp25tools::prepare_data2(...)
    rslt_total <-
      Likert(formula(paste(
        "~", paste(dotts$measure.vars, collapse = "+"))),
      dotts$data)

    rslt_total$results <- cbind(rslt_total$results[1], rslt_total$freq.na)
    results <- dplyr::bind_rows(results, rslt_total$results)
    results[[1]] <- factor(results[[1]], c(str_total, levels(results[[1]])))
    results[[1]][is.na(results[[1]])] <- str_total

    item_mean <- c(item_mean, rslt_total$m)
    item_sd <- c(item_sd, rslt_total$sd)
  }




  rslt <- list(
    results = results[-ncl],
    names =   results[-c(which(nms), ncl)],
    freq =    results[which(nms[-ncl])],
    freq.na = results[which(nms)],
    N =       sum(results[which(nms)]) / nlevels(results$Item),
    n =       as.vector(rowSums(results[which(nms[-ncl])])),
    m =       item_mean,
    sd =      item_sd,
    # Mittelwert = rndr_mean(item_mean, item_sd),
    # items =  data.frame(),#  grouping = NULL,
    formula =  if (pos_col_names == 1) {Item ~ .}
               else{formula(paste("Item ~ .|",
                                  paste(col_names[1:(pos_col_names - 1)], collapse = "+")))},
    nlevels = sum(nms) - 1,
    levels =  names(nms[-ncl])[nms[-ncl]]
  )
  class(rslt) <- c('likert', class(rslt))

  rslt
}



RowSums2 <- function(x)
  if (is.vector(x)) x else rowSums(x, na.rm = TRUE)


#' @rdname Tbll_likert
#' @export
print.likert<-function(x, ...){
  cat("\nnames: ", paste(names(x), collapse=", "),"\n")
  cat("\nresults:  \n ")
  print( head(x$results))
  cat("\nlevels: ", paste(x$levels, collapse=", "),"\n")
}


# my personal preference, when dealing with likert scales, is to complement the
# presentation of the detailed responses with the so-called Dominant Opinion Index
# (don't remember who first came up with the idea) : DOI = (% positive - % negative)
# x (% positive + % negative) = (% positive - % negative) x (100% - % neutral)
# # the formula becomes slightly more complicated if the intensity of opinion
# (e.g., agree vs strongly agree) is taken into account and the percentages are
# weighted based on that
#
# the index ranges from -100 (strongly negative) to +100 (strongly positive),
# with 0 midpoint as neutral
#
# details are there for whoever needs them (usually shown as diverging bars
# with extra neutrals), but I'm focused on the DOI
#
# https://jakec007.github.io/2021-06-23-R-likert/
# https://blog.datawrapper.de/divergingbars/


