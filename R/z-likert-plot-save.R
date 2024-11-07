# stp25plot::likertplot <- 
#   function(x = Item   ~ . ,
#            data,
#            main = '',
#            ylab = "",
#            sub = "",
#            xlab = if(horizontal) {if(as.percent) "Prozent" else "Anzahl"} else "",
#            col = NULL,
#            rightAxis = FALSE,
#            positive.order = FALSE, 
#            as.percent = TRUE,
#            auto.key = list(space = space, 
#                            columns = columns,
#                            between = 1
#            ),
#            
#            ReferenceZero = NULL,
#            reference.line.col = "gray65",
#            col.strip.background = "gray97",
#            wrap = TRUE,
#            columns = 2,
#            space ="top",
#            horizontal = TRUE,
#            between = list(x = 1 + (horizontal), 
#                           y = 0.5 +2 * (!horizontal)),
#            #  between.key = 1,
#            ...) {
#     
#     if (is.data.frame(x)) {
#       data <- x
#       x <- attr(x, "plot")$formula
#     }
#     
#     
#     if (!is.null(attr(data, "plot"))) {
#       
#       if (is.null(ReferenceZero)) {
#         ReferenceZero <-  attr(data, "plot")$ReferenceZero
#       }
#       
#       
#       if (is.null(col)) {
#         col <- if (is.null(ReferenceZero))
#           likert_col(attr(data, "plot")$nlevels)
#         else
#           likert_col(attr(data, "plot")$nlevels,
#                      middle = ReferenceZero)
#       }
#       
#       
#       if (is.logical(wrap)) {
#         if (wrap) {
#           attr(data, "plot")$results$Item <-
#             stp25tools:::wrap_sentence(as.character(attr(data, "plot")$results$Item),
#                                        35)
#           
#         }
#       } 
#       else{
#         attr(data, "plot")$results$Item <-
#           stp25tools:::wrap_sentence(as.character(attr(data, "plot")$results$Item),
#                                      wrap)
#         
#       }
#       
#       HH:::plot.likert.formula(
#         x,
#         attr(data, "plot")$results,
#         main = main,
#         ylab = ylab,
#         sub = sub,
#         xlab = xlab,
#         col = col,
#         rightAxis = rightAxis,
#         positive.order = positive.order,
#         as.percent = as.percent,
#         auto.key = auto.key,
#         ReferenceZero =  ReferenceZero,
#         reference.line.col = reference.line.col,
#         col.strip.background = col.strip.background,
#         between=between,
#         horizontal = horizontal,
#         ...
#       )
#     }
#     else if (inherits(data, "likert")) {
#       
#       
#       if(is.null(ReferenceZero)){ 
#         ReferenceZero<-  data$ReferenceZero 
#       } 
#       
#       
#       if (is.null(col)){
#         col <- if (is.null(ReferenceZero))
#           likert_col(data$nlevels)
#         else
#           likert_col(data$nlevels,
#                      middle = ReferenceZero)}
#       
#       
#       HH:::plot.likert.formula(
#         x,
#         data$results,
#         
#         main = main,
#         ylab = ylab,
#         sub = sub,
#         xlab = xlab,
#         col = col,
#         rightAxis = rightAxis,
#         positive.order = positive.order,
#         as.percent = as.percent,
#         auto.key = auto.key,
#         ReferenceZero = ReferenceZero,
#         reference.line.col = reference.line.col,
#         col.strip.background = col.strip.background,
#         between,
#         horizontal=horizontal,
#         ...
#       )
#     }
#     else{
#       if (is.null(col))
#         col <-  stp25plot::farbe("likert.blue.red", 5)
#       HH:::plot.likert.formula(
#         x,
#         data,
#         main = main,
#         ylab = ylab,
#         sub = sub,
#         xlab = xlab,
#         col = col,
#         rightAxis = rightAxis,
#         positive.order = positive.order,
#         as.percent = as.percent,
#         auto.key = auto.key,
#         ReferenceZero = ReferenceZero,
#         reference.line.col = reference.line.col,
#         col.strip.background = col.strip.background,
#         ...
#       )
#     }
#     
#     
#   }
# stp25plot::likert_plot<- 
# function(...,
#          main = '',
#          ylab = "",
#          sub = "",
#          xlab = if(as.percent) "Prozent" else "Anzahl",
#          col = NULL,
#          rightAxis = FALSE,
#          positive.order = FALSE, 
#          as.percent = TRUE,
#          auto.key = list(space = space, 
#                          columns = columns,
#                          between = 1
#          ),
#          ReferenceZero = NULL,
#          reference.line.col = "gray65",
#          col.strip.background = "gray97",
#          wrap = TRUE,
#          columns = 2,
#          space ="top",
#          relevel = NULL,
#          horizontal = TRUE,
#          between = list(x = 1 + (horizontal), 
#                         y = 0.5 +2 * (!horizontal)),
#          par.strip.text = list(lines = 1, cex = .8)
# ){
#   if (is.null(relevel))
#     X <- stp25stat2::Tbll_likert(...)
#   else{
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
#     X <- stp25stat2::Tbll_likert(X_old$formula,  X_old$data)
#   }
#   
#   
#   # attr(tbl, "plot") <- list(
#   #   item = levels(rslt$results$Item),
#   #   formula =  rslt$formula,
#   #   results =  if(is.null(attr(tbl, "plot")$order)) rslt$results
#   #   else rslt$results[attr(tbl, "plot")$order,],
#   #   nlevels = rslt$nlevels,
#   #   ReferenceZero = ReferenceZero
#   # )
#   
#   
#   stp25plot::likertplot(
#     X,
#     main = main,
#     ylab = ylab,
#     sub = sub,
#     xlab = xlab,
#     col = col,
#     rightAxis = rightAxis,
#     positive.order = positive.order,
#     as.percent = as.percent,
#     auto.key = auto.key,
#     ReferenceZero = ReferenceZero,
#     reference.line.col = reference.line.col,
#     col.strip.background = col.strip.background,
#     wrap = wrap,
#     horizontal = horizontal,
#     between = between,
#     par.strip.text = par.strip.text
#     #columns = 2,
#     # space ="top"
#   )
# }