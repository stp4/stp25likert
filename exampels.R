require(stp25likert)
#require(stp25stat2)
Multi <- dummy_multi_data(245)
Likert <- dummy_likert_data(245)
Likert |> Tbll_likert(q1, q2, q3,
             by = ~ Sex,
            include.order=TRUE)
Likert |>
  Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9,
                         by =~ Sex,
                         #  include.total = TRUE,
                         grouping = list(
                           FC.2 = c("q1", "q2"),
                           FC.3 = c("q3","q4", "q5", "q6"),
                           FC.4 = c("q7", "q8", "q9")
                         )) |>   gg_likertplot( include.order = "r" ) 

Likert |>
  Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9
                    
  ) -> dat
 
cowplot::plot_grid(
  Likert |>
    Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9) |>
    gg_likertplot(),
  Likert |>
    Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9) |>
    gg_likertplot(include.order = "r"),
  Likert |>
    Summarise_likert(q1, q2, q3, q4, q5, q6, q7, q8, q9) |>
    likertplot(),
  Likert |>
    Summarise_likert(q1, q2, q3, q4, q5, q6, q7, q8, q9) |>
    likertplot(include.order = "r")
)
  
  

cowplot::plot_grid(
Likert |>
  Summarise_likert_long(q1, q2, q3, q4, q5, q6, q7, q8, q9,
                        grouping = list(
                          FC.2 = c("q1", "q2"),
                          FC.3 = c("q3","q4", "q5", "q6"),
                          FC.4 = c("q7", "q8", "q9")
                        )) |>   gg_likertplot( include.order = "r" ) ,
  
Likert |>
  Summarise_likert(q1,q2,q3,q4,q5,q6,q7,q8,q9,
    grouping = list(
      FC.2 = c("q1", "q2"),
      FC.3 = c("q3", "q4", "q5", "q6"),
      FC.4 = c("q7", "q8", "q9")
    )
  ) |>
  likertplot(   Item ~ . | .grouping,
    include.order = "r",

   scales = list(y = list(relation = "free")),
     layout = c(1, 3),
    between = list(y = 0)
    #   strip = FALSE,
    #  strip.top = strip.custom(bg = "gray97"),
    #  strip.left = strip.custom(bg = "gray97"),
    #  par.strip.text = list(cex = .8, lines = 5),
   # main = "Ernärung",
  #  ylab = NULL,
   # wrap = FALSE
  )
)


Multi |>
  Summarise_multi_long(
    q1,q2,q3,q4,q5,q6,q7,q8,q9,
    by =  ~ Sex,
    grouping = list(
      FC.2 = c("q1", "q2"),
      FC.3 = c("q3","q4", "q5", "q6"),
      FC.4 =  c( "q7", "q8", "q9")
    
    )
  ) -> Multi_sum


Multi_sum |>
  gg_likert_stacked(.grouping ~ Sex, include.order=TRUE)  
  #  theme_bw() +
#  theme(strip.text.y = element_text(angle = 0))
Multi  |>
 stp25plot::multi_barplot(  q1 + q2 + q3 + q4 + q5 + q6 + q7 + q8 + q9~ Sex )
 
#Multi_sum |> likert_stacked(.grouping ~ Sex, include.order=TRUE, layout=c(2,3)) 
 
# dat |>  gg_likertplot(include.order = TRUE)
# dat |> Tbll_likert()
# dat

 