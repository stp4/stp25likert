require(stp25likert)
#require(stp25stat2)

#'set.seed(1)
n <- 100
lvs <- c("--", "-", "o", "+", "++")
DF2 <- data.frame(
  Magazines = cut(rnorm(n), 5, lvs),
  Comic.books = cut(rnorm(n), 5, lvs),
  Fiction = cut(rnorm(n), 5, lvs),
  Newspapers = cut(rnorm(n), 5, lvs),
  Geschlecht = cut(rnorm(n), 2, c("m", "f"))
) |>  stp25tools::Label(Comic.books = "Comic-books")

dat_l <-
  Tbll_likert(DF2,
              Magazines,
              Comic.books,
              Fiction,
              Newspapers,
              ReferenceZero = 2.5)
dat_lg <- Tbll_likert(
  DF2,
  Magazines,
  Comic.books,
  Fiction,
  Newspapers,
  by =  ~ Geschlecht,
  include.order = TRUE
)
#likertplot(Item ~ . |Geschlecht  , data = dat_lg)
cowplot::plot_grid(
  likertplot(dat_l,
             auto.key = list(columns = 5)),
  likertplot(
    Item ~ . | Geschlecht,
    data = dat_lg,
    auto.key = list(columns = 5)
  ),
  ncol = 2
)


#library(ggstats)
library(dplyr)
library(ggplot2)

gglikert(
  DF2,
  c(Magazines,
    Comic.books,
    Fiction,
    Newspapers),
  facet_cols = vars(Geschlecht),
  labels_size = 3
)


dat_l |> likert_data() |>
  ggplot() +
  aes(
    x = Item,
    fill = levels,
    weight = Freq,
    by = Item
  ) +
  geom_bar(position = "fill") +
  geom_text(stat = "prop", position = position_fill(.5)) +
    coord_flip()



