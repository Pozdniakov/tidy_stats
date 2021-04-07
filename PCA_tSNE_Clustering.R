library(tidyverse)

plot(iris %>% select(!Species), col=iris$Species)

iris %>%
  select(!Species) %>%
  cor()

iris_pr <- iris %>%
  select(!Species) %>%
  prcomp(center = TRUE, scale. = TRUE)

summary(iris_pr)

plot(iris_pr)

plot(iris_pr$x[,1:2], col=iris$Species)

library(ggfortify)
autoplot(iris_pr, data = iris, colour = "Species", loadings = TRUE, loadings.label = TRUE)



# t-SNE -------------------------------------------------------------------

mnist <- read.csv("https://raw.githubusercontent.com/Pozdniakov/tidy_stats/master/data/mnist_train.csv")

pca_fit <- mnist %>%
  select(-1) %>%
  prcomp()

summary(pca_fit)
plot(pca_fit)

pal_set1_brewer <- RColorBrewer::brewer.pal(10, "Set3")

plot(pca_fit$x[, 1:2], main = "PCA", t = "n")
text(pca_fit$x, labels=mnist$label, col = pal_set1_brewer[mnist$label + 1])


#install.packages("Rtsne")
library(Rtsne)
rtsne_fit <- mnist %>%
  select(-1) %>%
  Rtsne(verbose=TRUE, max_iter = 500)


plot(rtsne_fit$Y, t = "n", main = "tsne")
text(rtsne_fit$Y, labels=mnist$label, 
     col = pal_set1_brewer[mnist$label + 1])



# clustering --------------------------------------------------------------

iris_3means <- kmeans(iris %>% select(!Species), centers = 3)
table(iris$Species, iris_3means$cluster)
plot(iris %>% select(!Species), col = iris_3means$cluster,
     pch = as.numeric(as.factor(iris$Species)))

