options(width=160)
rm( list = setdiff(ls(),c("data","labels")))
cat("\f")

## @knitr load_packages
library(dplyr)
library(ggplot2)

## @knitr load_data
# data <- readRDS("./data.rds")
source("./0_enter_data.R")

## @knitr tweak_data

data <- data
str(data)
data$value <- as.numeric(data$value)-1
data$valueTF <- as.logical(data$value)
head(data)



# res$label2
# print(res, n=nrow(res))

## @knitr define_themes
baseSize <- 12
theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 

## @knitr counts
ds <- data %>%
  dplyr::group_by(wine, region, year) %>%
  summarize(count=n_distinct(.id))
print(ds, n=nrow(ds))

ds <- data %>%
  dplyr::group_by(wine, person) %>%
  summarize(count=n_distinct(.id))
t <- table(ds$wine, ds$person)
t[t==0] <- "."
t



## @knitr likability
ds <- data %>% dplyr::select(person, wine, region, year, like) %>%
  dplyr::group_by(person, wine, region, year) %>%
  dplyr::summarize(like=mean(like)) %>%
  dplyr::arrange(person,wine)
ds <- ds %>% dplyr::group_by(wine, region) %>%
  dplyr::summarize(like=mean(like)) 
ds$like <- round(ds$like,2)

g <- ggplot2::ggplot(ds, aes(x=wine,y=like, fill=region))
g <- g + geom_bar(stat="identity")
# g <- g + scale_y_discrete(limits=rev(labels))
g <- g + labs(title="Average likability across raters", x="Wine", y="Average likability")
# g <- g + facet_grid(.~sense)
# g <- g + scale_fill_gradient(low="white", high="red")
g <- g + geom_text(aes(label=like), vjust=1.5 )
g <- g + theme1
# g <- g + theme(axis.title.y = element_blank())
g



## @knitr summary_heat
ds <- data
res <- ds %>% dplyr::group_by(wine,label, sense) %>%
  dplyr::summarize(score=mean(value))
res$score <- round(res$score,2)
##

g <- ggplot2::ggplot(res, aes(x=wine,y=label, fill=score))
g <- g + geom_tile()
g <- g + scale_y_discrete(limits=rev(labels))
g <- g + labs(title="Average scores across raters ", x="Wine")
g <- g + facet_grid(.~sense)
g <- g + scale_fill_gradient(low="white", high="red")
g <- g + geom_text(aes(label=score))
g <- g + theme1
g <- g + theme(axis.title.y = element_blank())
g

# ## @knitr aroma_heat
# ds <- data[data$sense=="aroma",]
# res <- ds %>%  
#   dplyr::group_by(wine,label) %>%
#   dplyr::summarize(score=mean(value))
# g <- ggplot2::ggplot(res, aes(x=wine,y=label, fill=score))
# g <- g + geom_tile()
# g <- g + scale_y_discrete(limits=rev(labels))
# g <- g + labs(title="Summary scores across raters", x="Wine")
# # g <- g + facet_grid(.~sense)
# g <- g + scale_fill_gradient(low="white", high="red")
# g <- g + geom_text(aes(label=score))
# g <- g + theme1
# g <- g + theme(axis.title.y = element_blank())
# g
# 
# ## @knitr flavour_heat
# ds <- data[data$sense=="flavour",]
# res <- ds %>%  
#   dplyr::group_by(wine,label) %>%
#   dplyr::summarize(score=mean(value))
# g <- ggplot2::ggplot(res, aes(x=wine,y=label, fill=score))
# g <- g + geom_tile()
# g <- g + scale_y_discrete(limits=rev(labels))
# g <- g + labs(title="Summary scores across raters", x="Wine")
# # g <- g + facet_grid(.~sense)
# g <- g + scale_fill_gradient(low="white", high="red")
# g <- g + geom_text(aes(label=score))
# g <- g + theme1
# g <- g + theme(axis.title.y = element_blank())
# g

## @knitr both_heat
ds <- data
res <- ds %>%  
  dplyr::group_by(wine,label) %>%
  dplyr::summarize(score=mean(value))
res$score <- round(res$score,2)

g <- ggplot2::ggplot(res, aes(x=wine,y=label, fill=score))
g <- g + geom_tile()
g <- g + scale_y_discrete(limits=rev(labels))
g <- g + labs(title="Average scores across raters (both aromas and flavours)", x="Wine")
# g <- g + facet_grid(.~sense)
g <- g + scale_fill_gradient(low="white", high="red")
g <- g + geom_text(aes(label=score))
g <- g + theme1
g <- g + theme(axis.title.y = element_blank())
g
# table(ds$valueTF, ds$wine)




