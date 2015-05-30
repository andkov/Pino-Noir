options(width=160)
rm(list=ls())
cat("\f")

## @knitr load_packages
library(dplyr)

## @knitr define_labels
fruit <- c("Plum", "Cherry","Currant","Tomato", "Raspberry", "Strawberry")
oak <- c("Oak","Vanilla", "Smoke", "Pepper", "Coconut", "Cinnamon")
complex <- c("Earth", "Coffee", "Leather", "Tobacco","Chocolate","Mushroom")

labels <- c(fruit, oak, complex)

## @knitr define_wines
wine1 <- list("Name"= "Innocent Bystander",
              "Region"="Australia",
              "Year"="2006"); 
wine2 <- list("Name"= "Lucia Highlands",
              "Region"="California",
              "Year"="2009"); 
wine3 <- list("Name"= "Kris",
              "Region"="New Zealand",
              "Year"="2007"); 
wine4 <- list("Name"= "Innocent Bystander II",
              "Region"="Australia II",
              "Year"="2002"); 
wine5 <- list("Name"= "Lucia Highlands II",
              "Region"="California II",
              "Year"="2001"); 
wine6 <- list("Name"= "Kris II",
              "Region"="New Zealand II",
              "Year"="2003"); 

wines <- list(wine1, wine2, wine3)
names(wines) <- paste0("wine",1:length(wines))

data <- list()

## @knitr define_compiling_function
compile.data <- function(wineID, personID){
  hat <- list("wine"=wine,"PersonID"=person)
    value <- c(  aroma_fruit,   aroma_oak,   aroma_complex,
             flavour_fruit, flavour_oak, flavour_complex)
  label <- c(labels, labels)
  sense <- c(rep("aroma", length(labels)), rep("flavour", length(labels)))
  group <- c( rep("fruit", length(fruit)),rep("oak", length(oak)),rep("complex", length(complex)) )
  ds <- as.data.frame(cbind(value, label))
  ds$person <- hat[["PersonID"]]
  ds$wine <- hat[["wine"]][["Name"]]
  ds$region <- hat[["wine"]][["Region"]]
  ds$year <- hat[["wine"]][["Year"]]
  ds$sense <- sense
  ds$group <- group
  ds$like <- likability
  ds <- ds %>% dplyr::select(person, wine, region, year, sense, group, label, value, like)
  # data[[paste0( hat[["wine"]][["Name"]]," by ", hat[["PersonID"]])]]
  return(ds)
}


dtos <- list()
## @knitr enter_data

# #################################
# wine <- "wine"
# person <- "person"
# aroma_fruit     <- c()
# aroma_oak       <- c()
# aroma_complex   <- c()
# flavour_fruit   <- c()
# flavour_oak     <- c()
# flavour_complex <- c()
# dtos[[wine]][[person]] <- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Ken"
aroma_fruit     <- c(0,1,0,1,1,0)
aroma_oak       <- c(1,0,1,1,1,0)
aroma_complex   <- c(1,0,0,0,0,1)
flavour_fruit   <- c(1,1,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(1,1,1,1,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)


################################# 
wine <- wine2
person <- "Bob"
aroma_fruit     <- c(0,1,1,1,0,1)
aroma_oak       <- c(0,0,0,1,1,1)
aroma_complex   <- c(1,0,1,0,1,0)
flavour_fruit   <- c(1,1,1,1,1,1)
flavour_oak     <- c(0,0,0,0,1,1)
flavour_complex <- c(0,1,0,1,1,1)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Jake"
aroma_fruit     <- c(0,1,1,0,0,0)
aroma_oak       <- c(1,0,0,1,0,1)
aroma_complex   <- c(0,1,0,1,0,0)
flavour_fruit   <- c(0,0,0,0,1,0)
flavour_oak     <- c(0,0,0,1,0,1)
flavour_complex <- c(0,0,0,1,0,1)
likability <- 7
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

# ################################# 
wine <- wine1
person <- "Mike"
aroma_fruit     <- c(1,1,1,0,1,0)
aroma_oak       <- c(0,1,0,1,1,1)
aroma_complex   <- c(0,1,0,0,0,0)
flavour_fruit   <- c(1,0,1,0,0,0)
flavour_oak     <- c(0,0,0,0,1,1)
flavour_complex <- c(1,1,1,0,1,0)
likability <- 1
dtos[[paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Jake"
aroma_fruit     <- c(1,1,1,0,1,0)
aroma_oak       <- c(0,1,0,1,0,1)
aroma_complex   <- c(0,1,0,1,0,0)
flavour_fruit   <- c(1,0,1,1,1,0)
flavour_oak     <- c(0,0,1,0,0,1)
flavour_complex <- c(1,1,1,0,1,0)
likability <- 4
dtos[[paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "John"
aroma_fruit     <- c(1,1,1,0,0,1)
aroma_oak       <- c(0,1,0,1,0,1)
aroma_complex   <- c(0,1,0,0,0,0)
flavour_fruit   <- c(1,0,1,0,1,0)
flavour_oak     <- c(0,0,0,0,1,0)
flavour_complex <- c(1,0,0,0,1,0)
likability <- 2
dtos[[paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine6
person <- "Trevor"
aroma_fruit     <- c(1,1,1,0,1,0)
aroma_oak       <- c(0,1,0,1,1,1)
aroma_complex   <- c(0,1,0,1,0,0)
flavour_fruit   <- c(1,0,1,0,0,0)
flavour_oak     <- c(0,1,0,0,1,1)
flavour_complex <- c(1,0,1,0,0,0)
likability <- 6
dtos[[paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)




## @knitr produce_final_data
data <- plyr::ldply(dtos, data.frame)
rm( list = setdiff(ls(),c("data","labels")))
saveRDS(data,"data.rds")




