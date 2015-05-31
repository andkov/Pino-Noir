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
wine1 <- list("Name"= "Quails' Gate",
              "Region"="Okanagan",
              "Year"="2013"); 
wine2 <- list("Name"= "Cono Sur",
              "Region"="Chile",
              "Year"="2012"); 
wine3 <- list("Name"= "Sokol Blosser",
              "Region"="Oregon",
              "Year"="2011"); 
wine4 <- list("Name"= "Curly Flat",
              "Region"="Australia",
              "Year"="2012"); 
wine5 <- list("Name"= "Whitehaven",
              "Region"="New Zealand",
              "Year"="2012"); 
# wine6 <- list("Name"= "Kris II",
#               "Region"="New Zealand II",
#               "Year"="2003"); 

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
wine <- wine2
person <- "Cassandra"
aroma_fruit     <- c(0,1,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,1)
flavour_oak     <- c(1,1,0,1,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Cassandra"
aroma_fruit     <- c(0,1,0,0,0,1)
aroma_oak       <- c(0,1,0,0,1,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,1)
flavour_oak     <- c(1,1,0,1,0,0)
flavour_complex <- c(1,0,0,0,1,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "DC"
aroma_fruit     <- c(1,0,1,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(1,0,1,0,0,0)
flavour_fruit   <- c(1,0,1,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(1,0,1,0,1,0)
likability <- 7
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Sasha"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,1,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Cassandra"
aroma_fruit     <- c(0,0,0,1,0,0)
aroma_oak       <- c(0,0,0,1,0,0)
aroma_complex   <- c(1,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Amanda"
aroma_fruit     <- c(0,1,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Melanie"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,0,1,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(0,0,0,1,0,0)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine2
person <- "Amanda"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,0,3,0,0,0)
aroma_complex   <- c(0,0,0,1,0,1)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Melanie"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(1,0,0,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Ken"
aroma_fruit     <- c(0,1,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(1,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)


################################# 
wine <- wine2
person <- "Ken"
aroma_fruit     <- c(1,0,0,0,0,0)
aroma_oak       <- c(0,0,1,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(0,0,1,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Ken"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,1,0,0)
flavour_oak     <- c(0,1,0,0,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Ken"
aroma_fruit     <- c(0,0,0,0,0,1)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,0,1,0)
flavour_complex <- c(0,0,0,0,1,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Ken"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "DC"
aroma_fruit     <- c(1,0,0,0,0,0)
aroma_oak       <- c(1,0,1,0,0,0)
aroma_complex   <- c(0,1,0,0,0,1)
flavour_fruit   <- c(1,0,0,1,0,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(1,0,1,1,0,0)
likability <- 4 #NA
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine2
person <- "DC"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,1,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 1
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "DC"
aroma_fruit     <- c(0,0,0,0,0,1)
aroma_oak       <- c(0,0,1,0,0,0)
aroma_complex   <- c(1,0,0,0,0,1)
flavour_fruit   <- c(0,0,0,0,0,1)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(1,0,1,0,0,1)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "DC"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,1,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(1,1,0,0,1,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Melanie"
aroma_fruit     <- c(0,0,0,1,0,0)
aroma_oak       <- c(0,0,0,1,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Amanda"
aroma_fruit     <- c(0,0,0,1,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,1,0)
flavour_oak     <- c(0,1,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4 #NA
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Ola"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(0,0,0,1,0,0)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Ola"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(1,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(1,0,1,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Ola"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,0,1,0,0)
aroma_complex   <- c(0,0,1,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,1)
flavour_oak     <- c(0,0,1,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Ola"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,1,1,0,0)
aroma_complex   <- c(1,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(0,1,0,0,0,0)
flavour_complex <- c(1,0,0,0,1,0)
likability <- 7
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Sasha"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(1,0,0,0,0,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(0,0,0,1,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Sasha"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,1,0,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Sasha"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine2
person <- "Sasha"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,0,0,0,1)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,1)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Andrey"
aroma_fruit     <- c(1,0,1,0,0,1)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,0,1,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(1,1,0,0,0,0)
likability <- 3
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Andrey"
aroma_fruit     <- c(0,0,0,0,1,0)
aroma_oak       <- c(0,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(1,0,1,0,0,0)
flavour_oak     <- c(0,0,0,0,0,0)
flavour_complex <- c(0,1,0,0,1,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Peter"
aroma_fruit     <- c(1,0,0,0,0,0)
aroma_oak       <- c(0,0,1,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,1,0,0,0)
flavour_oak     <- c(0,0,1,1,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine2
person <- "Peter"
aroma_fruit     <- c(0,0,1,0,0,0)
aroma_oak       <- c(0,0,1,1,0,0)
aroma_complex   <- c(1,0,0,0,0,1)
flavour_fruit   <- c(0,1,1,0,0,1)
flavour_oak     <- c(0,0,0,1,0,0)
flavour_complex <- c(1,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Peter"
aroma_fruit     <- c(0,1,1,0,0,0)
aroma_oak       <- c(0,1,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,1,0,0,0)
flavour_oak     <- c(0,1,0,1,0,0)
flavour_complex <- c(0,0,0,0,1,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Peter"
aroma_fruit     <- c(0,1,0,0,0,0)
aroma_oak       <- c(1,0,1,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,1,0,0,0)
flavour_oak     <- c(1,0,1,1,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine1
person <- "Bob"
aroma_fruit     <- c(0,1,0,0,0,0)
aroma_oak       <- c(1,0,0,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,0)
flavour_oak     <- c(1,0,0,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine3
person <- "Raquel"
aroma_fruit     <- c(1,0,1,0,0,0)
aroma_oak       <- c(1,0,1,0,0,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(1,1,1,0,0,0)
flavour_oak     <- c(1,0,1,1,0,0)
flavour_complex <- c(0,0,0,1,0,0)
likability <- 4
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine4
person <- "Raquel"
aroma_fruit     <- c(0,0,0,0,0,0)
aroma_oak       <- c(0,1,0,0,0,0)
aroma_complex   <- c(0,0,0,0,1,0)
flavour_fruit   <- c(0,1,1,0,1,0)
flavour_oak     <- c(1,0,1,0,0,0)
flavour_complex <- c(0,0,0,0,0,0)
likability <- 5
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)

################################# 
wine <- wine5
person <- "Raquel"
aroma_fruit     <- c(0,1,0,0,0,1)
aroma_oak       <- c(0,1,0,0,1,0)
aroma_complex   <- c(0,0,0,0,0,0)
flavour_fruit   <- c(0,1,0,0,0,1)
flavour_oak     <- c(1,1,0,0,1,0)
flavour_complex <- c(1,0,0,0,1,0)
likability <- 6
dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)



# ################################# 
# wine <- wine
# person <- "Name"
# aroma_fruit     <- c(0,0,0,0,0,0)
# aroma_oak       <- c(0,0,0,0,0,0)
# aroma_complex   <- c(0,0,0,0,0,0)
# flavour_fruit   <- c(0,0,0,0,0,0)
# flavour_oak     <- c(0,0,0,0,0,0)
# flavour_complex <- c(0,0,0,0,0,0)
# likability <- 0
# dtos[[ paste0(wine[["Name"]],"_",person)]]<- compile.data(wine,person)
# 
# 



## @knitr produce_final_data
data <- plyr::ldply(dtos, data.frame)
rm( list = setdiff(ls(),c("data","labels")))
saveRDS(data,"data.rds")




