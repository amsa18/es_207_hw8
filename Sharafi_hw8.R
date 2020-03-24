library(tidyverse)
x <- c("Apple", "Banana", "Pear")
str_sub(x,(str_length(x)+1)/2, (str_length(x)+1)/2) # Choose integr before middle 
str_sub(x,ceiling((str_length(x)+1)/2), ceiling((str_length(x)+1)/2))#choose integer after middle 

string_maker <- function(x){n <- length(x)
y <- c()
# Create a particular condition when the length of the vector is 0,1,2
if (n == 0) {
  NULL
}else if  (n == 1){
  str_c(" ", x, " ")
}else if  (n == 2){
  str_c(" ",x[[1]], ",and", x[[2]]," ",sep =" " )}
else {
  for(i in 1:(n-1)){y <- str_c(y," ",x[[i]],",")}
  y <- str_c(y," and"," ",x[[n]])
  y
}
}

str_view(c(".a.a.a","abc", "a.b"), "\\..\\..\\..")
str_view(c("why","apple","enter","booked","nine","item"),("^[aeiou]"))

str_view(c("why","apple","enter","booked","nine","item"), "^[^aeiou]+$")


str_view(c("why","apple","entering","booked","nine","item","seed"),"[^e]ed$")


str_view(c("why","apple","entering","booked","nine","item","seed","wise"),"[i](se|ng)$")

#define the contractions
contract <- "([A-Za-z]+)'([A-Za-z]+)"
ans <-  str_extract(sentences, contract) %>% na.omit() %>% str_split("'",simplify = TRUE)
ans

str_replace_all("c/desktop/es207/hw/hw2", "/", "\\\\")

x <- c("apples, pears, and bananas") 
x <- str_split(x, ",|and")
x

word <-  unlist(str_extract_all(sentences, boundary("word")))
word <- tibble(word) %>%
  mutate(word = str_to_upper(word)) %>%
  count(word, sort = TRUE)
word 

library(tidyverse, quietly = TRUE)
library(readr, quietly = TRUE)
o3.filepaths <- list.files("~/Documents/es_207_hw8", "\\.txt$", full.names = TRUE)
o3.filenames <- list.files("~/Documents/es_207_hw8", pattern = ".txt") # check your dirs
o3.filelist <- lapply(o3.filepaths, read_delim, delim = "|")
names(o3.filelist) <- gsub(".txt","", o3.filenames)
o3.filelist

class(o3.filelist)

# first define my function
o3summarize <- function(x){
  out <- group_by(x, site = as.factor(site), date) %>%
    summarize(o3 = mean(obs, na.rm = TRUE))
}
# now apply my function to the list of tibbles
daily <- o3.filelist %>% 
  map(o3summarize)
daily

daily3 <- daily[[3]]
daily3 <- as.tibble(daily3)
library(lubridate)
daily3$year <- year(daily3$date)
daily3$month <- month(daily3$date)
monthly.o3 <- daily3 %>% select(site, year, month,o3) 
monthly.o3

daily3$day <- day(daily3$date)
daily.o3 <- daily3 %>% select(site, year, month,day,o3) 
daily.o3

library(readxl)
loc <- read_excel("~/Documents/es_207_hw8/location.xls")
z <- loc$`Site Name`
z = str_to_upper(z)
z <- as.tibble(z)
NameSan<- str_count(z,"SAN|SANTA")
print(paste("Number of name of sites with -SAN- in it:",NameSan))

# new annual mean fucntion
o3year <- function(x){
  out <- group_by(x, site = as.factor(site), year) %>%
    summarize(o3 = mean(o3 , na.rm = TRUE))
}

daily.site$year <- year(daily.site$date)
yearly <-  group_by(daily.site, site = as.factor(site), year)%>% summarize(o3 = mean(o3 , na.rm = TRUE))
head(yearly)

loc_merced <- loc %>% filter(`County Abbr`== "MER")
q <- loc_merced$site
yearly_merced <- yearly %>% filter(site%in% q)
yearly_merced <-yearly_merced %>% filter(site=="3022")
ggplot(yearly_merced, aes(x=year, y=o3)) + 
  geom_point()

knitr::kable(
  yearly_merced[,2:3 ], caption = 'The annual daily mean of o3 for Merced County.'
)

#Specify it for Merced county
daily.site.merced <- daily.site%>% filter(site%in% "3022") # I chose one stataion in merced
#make a daily 
daily.merced <- daily.site.merced  %>% select(site,o3,date,year) %>% na.omit()
daily.merced$time <- seq(1,length(daily.merced$date))
# i make time series of day
Day <- daily.merced $time # for the sake of cleanliness
o3 <- log(daily.merced $o3)
loess25 <- loess(Day~o3, degree = 2, span = 0.25) # 50% smoothing span
#plot time series quadratic
g <- ggplot(daily.merced , aes(x = Day, y = o3,color=year))+ 
  geom_point(alpha = 0.2, na.rm = T)
g <- g +   
  geom_smooth(method = "loess", span = 0.25,method.args = list(degree=2), se = FALSE,color = "blue", na.rm = T)
g

#stl(loess25, s.degree = 0)
#error:  Error in complete.cases(object) : not all arguments have the same length