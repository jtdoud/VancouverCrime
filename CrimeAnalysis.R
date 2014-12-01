
library(dplyr)
library(ggplot2)

# Set directory
setwd("~/My Dropbox/Professional/VancouverCrimeData")

# Get data
# download.file(url = "ftp://webftp.vancouver.ca/opendata/csv/crime_2013.csv", 
#               destfile = "./Data/crime_2013.csv")

# Read in data
data <- read.csv("./Data/crime_2013.csv", stringsAsFactors = F)

# Explore variable names
names(data)

# Change names to lower case
names(data) <- tolower(names(data))

# Dimensions
dim(data)

# Review structure
str(data)

# Check out first 10 rows
head(data, 10)

# Review a random sample of rows
data[sample(x = nrow(data), size = 10), ]

# Make sure month variable is as expected
range(data$month)

# Check year
unique(data$year)

# View unique categories of crime type
unique(data$type)

# Convert type to factor
data$type <- factor(x = data$type)
data$type <- reorder(x = data$type, X = data$type, FUN = length)

data$monthRC <- factor(x = data$month, 
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul" ,"Aug" ,"Sep" , "Oct", "Nov", "Dec"),
                        ordered = T)

# Any missing?
sum(complete.cases(data))

# Tally by type
table(data$type)
table(data$type)[order(table(data$type), decreasing = T)] # Order decreasing

# Tally by month dplyr way
group_by(.data = data, monthRC) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

# Bar chart
my_theme <- theme_bw() +
    theme(panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.5),
          axis.title = element_text(colour = "grey40"),
          plot.title = element_text(vjust = 2.0))

ggplot(data = data, aes(x = monthRC)) + 
    my_theme +
    geom_bar(stat = "bin", fill = "lightblue", width = 0.75)

ggplot(data = data, aes(x = type)) + 
    my_theme +
    geom_bar(stat = "bin", fill = "lightblue") +
    coord_flip()
    
temp <- data[which(as.numeric(data$type) == 1 |
                   as.numeric(data$type) == 2 |
                   as.numeric(data$type) == 3 |
                   as.numeric(data$type) == 4), ]

# ggplot(data = temp, aes(x = monthAbb)) + geom_bar() +
#     facet_grid(facets = type ~ .)

ggplot(data = temp, aes(x = monthRC, color = type, group = type)) +
    my_theme +
    geom_line(stat = "bin", size = 1)

temp <- data[which(as.numeric(data$type) == 5 |
                       as.numeric(data$type) == 6 |
                       as.numeric(data$type) == 7), ]

ggplot(data = temp, aes(x = monthRC, color = type, group = type)) + 
    my_theme +
    geom_line(stat = "bin", size = 1)

# for (i in yr) {
#     print(paste("ftp://webftp.vancouver.ca/opendata/csv/crime_", i, sep = ""))
# }