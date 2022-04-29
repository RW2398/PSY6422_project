# install any packages if needed
# install.packages("readxl")
# install.packages("tidyverse")
# install.packages("here")
# install.packages("gganimate")
# install.packages("plotly")
# install.packages("gifski")
# open packages
library(tidyverse)             
library(readxl)
library(here)
library(gganimate)
library(plotly)
library(gifski)
# Set working directory 
#setwd("here")
basedir <- here()
setwd(basedir)
#setwd(here("assessment"))
#setwd(here("~/MSC/data_analysis_and_visualisation/assessment"))
my_data <- read_excel(here("raw_data", "how-many-books-brits-read.xlsx")) 
head(my_data, 7)            # show first 7 rows of data
summary (my_data)           # show a summary of all raw data
# Rename columns to simpler names
my_data <- rename(my_data,'Books_read'= 'Roughly how many, if any, books have you read over the last 3 months?') 
my_data <- rename(my_data, 'Sum-19'='2019-07-04') 
my_data <- rename(my_data, 'Aut-19'='2019-09-26', 'Win-19'='2019-12-26','Spr-20'='2020-03-26','Sum-20'='2020-06-18', 'Aut-20'='2020-09-24')
my_data <- rename(my_data, 'Win-20'='2020-12-24','Spr-21'='2021-03-25','Sum-21'='2021-06-17','Aut-21'='2021-09-23','Win-21'='2021-12-23')
my_data$Avg_per = rowMeans(my_data[c(2,3,4,5,6,7,8,9,10,12)])
# Delete the unnecessary bottom 2 rows. Row 10 has also been eliminated as assigning a numerical value to 'Don't Know' would cause the data to be misleading.
my_data <- my_data[-c(13, 12, 11, 10), ] 
# Categories with non-numeric values e.g 'Don't Know' have been assigned numeric values. Also, categories with a range e.g. '6-8' have been assigned their mean value. 
my_data[9, "Books_read"] <- "12"
my_data[8, "Books_read"] <- "10"
my_data[7, "Books_read"] <- "7"
my_data[1, "Books_read"] <- "0"
# Convert character data to numeric data
my_data$"Books_read" <- as.numeric(my_data$"Books_read")
# Multiply Avg_per by 100 to give percentages
#my_data$Avg_per <- my_data$Avg_per * 100
#Calculate average for months
my_data$AvgWin = rowMeans(my_data[c(4,8,12)]) 
my_data$AvgSpr = rowMeans(my_data[c(5,9)]) 
my_data$AvgSum = rowMeans(my_data[c(2,6,10)]) 
my_data$AvgAut = rowMeans(my_data[c(3,7,11)]) 
# Calculating genders
fd <- read_excel(here("raw_data", "how-many-books-brits-read.xlsx"),  sheet = 9)  
fd <- rename(fd,'Books_read'= 'Roughly how many, if any, books have you read over the last 3 months?') 
fd <- rename(fd, 'Sum-19'='2019-07-04') 
fd <- rename(fd, 'Aut-19'='2019-09-26', 'Win-19'='2019-12-26','Spr-20'='2020-03-26','Sum-20'='2020-06-18', 'Aut-20'='2020-09-24')
fd <- rename(fd, 'Win-20'='2020-12-24','Spr-21'='2021-03-25','Sum-21'='2021-06-17','Aut-21'='2021-09-23','Win-21'='2021-12-23')
fd$Avg_per = rowMeans(fd[c(2,3,4,5,6,7,8,9,10,12)]) 
# Delete the unnecessary bottom 2 rows. Row 10 has also been eliminated as assigning a numerical value to 'Don't Know' would cause the data to be misleading.
fd <- fd[-c(13, 12, 11, 10),]
# Categories with non-numeric values e.g 'Don't Know' have been assigned numeric values. Also, categories with a range e.g. '6-8' have been assigned their mean value. 
fd[9, "Books_read"] <- "12"
fd[8, "Books_read"] <- "10"
fd[7, "Books_read"] <- "7"
fd[1, "Books_read"] <- "0"
# Convert character data to numeric data
fd$"Books_read" <- as.numeric(fd$"Books_read")
# Multiply Avg_per by 100 to give percentages
fd$Avg_per <- fd$Avg_per * 100
# Male data
md <- read_excel(here("raw_data", "how-many-books-brits-read.xlsx"),  sheet = 8)
md <- rename(md,'Books_read'= 'Roughly how many, if any, books have you read over the last 3 months?') 
md <- rename(md, 'Sum-19'='2019-07-04') 
md <- rename(md, 'Aut-19'='2019-09-26', 'Win-19'='2019-12-26','Spr-20'='2020-03-26','Sum-20'='2020-06-18', 'Aut-20'='2020-09-24')
md <- rename(md, 'Win-20'='2020-12-24','Spr-21'='2021-03-25','Sum-21'='2021-06-17','Aut-21'='2021-09-23','Win-21'='2021-12-23')
md$Avg_per = rowMeans(md[c(2,3,4,5,6,7,8,9,10,12)]) 
# Delete the unnecessary bottom 2 rows. Row 10 has also been eliminated as assigning a numerical value to 'Don't Know' would cause the data to be misleading.
md <- md[-c(13,12,11,10), ] 
# Categories with non-numeric values e.g 'Don't Know' have been assigned numeric values. Also, categories with a range e.g. '6-8' have been assigned their mean value. 
md[9, "Books_read"] <- "12"
md[8, "Books_read"] <- "10"
md[7, "Books_read"] <- "7"
md[1, "Books_read"] <- "0"
# Convert character data to numeric data
md$"Books_read" <- as.numeric(md$"Books_read")
# Multiply Avg_per by 100 to give percentages
md$Avg_per <- md$Avg_per * 100

# Visualisations

# Sex line plot
sex <- ggplot() 
sex <- sex + geom_line(data = md, aes(x = `Books_read`, y = `Avg_per`), color = "royalblue2", size = 1, arrow = arrow()) + 
  geom_line(data = fd, aes(x = `Books_read`, y = `Avg_per`), color = "hotpink1", size = 1, arrow = arrow()) +
  geom_point(data = md, aes(x = `Books_read`, y = `Avg_per`), color = "navyblue", size = 2) +
  geom_point(data = fd, aes(x = `Books_read`, y = `Avg_per`), color = "deeppink1", size = 2) +
  labs(subtitle="Male = Blue, Female = Pink", 
       x="Books read over 3 months", 
       y=" Average percentage", 
       title="Sex differences in number of books read", 
       caption = "Source: YouGov") 
#extras
sex_int <- ggplotly(sex)
sex_int
ggsave(filename = file.path("figures","sex_int.gif")) # saving interactive plot
sex_anim <- sex + transition_reveal(`Books_read`)
animate(sex_anim)
anim_save(filename = file.path("figures","sex_anim.gif")) # saving animated plot

#Donut plots

# Winter

hsize <- 1 # adjusting size of donut hole
# creating visualisation, specifying variables, colours, fill and labels for each plot
winter <- ggplot(my_data, aes(x = hsize, y = AvgWin, fill = as.factor(`Books_read`))) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "GnBu") +     # seasonal colours
  geom_text(aes(label = scales::percent(`AvgWin`, accuracy=0.1)), # percentage numbers within plot
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0, direction = -1) +  # starting plot from top of donut
  xlim(c(0.2, hsize + 0.5)) +
  labs(subtitle="Percentage of books read over 3 months during winter", 
       x="Percentage", 
       title="Winter plot", 
       caption = "Source: YouGov") +
  theme(panel.background = element_rect(fill = "white"),   # removing background and lines
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(title = "Books read"))
plot(w)
ggsave(filename = file.path("figures","winter.png"))
 
#Spring
spring <- ggplot(my_data, aes(x = hsize, y = AvgSpr, fill = as.factor(`Books_read`))) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "Greens") +
  geom_text(aes(label = scales::percent(`AvgSpr`, accuracy=0.1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  xlim(c(0.2, hsize + 0.5)) +
  labs(subtitle="Percentage of books read over 3 months during spring", 
       x="Percentage", 
       title="Spring plot", 
       caption = "Source: YouGov") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(title = "Books read"))
ggsave(filename = file.path("figures","spring.png"))

#Summer
summer <- ggplot(my_data, aes(x = hsize, y = AvgSum, fill = as.factor(`Books_read`))) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "RdPu") +
  geom_text(aes(label = scales::percent(`AvgSum`, accuracy=0.1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  xlim(c(0.2, hsize + 0.5)) +
  labs(subtitle="Percentage of books read over 3 months during summer", 
       x="Percentage", 
       title="Summer plot", 
       caption = "Source: YouGov") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(title = "Books read"))
ggsave(filename = file.path("figures","summer.png"))

#Autumn
autumn <- ggplot(my_data, aes(x = hsize, y = AvgAut, fill = as.factor(`Books_read`))) +
  geom_col(color = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  geom_text(aes(label = scales::percent(`AvgAut`, accuracy=0.1)),
            position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y", start = 0, direction = -1) +
  xlim(c(0.2, hsize + 0.5)) +
  labs(subtitle="Percentage of books read over 3 months during autumn", 
       x="Percentage", 
       title="Autumn plot", 
       caption = "Source: YouGov") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(title = "Books read"))
ggsave(filename = file.path("figures","autumn.png"))

# attempt at creating a loop to make donut plots
titles = c('Winter plot', 'Spring plot', 'Summer plot', 'Autumn plot')
colours = c("GnBu", "Greens", "RdPu", "YlOrRd")
columns = c(my_data$AvgWin, my_data$AvgSpr, my_data$AvgSum, my_data$AvgAut)
l <-for(i in 1:4){
  ggplot(my_data, aes(x = hsize, y=get(my_data[ ,i]),fill = as.factor(`Books_read`))) +
         geom_text()+ # percentage numbers within plot
           geom_col(color = "black") +
           scale_fill_brewer(palette = i) +
           coord_polar(theta = "y", start = 0, direction = -1) +  # starting plot from top of donut
           xlim(c(0.2, hsize + 0.5)) +
           labs(subtitle="Percentage of books read over 3 months", 
                x="Percentage", 
                title="i", 
                caption = "Source: YouGov") +
           theme(panel.background = element_rect(fill = "white"),   # removing background and lines
                 panel.grid = element_blank(),
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank()) +
           guides(fill = guide_legend(title = "Books read"))
}
plot(l)
