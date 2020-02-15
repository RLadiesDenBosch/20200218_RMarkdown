### R script for EAIR workshop 2019-08-26


# 1 - Add some code and run some code ----------------------------------------

p <- 50
s <- "The number of participants is"
paste(s, p)


# 2 - Use the tidyverse, read the data & Knit --------------------------------

# in the chunk setup

library(tidyverse)

# the chunk readData incl contents


```{r readData, cache = TRUE}

# read file IncomingInternationalDegreeStudents.csv

location_of_data <- file.path("data",
                              "IncomingInternationalDegreeStudents.csv")
dStudents <- read_csv2(location_of_data)

```

# 3 - Adjustment in setup & Knit again ------------------------------------

# change the knitr::opts
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)


# 4 - Some content  ------------------------------------------------------

# add this text below the chunk "readData", including "# Introduction"

# Introduction  

The data

```{r}
dStudents
```


# 5 - A less ugly way of displaying a table ----------------------

# installing the package knitr, do this in the Console, not in the Rmd file
install.packages("knitr")

# loading the package knitr, by placing this line in the chunk "setup"
library(knitr)

# changing the code in the most recent chunk ( the one with "dStudents" in it:

dStudents %>%
  kable()

# and now this code with a caption

dStudents %>%
  kable(caption = "The data")



# 6 - Tidy dStudents  ----------------------------------------------------

# The new content of the chunck readData

# read file IncomingInternationalDegreeStudents.csv

location_of_data <- file.path("data",
                              "IncomingInternationalDegreeStudents.csv")
dStudents <- read_csv2(location_of_data) %>%
  # the data is not in tidy format,
  # reshape wide to long using tidyr::gather
  # the first column (Program) is ok, no gathering necessary
  gather(key = "Year", value = "n", - Program)




# 7 - A solution  -----------------------------------------------------------


```{r}
dStudents %>%
  filter(Year %in% c(2006, 2017)) %>%
  group_by(Year) %>%
  mutate(Total_per_year = sum(n),
         part = n / Total_per_year,
         part = paste0(round(100*part,0), "%")) %>%
  select(Program, Year, part) %>%
  spread(Year, part) %>%
  # caption in kable means title
  kable(caption = "Choice of Program, per year",
        align = "lrr")
```

# 8 - Some base R and some variables ---------------------------------------

# the new chunk calculations, place directly after readingData chunk

```{r calculations}

# The first year in the data
min_year <- min(dStudents$Year)

# the most recent year in the data
max_year <- max(dStudents$Year)
```

# The adjusted table chunk


```{r}
dStudents %>%
  filter(Year %in% c(min_year, max_year)) %>%
  group_by(Year) %>%
  mutate(Total_per_year = sum(n),
         part = n / Total_per_year,
         part = paste0(round(100*part,0), "%")) %>%
  select(Program, Year, part) %>%
  spread(Year, part) %>%
  # caption in kable means title
  kable(caption = "Choice of Program, per year",
        align = "lrr")
```

# 9 - Solution to this problem  ----------------------------------------

# the adjusted readData chunk

# read file IncomingInternationalDegreeStudents.csv

location_of_data <- file.path("data",
                              "IncomingInternationalDegreeStudents.csv")
dStudents <- read_csv2(location_of_data) %>%
  # the data is not in tidy format,
  # reshape wide to long using tidyr::gather
  # the first column (Program) is ok, no gathering necessary
  gather(key = "Year", value = "n", - Program, convert = TRUE)


#10 -  ----------------------------------------------------------------------------

# the new content of the chunk calculations  


# The first year in the data
min_year <- min(dStudents$Year)

# the most recent year in the data
max_year <- max(dStudents$Year)

diff_years <- max_year - min_year

# totals per year, only for the min and the max year
dTotalPerYear <- dStudents %>%
  group_by(Year) %>%
  summarise(Total = sum(n)) %>%
  filter(Year %in% c(min_year, max_year))

# base R for calculating the total in max_year
total_max_year <- dTotalPerYear$Total[dTotalPerYear$Year == max_year] %>%
  # the counts were numeric, we want an integer
  as.integer()

# another way for the min_year
total_min_year <- dTotalPerYear %>%
  filter(Year == min_year) %>%
  select(Total) %>%
  # now still a dataset, we want only the value
  as.integer()

# the growth
growth_between <- (total_max_year - total_min_year) / total_min_year
# and now formatted as percentage
growth_between <- sprintf("%.0f%%", 100* growth_between)


# 11 - A ggplot for the document -----------------------------------------------


```{r plotperprogram}

dStudents %>%
  ggplot(aes(x = Year,
             y = n,
             color = Program)) +
  geom_line()

```

# 12 - Some improvements on the plot ------------------------------------------


```{r plotperprogram}

dStudents %>%
  ggplot(aes(x = Year, y = n, color = Program)) +
  geom_line(size = 1) +
  # better values on the x axis:
  scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 2)) +
  # add titles and better labels
  labs(y = "students",
       title = "Incoming international students",
       # caption in ggplot is used for source & credits information
       caption = "(Data: Nuffic | Adaptation; Author)") +
  # another theme
  theme_minimal() +
  # caption on the left, label x axis on the right
  # and set some font sizes
  theme(plot.caption = element_text(hjust = 0, size = 8),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 9),
        axis.title.x = element_text(hjust = 1),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8))


```

# 13 - The last tweaks ---------------------------------------------------

# adjust the chunk header of the chunk plotperprogram

```{r plotperprogram, fig.height = 2, fig.width = 6}

# 14 - In case there is any time left  ---------------------------------

```{r, fig.width= 8}
dStudents %>%
  group_by(Year) %>%
  mutate(Total_per_year = sum(n),
         part = n / Total_per_year,
         part_label = paste0(round(100*part,0), "%")) %>%
  ggplot(aes(x = as.factor(Year), y = part, fill = Program, label = part_label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5),
            size = 4,
            color = "white",
            fontface = "bold") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust = 0, size = 8),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "Relative more students in NL for Research University",
       subtitle = "(2006 - 2017)",
       x = "Year",
       caption = "(Data: Nuffic | Adaptation; Author)")

```









