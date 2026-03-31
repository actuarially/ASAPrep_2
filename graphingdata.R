#ASA Datafest Part 2 Prep 2026
#Created March 30, 2026 
#Basis graph and which graph to use
#Anh, Jonathan, and James UF Statistics Club

####Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

####Read in data files
data <- read.csv("Guetschow-et-al-2022-PRIMAP-hist_v2.4_11-Oct-2022.csv")

####Basis on Graphing
#There are several graphs you should know how to utilize and apply in different situations
#From Statistics classes, you will be familiar with the following common graphs:
#Line Plot, Scatter plot (usually with line of best fit), Histogram, Box Plot, and Pie Chart
#Any extra graphs will often be a derivative of these graphs, and the documentation can be looked up as usual

####Line Graph
#This graph is mainly used to see how the data has changed over time
#We are going to use the code from previous session to clean the data up a little bit
colnames(data) <- c("source", "scenario (PRIMAP-hist)", "area (ISO3)", "entity", 
                    "unit", "category (IPCC2006_PRIMAP)", 1750:2021)
#To not make it not too overwhelming, we will be using the 21st century data again for the CO2 concentration
data21century <- subset(data, select = c("source", "scenario (PRIMAP-hist)", "area (ISO3)", "entity", 
                                         "unit", "category (IPCC2006_PRIMAP)", 2000:2021)) %>% filter(entity == "CO2") %>% filter(!is.na(`2000`) | !is.na(`2001`))
#The caveat for these line graphs is that you'll need a single data for each time period
#Since there are multiple data within each year for each area, we are going to just graph out the first category
CO2TrendCat1 <- data21century %>% filter(`category (IPCC2006_PRIMAP)` == "1")
#Reviewing the table, you'll see how there are two instance of the category
#Since we'll only need one, we'll take the average
CO2TrendCat1 <- CO2TrendCat1 %>% group_by(`area (ISO3)`) %>% summarise(across(any_of(as.character(2000:2021)), \(x) mean(x, na.rm = TRUE))) %>% as.data.frame()
#Now, if you look at our column names, we can use the year as our x, however, this will become very messy
#This is because your x values have large string, and it can overlap onto each other.
#So, we will rename our x values to be the number of year since 2000, and we'll make sure to provide an x-axis header
#We are also going to rename our first column
colnames(CO2TrendCat1) <- c("Area", 0:21)
#Now, let's say that you want compare between ABW and AGO. We'll need to filter out those 2 areas.
#But since our year is also in a "wide" format and ggplot only do "long", we'll need to reshape or transpose those columns also.
plotdata <- CO2TrendCat1 %>%
  filter(Area == "ABW" | Area == "AGO") %>%
  #To summarize, we keep the column of "Area", a new column that take on the original column names, and another column that will hold the previous values
  pivot_longer(cols = -Area, names_to = "Year", values_to = "CO2_Emission") %>%
  mutate(Year = as.numeric(Year))
#We can now finally plot
ggplot(plotdata, aes(x = Year, y = CO2_Emission, color = Area)) +
  geom_line(linewidth = 1) +                  # Create the lines
  geom_point(size = 2) +                      # Add dots at each data point
  
  # Specify colors: ABW as blue, AGO as orange
  scale_color_manual(values = c("ABW" = "blue", "AGO" = "orange")) +
  
  # Force the X-axis to show every year from 0 to 21
  scale_x_continuous(breaks = 0:21) +
  
  # Add titles and labels as requested
  labs(
    title = "CO2 emission between ABW and AGO",
    x = "Number of years since 2000",
    y = "CO2 emission in gigagram per year",
    color = "Area"
  ) +
  
  # Clean up the visual style
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
#And as usual, if we just want ABW, then we just have to filter out only to ABW and input the same thing
plotdataABW <- plotdata %>% filter(Area == "ABW")
ggplot(plotdataABW, aes(x = Year, y = CO2_Emission, color = Area)) +
  geom_line(linewidth = 1) +                  # Create the lines
  geom_point(size = 2) +                      # Add dots at each data point
  
  # Specify colors: ABW as blue, AGO as orange
  scale_color_manual(values = c("ABW" = "blue")) +
  
  # Force the X-axis to show every year from 0 to 21
  scale_x_continuous(breaks = 0:21) +
  
  # Add titles and labels as requested
  labs(
    title = "CO2 emission in ABW since 2000",
    x = "Number of years since 2000",
    y = "CO2 emission in gigagram per year",
    color = "Area"
  ) +
  
  # Clean up the visual style
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

####Scatter Plot
#Now mainly the point of a scatter plot is to see if there's a linear trend or if there's a correlation in a correlation scatter plot
#So let's take Canada (CAN) and US (USA) as an example.
#To analyze this, we'll clean up the data and shove them into 1 table like we did before
CO2TrendCat1A <- data21century %>% filter(`category (IPCC2006_PRIMAP)` == "1.A") %>% filter(`area (ISO3)` == "CAN" | `area (ISO3)` == "USA")
#This time since we are using scatter plot, we won't need to average out the data. We will just take the necessary columns instead
CO2TrendCat1A <- CO2TrendCat1A %>% select(`area (ISO3)`, `2000`:`2021`)
colnames(CO2TrendCat1A) <- c("Area", 0:21)
#Once again, we are going to have to convert this data to "long" so,
plotdatascatter <- CO2TrendCat1A %>% pivot_longer(cols = -Area, names_to = "Year", values_to = "CO2_Emission") %>%
  mutate(Year = as.numeric(Year))
#Now, notice how the data between the two countries have a huge gap. Because of that, we are going to normalize the data.
plotdatastandardized <- plotdatascatter %>% group_by(Area) %>% mutate(Z_score = (CO2_Emission - mean(CO2_Emission))/ sd(CO2_Emission))
ggplot(plotdatastandardized, aes(x = Year, y = Z_score, color = Area)) +
  # 1. Add the points
  geom_point(size = 3) +
  
  # 2. Add in Line of Best Fit
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  
  # 2. Ensure every year shows up on the X-axis
  scale_x_continuous(breaks = 0:21) +
  
  # 3. Set the colors you wanted
  scale_color_manual(values = c("CAN" = "blue", "USA" = "orange")) +
  
  # 4. Add your professional labels
  labs(
    title = "Scatterplot of CO2 Emissions: CAN vs USA",
    x = "Number of years since 2000",
    y = "CO2 emission (Standardized Z-score)",
    color = "Country"
  ) +
  
  # 5. Clean up the look
  theme_minimal()
#You can see the respective linear trend between the two countries. Now, what happen if we want to see if one affect the other?
#To do this, we are going to have to convert this data to wide since ggplot like wide data for correlation plot
#But since there are multiply instances of each year, we are going to assign special ID to maintain uniqueness
comparison_wide <- plotdatastandardized %>%
  group_by(Area, Year) %>% # This will treat each instance by their area and year
  mutate(obs_id = row_number()) %>% # Labels the first CAN and USA Year 0 as '1', second as '2'
  ungroup() %>%
  pivot_wider(names_from = Area, values_from = Z_score, id_cols = c(Year, obs_id))
#Graphing out again with the following instructions:
ggplot(comparison_wide, aes(x = CAN, y = USA)) +
  geom_point(aes(color = factor(Year)), size = 2.5, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "solid") +
  scale_color_viridis_d(name = "Year Since 2020") + # Makes the years easy to distinguish
  labs(
    title = "Direct Correlation: CAN vs USA (1.A Category)",
    subtitle = paste("Pearson r =", round(cor(comparison_wide$CAN, comparison_wide$USA, use = "complete.obs"), 3)),
    x = "Canada (Standardized Z-Score)",
    y = "USA (Standardized Z-Score)"
  ) +
  theme_minimal()

####Histogram
#Is one of the most common plots to use
#Allows us to gain a bigger picture of the shape of a distribution
#Here is a histogram for CO2 Emissions since 2000
histogramdata = CO2TrendCat1 |>
  pivot_longer(cols = -Area, names_to = "Year", values_to = "CO2_Emission") |>
  group_by(Area) |>
  summarize(CO2_Emission_Since_2000 = sum(CO2_Emission))
ggplot(data = histogramdata, aes(x = CO2_Emission_Since_2000)) +
  geom_histogram()

####Box Plot
#Plots summaries of data
#Plots the center of a distribution (median), the values that mark off the middle half of data (first and third quartiles), and the values that mark of fthe vast majority of the data (ends of whiskers)
#Here is a boxplot for CO2 Emissions since 2000
#Notice, we have only changed one line in our R code, switching to geom_boxplot()
ggplot(data = histogramdata, aes(x = CO2_Emission_Since_2000)) +
  geom_boxplot()

####Pie Chart
#Is a very common way to represent the distribution of a single categorical variable
#Here is a pie chart for ABW CO2 Emissions since 2000 vs AGO, CAN, and USA
piechartdata = CO2TrendCat1 |>
  filter(Area == "ABW" | Area == "AGO" | Area == "CAN" | Area == "USA") |>
  pivot_longer(cols = -Area, names_to = "Year", values_to = "CO2_Emission") |>
  group_by(Area) |>
  summarize(CO2_Emission_Since_2000 = sum(CO2_Emission))
ggplot(data = piechartdata, aes(x = "", y = CO2_Emission_Since_2000, fill = Area)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + # Turns bar chart into a pie chart
  theme_void() + # Removes background, grid, and axis labels
  labs(fill = "Area") # Changes the legend title
#You may also construct a bar chart
ggplot(data = piechartdata, aes(x = Area, y = CO2_Emission_Since_2000)) +
  geom_bar(width = 1, stat = "identity") +
  theme_minimal() +
  labs(title = "CO2 Emissions since 2000 by Area", x = "Area", y = "CO2 Emissions since 2000")
