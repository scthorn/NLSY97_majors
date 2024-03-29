#Tidyverse is a collection of R packages that provides a cohesive framework for cleaning, manipulatin, analyzing, and visualizing data. 
#For two excellent and free guides see:
  #https://r4ds.hadley.nz/. 
  #https://jhudatascience.org/tidyversecourse/

#This script walks you through the tidyverse by cleaning, reformatting, and exploring data from NLSY97, a large scale longitudinal survey conducted by the U.S. Bureau of Labor Statistics. 

#--------------------------------------
# load tidyverse, a collection of R packages that provides a cohesive framework for cleaning, manipulating, analyzing, and visualizing data.

#install.packages("tidyverse") #uncomment this line if you haven't installed tidyverse yet

library(tidyverse)

#--------------IMPORTING DATA WITH READR------------------------

# import data using readr, a package that provides a fast and friendly way to read rectangular data (like csv files) into R. Save it as a dataframe called df.

#The file path I used here is a relative path. This means that the file is in a folder called 'data' that is in the same directory as this script.
# If you're using RStudio, uncomment the line below to set the working directory to the directory of the current script.
#Otherwise, you'll need to change the code to specify the full path to the file.

#try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

df <- read_csv("data/tidytutorial_RAW.csv") #read in the data

# all sorts of ways to take a look at the data

df #prints the entire dataframe to the console

glimpse(df) #prints a summary of the dataframe to the console

head(df) #prints the first few rows of the dataframe to the console

summary(df) #prints summary statistics for each variable to the console


#--------------MANIPULATING DATA WITH DPLYR------------------------

# dplyr is a package that provides functions for manipulating data. 

# ------------- common dplyr verbs -------------------------------      

# work with columns
rename(df, yob = R0536402) #rename the variable R0536402 to yob
select(df, R0000100, R0536300, R0536402) #select only these three variables
relocate(df, R0536402) #move the variable R0536402 to the first column
mutate(df, ageish2021 = 2021 - R0536402) #create a new variable that calculates each R's approximate age in 2021

# work with rows
filter(df, R0536402 == 1980) #only include cases where the year of birth is 1980
slice(df, 1:3) #only include the first three cases
sample_n(df, 3) #randomly select three cases

# work with groups
group_by(df, R0536402) #Usually you would do this to apply a function to each group (see below for example)

# ------------- the pipe (%>%) -------------------------------      

# you can use the 'pipe' to combine multiple actions. It takes the output of one function and uses it as the input to the next function.

df %>%
  rename(yob = R0536402) %>% #rename the variable R0536402 to yob
  select(R0000100, R0536300, yob) %>% #select only these three variables
  relocate(yob) %>% #move the variable yob to the first column
  mutate(ageish2021 = 2021 - yob) %>% #create a new variable that calculates each R's approximate age in 2021
  filter(ageish2021 == 41) #only include cases where the age in 2021 is 41

df %>%
  group_by(R0536402) %>% #group by year of birth (R0536402)
  count() #this will count the number of Rs for each year of birth

# The syntax as written above only prints to the console. If you want to save the output you need to assign it to a new dataframe.

df_practice <- df %>% 
  rename(yob = R0536402) %>%
  select(R0000100, R0536300, yob) %>%
  relocate(yob) %>%
  mutate(ageish2021 = 2021 - yob) %>%
  filter(ageish2021 == 41)

glimpse(df_practice)

# ------------- working with the NLSY data -------------------------------      

# Let's use these dplyr functions to make this data a little easier to work with. If you're new to R, don't worry about understanding every line of code here--it's building on the basic dplyr verbs a bit.

#rename variables
df <- df |>
  rename(ID = R0000100, 
       sex = R0536300,                  
       yob = R0536402,                         
       biodad_edu = R1302400,                                
       biomom_edu = R1302500,                             
       resdad_edu = R1302600,                             
       resmom_edu = R1302700,                        
       race = R1482600,       
       inschool2015 = U0008700,                                              
       hhincome2015 = U0008900,
       marstat2015 = U0014500,                  
       urbanrural2015 = U0015000,
       inschool2017 = U1845300,                             
       hhincome2017 = U1845500,                                              
       marstat2017 = U1852300,                                   
       urbanrural2017 = U1853200,
       inschool2019 = U3443800,        
       hhincome2019 = U3444000,         
       marstat2019 = U3451400,          
       urbanrural2019 = U3453600,         
       inschool2021 = U4949500,           
       hhincome2021 = U4949700,          
       marstat2021 = U4954500,               
       urbanrural2021 = U4956900,     
       educ = Z9083900,
       ba_date = Z9084400,          
       prof_date = Z9084500,         
       phd_date = Z9084600,         
       ma_date = Z9084700,            
       last_interview_round = Z9085100,           
       hhnetworth_30 = Z9121900,    
       hhnetworth_35 = Z9141400,    
       hhnetworth_40 = Z9164500)

#replace missing values (represented by -1 to -5) with NA
df <- df |>
  mutate(across(everything(), ~ifelse(. %in% c(-1, -2, -3, -4, -5), NA, .))) 


#assign appropriate datatypes and recode from numbers to values
df <- df |>
  relocate(yob:resmom_edu, starts_with("hhincome"), .after = hhnetworth_40) |> #we're moving variables just to lump datatypes together to make the next step simpler
  mutate(across(ba_date:hhincome2021, ~ as.integer(.))) |> #convert these variables to integer
  mutate(across(sex:educ, ~ as.factor(.))) |> #convert these variables to factors
  mutate(sex = fct_recode(sex, 
                          "male" = "1",
                          "female" = "2")) |> #recode from numbers to the values they represent
  mutate(race = fct_recode(race,
                           "black" = "1",
                           "hispanic" = "2",
                           "multi" = "3",
                           "nonblacknonhispanic" = "4")) |>
  mutate(educ = fct_recode(educ,
                           "None" = "0",
                           "GED" = "1",
                           "HS" = "2",
                           "AA" = "3",
                           "BA" = "4",
                           "MA" = "5",
                           "PhD" = "6",
                           "ProfD" = "7")) |>
  mutate(across(biodad_edu:resmom_edu, ~ case_when( #recode from years of education to a smaller number of categories
    . < 12 ~ "less than HS",
    . == 12 ~ "HS",
    . > 12 & . < 16 ~ "Some college", 
    . >= 16 ~ "BA or more", 
    TRUE ~ NA_character_ 
  )))


# ------------- exercises 1 -------------------------------      

#1. Rename the hhincome2015 variable to hhincome15 (or anything else you want).
#2. Recode the urbanrural2021 variable to "rural" for 0, "urban" for 1, and "unknown" for 2.

#trickier
#3. Set all values of "unknown" to NA for the area_type variable.
#4. Recode all the urbanrural variables to "rural" for 0, "urban" for 1, and "unknown" for 2, within the same mutate function.

#--------------RESHAPING DATA WITH TIDYR (and a little string manipulation with stringr) ----------------------------

#Tidyr is a package that provides tools for tidying data. Tidying data means structuring datasets to facilitate analysis. Principles of tidy data are as follows: 
#1. Each variable forms a column.
#2. Each observation forms a row.
#3. Each cell contains a single value.

#For demonstration purposes we'll use a subset of the data.

demo <- df |>
  select(ID, hhincome2017, hhincome2019, hhincome2021) 

demo #take a look. this data is in wide format

#pivot_longer is a function that takes data in wide format and makes it longer.
longer <- demo |>
  pivot_longer(cols = starts_with("hhincome"), #specify the columns you want to make longer
               names_to = "year", #name the new variable that will hold the column names
               values_to = "hhincome") #name the new variable that will hold the values

longer #take a look. this data is in long format

#sidenote: you can use stringr to extract the year from the variable name 
longer <- longer |>
  mutate(year = str_sub(year, -4)) #str_sub is a function that extracts a substring from a string. Here I'm saying to take the last four characters.
#pivot_wider is a function that takes data in long format and spreads it out.

back_to_wide <- longer |>
  pivot_wider(names_from = year,  #specify the variable whose values will become column names
              values_from = hhincome) #specify the variable whose values will become the cell values

back_to_wide #take a look. this data is back in wide format

back_to_wide <- back_to_wide |>
  rename_with(~str_c("hhincome", .x), '2017':'2021') #str_c is a function that concatenates strings. Here I'm adding "hhincome" to the beginning of each variable name

# ------------- exercises 2 -------------------------------      

#1. Create a new dataframe that includes only the variables ID, yob, urbanrural2021, race, HHnetworth_30, HHnetworth_35, and HHnetworth_40. Save it as df_networth.
#2. Reshape df_networth so that the net worth variables are in long format.

#trickier

#3. Rename the household net worth variables in df_networth to "at30", "at35", and "at40."
#4. Create a new variable for the year each R was 30.



#--------------DESCRIPTIVE STATS WITH DPLYR------------------------

#I've created a simplified dataframe to demonstrate some basic descriptive statistics and exploratory visualization.

df_simple <- read.csv("data/df_simple.csv") #read in the data, remember to change the file path if you need to

# ------------- univariate frequencies and summary stats -------------------------------      

#categorical variable
df_simple |>
  group_by(parent_edu) |>  #group by a variable
  count() #count the number of cases in each group

#continuous variable
df_simple |> 
  summarize(mean_income = mean(hhincome2021, na.rm = TRUE), #the na.rm argument tells R to ignore missing values
            median_income = median(hhincome2021, na.rm = TRUE), 
            sd_income = sd(hhincome2021, na.rm = TRUE),
            min_income = min(hhincome2021, na.rm = TRUE),
            max_income = max(hhincome2021, na.rm = TRUE))

# ------------- bivariate frequencies (crosstabs) and summary stats -------------------------------      

df_simple |>
  group_by(parent_edu, educ) |> #group by two variables
  count() |> #count the number of cases for each combination of the two variables
  pivot_wider(names_from = educ, values_from = n, values_fill = 0) #spread the data out to display as crosstabs

df_simple |>
  group_by(parent_edu) |> #group by parents education, then calculate summary stats per group
  summarize(mean_income = mean(hhincome2021, na.rm = TRUE),
            median_income = median(hhincome2021, na.rm = TRUE),
            sd_income = sd(hhincome2021, na.rm = TRUE),
            min_income = min(hhincome2021, na.rm = TRUE),
            max_income = max(hhincome2021, na.rm = TRUE))

#--------------EXPLORATORY VISUALIZATION WITH GGPLOT (if we have time)-----------------------

#ggplot2 is a package that implements the grammar of graphics. It's a powerful tool for creating visualizations.

#You can think of ggplot as a way to build up a plot piece by piece. You start by specifying the data you want to plot and the variables you want to map to aesthetics, then you add layers to the plot.

# ------------- univariate   -------------------------------      

#categorical variable
ggplot(df_simple, aes(x = parent_edu)) + #specify the dataset and the variable you want to plot
  geom_bar()  #add a layer for the plot type

#continuous variable
ggplot(df_simple, aes(x = hhincome2021)) + 
  geom_histogram()

ggplot(df_simple, aes(y = hhincome2021)) + 
  geom_boxplot() 

#You can add more layers to the plot to customize it further. 
ggplot(df_simple, aes(y = hhincome2021)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::dollar_format()) + #format the y-axis as dollars
  labs(title = "Household income distribution") #add a title


# ------------- bivariate -------------------------------      

ggplot(df_simple, aes(x = parent_edu, y = educ)) + 
  geom_jitter() #jitter is a way to spread out points that would otherwise overlap

ggplot(df_simple, aes(x = parent_edu, y = hhincome2021)) + 
  geom_boxplot() + 
  scale_y_continuous(labels = scales::dollar_format())

ggplot(df_simple, aes(x = hhincome2021)) + 
  geom_histogram() + 
  facet_wrap(~parent_edu) #facet_wrap creates separate plots for each level of a categorical variable

# ------------- exercises 3 -------------------------------      

#Complete these questions using df_networth:

#1. What is the distribution of this sample by race? Run summary stats. If you have time, visualize this relationship with ggplot.
#2. How is the urbanrural variable distributed across racial categories in this sample? Run summary stats. If you have time, visualize this relationship with ggplot.
#3. How does household networth vary by race in this sample? Run summary stats. If you have time, visualize this relationship with ggplot.

#trickier
#4. Merge df_networth with df_simple. You will need to use join, a dplyr verb we didn't try. Ask google/chatgpt about it, or type '?join" into your console to read the docs.
#5. Create a scatterplot with household income on the x-axis and household networth on the y-axis. 
