#--------------CONSTRUCT A DATASET FOR ANALYSIS------------------------

#Let's limit our population to Rs that participated in the most recent interview and create a simpler dataframe with only the variables we're interested in.

df_simple <- df |>
  filter(last_interview_round == "20") |> #filter to only include cases with data from the most recent round of interviews
  select(ID:race, educ:ma_date, yob:resmom_edu, hhincome2021) #select variables of interest


df_simple|>
  summarize(across(everything(), ~ sum(is.na(.)))) #this gives a count of NA values for each variable

#for today's purposes we'll just drop cases that are missing hhincome2021 or edu
df_simple <- df_simple |>
  filter(!is.na(hhincome2021) & !is.na(educ)) #filter to only include cases with non-missing values for hhincome2021 and educ

#I think the reason we're seeing so many missing values for degree dates is because the respondent hasn't completed that degree. But let's check that logic. 
df_simple |>
  group_by(educ) |> #group by educational attainment
  summarize(across(ba_date:ma_date, ~ sum(is.na(.)))) #count missing values for degree dates by educational attainment

#That's not what I was hoping to see. Let's just get rid of those variables. We don't need them.
df_simple <- df_simple |>
  select(-ba_date, -prof_date, -phd_date, -ma_date) #the '-' tells R to drop the variables

###consolidate parent education variables

#We don't need four separate parent education variables. Let's consolidate them into a single variable indicating the highest level of education achieved by any parent.

#first we have to make it ordinal.
ranking <- c("less than HS" = 1, "HS" = 2, "Some college" = 3, "BA or more" = 4) #create a variable to hold the ranking for the levels of education

df_simple <- df_simple |> 
  mutate(across(biodad_edu:resmom_edu, ~ factor(., levels = names(ranking), ordered = TRUE))) #convert to factor with the levels we want

#Now we can create a variable for the highest level of education achieved by any parent.
df_simple <- df_simple |>
  mutate(parent_edu = pmax(as.numeric(biodad_edu), as.numeric(biomom_edu), as.numeric(resdad_edu), as.numeric(resmom_edu), na.rm = TRUE)) |> #pmax returns the maximum value for each row
  mutate(parent_edu = factor(parent_edu, levels = 1:4, labels = names(ranking), ordered = TRUE)) #convert to factor with the levels we want

#Take a look to ensure we did what we meant to do.
df_simple

#Now we can drop the individual parent education variables and the cases where no parent education data is available.
df_simple <- df_simple |>
  select(-biodad_edu, -biomom_edu, -resdad_edu, -resmom_edu) |>
  filter(!is.na(parent_edu))

#We can save this dataset by writing to a csv. Note that I put my output data in a different directory than my input data.
write_csv(df_simple, "data/output/df_simple.csv")