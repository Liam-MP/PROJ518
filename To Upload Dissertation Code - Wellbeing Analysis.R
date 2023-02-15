library(haven)
library(ggplot2)
library(forcats)
library(tidyverse)
library(rstudioapi)
library(expss)
library(kableExtra)
library(data.table)
library(sjPlot)
library(plyr)
library(mice)
library(xgboost)
library(Matrix)
library(caret)
library(car)
library(randomForest)
library(multcomp)
library(RColorBrewer)
library(stats)
library(pdp)
library(ROCR)
library(dplyr)
library(webshot)
library(ggpubr)
library(magick)
library(reshape)
library(gridExtra)
library(ggmosaic)
library(webshot)
library(magick)

########## Setup ####################

# This code is intended to serve as a reference to accompany dissertation rather than be directly reproduced by following the code. Some file locations
# for example relate to my machine only and are not made generic.

# ONS Accredited Researcher status required for full ONS data set, certain variables from the survey are omitted for security reasons. Some like health status could be good.
# requires a course and accreditation to access. Add as a limitation and potential future study possibility with increased data access.

setwd(dirname(getActiveDocumentContext()$path)) # auto set working directory to source file location. Ensure "apsp_jd21_eul_pwta20.sav" data file is saved to same location

# Define a theme for all plots to follow. This will keep things consistent
custom_plot_theme <- function(...){
  theme_classic() %+replace%
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = .7, color = "black"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.key.size = unit(0.4, "cm"),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 12, colour = "black", angle = 90),
          plot.title = element_text(hjust = 0.5, size = 14))
}

# Set theme for all plots 
theme_set(custom_plot_theme())

options(knitr.kable.NA = '') # this will hide missing values in the kable table

########## Dataset Development and Initial Analysis of Variables ############

# On the APS, the personal well-being questions (Satis, Worth, Happy, Anxious) are only
# asked of persons aged 16 and above who gave a personal interview; (this is approximately
# 54% of the cases on the APS person dataset)

path = file.path("apsp_jd21_eul_pwta20.sav") # Jan - Dec data
dataset = read_sav(path) # Read in ONS data
#dataset = read_sav(path, user_na = TRUE) # Read in ONS data

well_dataset <- dataset %>%
  dplyr::select(c("ANXIOUS", "HAPPY", "SATIS", "WORTH")) # select the wellbeing variables only

write.csv(well_dataset,"WellbeingVariables.csv", row.names = FALSE) # write data

write.csv(test,"headings.csv", row.names = FALSE) # write heading data

# As we only want to consider variables where the respondent has completed ALL 4 wellbeing questions we filter out any that have not responded to all 4 results.
# This will enable comparisons between the 4 wellbeing variables as well as allow for a combined wellbeing score from all 4 to be generated if desired.
# Responses without completion of wellbeing questions are not useful, we would be unable to verify accuracy of any predictions made.
# Roughly 51.2% of respondents provided an answer for all 4 wellbeing questions
# 109198 results remain from original 212976 are as a result of removing non completions.This still provides a good initial sample size to work off.
# this sample size may reduce further as we remove, for example, children.

full_well_dataset <- dataset %>% filter_at(ggplot2::vars(ANXIOUS,HAPPY,SATIS,WORTH),all_vars(!is.na(.))) # filters out any NA well being rows where all 4 are NA

rm(dataset) # Remove full dataframe to save memory
gc() # clear memory after removal

write.csv(full_well_dataset,"full_wellbeing_dataset2.csv", row.names = FALSE) # write dataset containing the fields where all wellbeing questions answered

# The 517 variables available in addition to the 4 wellbeing questions were reviewed and considered alongside the goals of this study. An initial 34 variables have been
# identified for initial analysis and filtering of results. Some variables may be cut should they prove to have an obviously non significant predicted effect on wellbeing responses.
# Variables may also not be complete enough to use, for example if more than 50% of the total respondents did not answer the question or the question was not applicable
# to them, it may be removed. Smaller numbers of missing values may be filled via imputation.
# For questions relating to employment such as hours worked, we want to refer to hours worked in reference week rather than usual hours as this better reflects current
# situation when considering the wellbeing questions (how did you feel yesterday etc)

desired_variable_dataset <- full_well_dataset %>%  dplyr::select(c("MARCHK", "MARDY6", "RELIG11", "GROSS99", "GRSEXP", "GRSPRD", "SECEX", "SECGA", 
                                                            "SECGRO", "ADDJOB", "AGE", "ANXIOUS", "BENFTS", "CAIND", "CLAIMS14", "COUNTRY", "DISEA", "DURUN", 
                                                            "EMPLEN", "EMPMON", "ETHUKEUL", "FTPT", "GOR9D", "HAPPY", "HIQUL15D", "HOURPAY", "ILODEFR", "LIV12W", "RESBBY", "SATIS", 
                                                            "SEX", "STUCUR", "SUMHRS", "TEN1", "UOTHR", "WORTH"
)) # create dataset of just the desired variables. Will re-order this later for easier comprehension

write.csv(desired_variable_dataset,"desired_variable_dataset.csv", row.names = FALSE) # write dataset containing the selected fields

pct_var <- desired_variable_dataset%>% summarise_all(list(name = ~sum(is.na(.))/length(.))) # Dataframe of % NA values for selected variables
# just in case decide to present this as a table in write up for reference

pct_var_final <- pct_var %>% # round values
  mutate_if(is.numeric,
            round,
            digits = 2)

pct_var_final2 <- transpose(pct_var_final) # transpose data

rownames(pct_var_final2) <- colnames(pct_var_final) # set row names to col names of previous frame to show variable names

pct_var_final2 <- cbind(Row.Names = rownames(pct_var_final2), pct_var_final2) # add row names

colnames(pct_var_final2) <- c("VARNAME", "PCT_NA") # rename column headings

pct_var_final2$VARNAME <- gsub("_name", '', pct_var_final2$VARNAME) # remove _name from VARNAME col

pct_var_final2 <- pct_var_final2[order(pct_var_final2$PCT_NA, decreasing = TRUE), ] # sort by PCT_NA descending. This adds new row names as orig order number

kable(pct_var_final2, row.names = FALSE)%>% # display in a table without row names
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# Full NA table sorted by value
ggplot(data=pct_var_final2, aes(x= fct_reorder(VARNAME, PCT_NA), y= PCT_NA, alpha = PCT_NA)) +
  geom_bar(stat= "identity", fill= "#E3242B", width= 0.6, show.legend=FALSE) +
  labs(x = "Variable", y = "% NA") +
  coord_flip() +
  scale_fill_hue(l=40) +
  scale_alpha("VARNAME") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), breaks = seq(0, 2, by = 0.1), limits = c(0, max(pct_var_final2$PCT_NA) + 0.025)) +
  scale_x_discrete(expand = c(0, 0))

# Save HTML of table values and question description, use webshot to save as a PNG
sjPlot::view_df(desired_variable_dataset, alternate.rows = FALSE, show.id = FALSE, sort.by.name = TRUE, max.len = 0, show.type = FALSE, use.viewer	= FALSE, show.values = FALSE, show.string.values	
 = FALSE, file = "full_data_tab.html")
webshot("full_data_tab.html", "full_data_tab.png")

# Following further analysis of the volume of N/As in desired variables, a number of variables have been cut as deemed too incomplete or non-useful.
# "Note that the LFS/APS measures employees' earnings. The LFS/APS does not ask any earnings
# questions to the self-employed and does not collect information on money coming from any
# other source apart from wages; therefore the LFS/APS cannot be used to measure income". - lfs_user_guide_vol10_analysis_of_data_collected_by_lfs_2022
# For this reason, responses relating to earnings will not be considered in this study.
# Where N/A values make up 50% or more of all responses, careful consideration has been made to their inclusion. Many have a high volume due to question mapping, these
# are only answered where an answer to previous question feeds onto it, for example questions around cohabitation. To provide a more complete picture, variables
# may be combined. Other variables can be reworked to provide clearer insight.
# Some variables such as CLAIMS14 and BENFTS are interrelated, that is BENFTS indicates if person claiming any benefit, CLAIMS14 is if they are claiming unemployment.
# For this reason, BENFTS will be only variable used for this as only interested, for this study, in whether respondent claiming a benefit rather than the specific type.

# Following previous analysis of NA and other variables in details, a final list of 23 desired variables (including 4 wellbeing vars) is generated, note "LNGLST" and LIMACT 
# are included only for accurate representation of DISEA values and will be removed later. 
# These will form the base data set for the model.
# Variables will have their values changed to match that of the data dictionary, for example 1 = yes, 2 = no etc. This is to aid in comprehension of results and the
# model calculations themselves.
# A variable will later be combined to better suit purposes, living with spouse and living with partner are separate variables, these combine to indicate
# where living with partner in general regardless of official relationship status. MARDY6 gives details of relationship status for model.

final_desired_variable_dataset <- full_well_dataset %>%  dplyr::select(c("EMPMON", "ILODEFR", "MARCHK", "MARDY6", "RELIG11", "AGE", "ANXIOUS", "BENFTS", "COUNTRY", "DISEA", "LNGLST",
                                                                  "LIMACT", "ETHUKEUL", "GOR9D", "HAPPY", "HIQUL15D", "LIV12W", "SATIS", "SEX", "STUCUR", "SUMHRS", 
                                                                  "TEN1", "WORTH"
)) # create new dataset of just the desired variables. Will re-order this later for easier comprehension

write.csv(final_desired_variable_dataset,"final_desired_variable_dataset_pre_mods.csv", row.names = FALSE) # write dataset containing the selected fields

# Read in data from CSV
final_desired_variable_dataset <- read.csv("final_desired_variable_dataset_pre_mods.csv", stringsAsFactors=TRUE)

# TABLE OF NA GENERATION #

pct_var_final <- final_desired_variable_dataset%>% summarise_all(list(name = ~sum(is.na(.))/length(.))) # Dataframe of % NA values for newly selected variables
# just in case decide to present this as a table in write up for reference

pct_var_final <- pct_var_final %>% # round values
  mutate_if(is.numeric,
            round,
            digits = 2)

pct_var_final2 <- transpose(pct_var_final) # transpose data

rownames(pct_var_final2) <- colnames(pct_var_final) # set row names to col names of previous frame to show variable names

pct_var_final2 <- cbind(Row.Names = rownames(pct_var_final2), pct_var_final2) # add row names

colnames(pct_var_final2) <- c("VARNAME", "PCT_NA") # rename column headings

pct_var_final2$VARNAME <- gsub("_name", '', pct_var_final2$VARNAME) # remove _name from VARNAME col

pct_var_final2 <- pct_var_final2[order(pct_var_final2$PCT_NA, decreasing = TRUE), ] # sort by PCT_NA descending. This adds new row names as orig order number

kable(pct_var_final2, row.names = FALSE)%>% # display in a table without row names
  row_spec(0,bold=TRUE) %>% 
  column_spec (1:2,border_left = T, border_right = T) %>%
  kable_styling()

# Full NA table sorted by value
ggplot(data=pct_var_final2, aes(x= fct_reorder(VARNAME, PCT_NA), y= PCT_NA, alpha = PCT_NA)) +
  geom_bar(stat= "identity", fill= "#E3242B", width= 0.6, show.legend=FALSE) +
  labs(x = "Variable", y = "% NA") +
  coord_flip() +
  scale_fill_hue(l=40) +
  scale_alpha("VARNAME") +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), breaks = seq(0, 2, by = 0.1), limits = c(0, max(pct_var_final2$PCT_NA)))

# Can clearly see from the table that a number of variables still contain a large number of NA values. Some work needs to be done to better understand what makes up
# the question and whether it is valid for all participants.

# CLEANUP #

rm(desired_variable_dataset, full_well_dataset, well_dataset) # Remove now un required dataframes to save memory
gc() # clear memory after removal

########## Data Cleansing ###########

# COMBINE VARIABLES #

# Combining the LIV12W and MARCHK variables as previously discussed
# bring over and yes or no values (1 or 2) from one variable to the other and rename.NA will then indicate not in a relationship? Apply MARDY6 to LIV12W. this will retain
# NA values for LIV12W which are the more relevant (as question applies to anyone not married/in a civil partnership).

final_desired_variable_dataset <- final_desired_variable_dataset %>% mutate(LIV12W = case_when(MARCHK == 1 | LIV12W == 1 ~ 1, MARCHK == 2 | LIV12W == 2 ~ 2))
# As LIV12W is always NA if MARCHK is applicable (person married or civil partnership) we can say if MARCHK or LIV12W = 1, LIV12W should be 1, same for if 2.
# This preserves the NA values of LIV12W which then indicates person is not married or in a relationship (1 = lives with relationship, 2 = does not live with relationship)

# We will then rename this variable and label for better comprehension as now changed from base data
final_desired_variable_dataset <- dplyr::rename(final_desired_variable_dataset, LIVPTSP = LIV12W) # rename var
final_desired_variable_dataset <- apply_labels(final_desired_variable_dataset, LIVPTSP = "Whether spouse or partner is household member") # add new labels for data
val_lab(final_desired_variable_dataset$LIVPTSP) = make_labels("
                              1 Yes
                              2 No
                              ")

# Now we can remove MARCHK as no longer required
final_desired_variable_dataset <- final_desired_variable_dataset[ , ! names(final_desired_variable_dataset) %in% "MARCHK"] # remove MARCHK var from dataframe

# APPLY DICTIONARY VALUES AND CONVERT TYPE #

# Many numerical values in the dataset are coded and can be translated to an actual response, for example ILODEFR = 1 = in employment. These corresponding values can be 
# found in the data dictionary provided alongside the dataset from ONS (apsp_jd21_eul_pwta20_ukda_data_dictionary.rtf) as well as in the labels and attributes of the datset itself
# We will need to apply the actual values in order to understand and visualise output. We will also convert to standard data types here for easier usage

sjPlot::view_df(final_desired_variable_dataset, alternate.rows = FALSE, show.id = FALSE, use.viewer	= FALSE, file = "desired_data_tab.html" ) # provides HTML table of variables, labels, values and value labels. Could be good in appendices. Maybe best to make
webshot("desired_data_tab.html", "desired_data_tab.png")
# in excel instead due to changed variable types after modification? Maybe put this earlier to show the set after initial selection.

# Convert EMPMON var - Just 1 and 2 to convert here as others not present in data
final_desired_variable_dataset$EMPMON <- as.character(final_desired_variable_dataset$EMPMON) # convert type to allow manipulation

# Convert and translate ILODEFR var
final_desired_variable_dataset$ILODEFR <- as.character(final_desired_variable_dataset$ILODEFR) # convert type to allow manipulation
final_desired_variable_dataset$ILODEFR <- revalue(final_desired_variable_dataset$ILODEFR, c("1" = "In Employment", "2" = "ILO Unemployed", "3" = "Inactive")) # add dict translation values
# We have no other values in our sample so just 1,2,3 translated is fine. ILO unemployed refers to a definition of unemployment from 
# the International Labour Organization:
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/aguidetolabourmarketstatistics section 9.
# without a job, have been actively seeking work in the past four weeks and are available to start work in the next two weeks
# out of work, have found a job and are waiting to start it in the next two weeks

# Convert and translate MARDY6 var - Just 1 and 2 to convert here as others not present in data
final_desired_variable_dataset$MARDY6 <- as.character(final_desired_variable_dataset$MARDY6) # convert type to allow manipulation
final_desired_variable_dataset$MARDY6 <- revalue(final_desired_variable_dataset$MARDY6, c("1" = "Married/Cohabiting/Civil Partner", "2" = "Non married")) # add dict translation values

# Convert and translate RELIG11 var
final_desired_variable_dataset$RELIG11 <- as.character(final_desired_variable_dataset$RELIG11) # convert type to allow manipulation
final_desired_variable_dataset$RELIG11 <- revalue(final_desired_variable_dataset$RELIG11, c("1" = "No Religion", "2" = "Christian (all denominations)", "3" = "Budhist", 
                                                                                            "4" = "Hindu", "5" = "Jewish", "6" = "Muslim", "7" = "Sikh",
                                                                                            "8" = "Any Other Religion"
)) 

# Convert AGE var
final_desired_variable_dataset$AGE <- as.numeric(final_desired_variable_dataset$AGE) # convert type to allow manipulation

# Convert ANXIOUS var
final_desired_variable_dataset$ANXIOUS <- as.numeric(final_desired_variable_dataset$ANXIOUS)

# Convert and translate BENFTS var
final_desired_variable_dataset$BENFTS <- as.character(final_desired_variable_dataset$BENFTS) # convert type to allow manipulation
final_desired_variable_dataset$BENFTS <- revalue(final_desired_variable_dataset$BENFTS, c("1" = "Yes", "2" = "No")) # add dict translation values

# Convert and translate COUNTRY var
final_desired_variable_dataset$COUNTRY <- as.character(final_desired_variable_dataset$COUNTRY) # convert type to allow manipulation
final_desired_variable_dataset$COUNTRY <- revalue(final_desired_variable_dataset$COUNTRY, c("1" = "England", "2" = "Wales", "3" = "Scotland",
                                                                                            "4" = "Scotland North of Caledonian Canal", "5" = "Northern Ireland"
)) # add dict translation values

# Convert and translate DISEA var - Just 1 and 2 to convert here as others not present in data
final_desired_variable_dataset$DISEA <- as.character(final_desired_variable_dataset$DISEA) # convert type to allow manipulation
final_desired_variable_dataset$DISEA <- revalue(final_desired_variable_dataset$DISEA, c("1" = "Equality Act Disabled", "2" = "Not Equality Act Disabled")) # add dict translation values
# definitions for "Equality act disabled" can be found at https://www.gov.uk/definition-of-disability-under-equality-act-2010

# Convert and translate ETHUKEUL var
final_desired_variable_dataset$ETHUKEUL <- as.character(final_desired_variable_dataset$ETHUKEUL) # convert type to allow manipulation
final_desired_variable_dataset$ETHUKEUL <- revalue(final_desired_variable_dataset$ETHUKEUL, c("1" = "White", "2" = "Mixed/Multiple ethnic groups", "3" = "Indian",
                                                                                              "4" = "Pakistani", "5" = "Bangladeshi", "6" = "Chinese",
                                                                                              "7" = "Any other Asian background", "8" = "Black/African/Caribbean/Black British",
                                                                                              "9" = "Other ethnic group"
)) # add dict translation values

# Convert and translate GOR9D var
final_desired_variable_dataset$GOR9D <- as.character(final_desired_variable_dataset$GOR9D) # convert type to allow manipulation
final_desired_variable_dataset$GOR9D <- revalue(final_desired_variable_dataset$GOR9D, c("E12000001" = "North East", "E12000002" = "North West", 
                                                                                              "E12000003" = "Yorkshire and The Humber", "E12000004" = "East Midlands", 
                                                                                              "E12000005" = "West Midlands", "E12000006" = "East of England",
                                                                                              "E12000007" = "London", "E12000008" = "South East", 
                                                                                              "E12000009" = "South West", "N99999999" = "Northern Ireland" , 
                                                                                              "S99999999" = "Scotland", "W99999999" = "Wales"
                                                                                           
)) # add dict translation values

# Convert HAPPY var
final_desired_variable_dataset$HAPPY <- as.numeric(final_desired_variable_dataset$HAPPY)

# Convert and translate HIQUL15D var
final_desired_variable_dataset$HIQUL15D <- as.character(final_desired_variable_dataset$HIQUL15D)
final_desired_variable_dataset$HIQUL15D <- revalue(final_desired_variable_dataset$HIQUL15D, c("1" = "Degree or equivalent", "2" = "Higher education", "3" = 
                                                                                              "GCE A level or equivalent", "4" = "GCSE grades A*-C or equivalent", 
                                                                                              "5" = "Other qualification", "6" = "No qualification", "7" = "Don't know" 
)) # add dict translation values

# Convert and translate LIVPTSP var
final_desired_variable_dataset$LIVPTSP <- as.character(final_desired_variable_dataset$LIVPTSP)
final_desired_variable_dataset$LIVPTSP <- revalue(final_desired_variable_dataset$LIVPTSP, c("1" = "Yes", "2" = "No")) # add dict translation values

# Convert SATIS var
final_desired_variable_dataset$SATIS <- as.numeric(final_desired_variable_dataset$SATIS)

# Convert and translate SEX var
final_desired_variable_dataset$SEX <- as.character(final_desired_variable_dataset$SEX)
final_desired_variable_dataset$SEX <- revalue(final_desired_variable_dataset$SEX, c("1" = "Male", "2" = "Female")) # add dict translation values

# Convert and translate STUCUR var
final_desired_variable_dataset$STUCUR <- as.character(final_desired_variable_dataset$STUCUR)
final_desired_variable_dataset$STUCUR <- revalue(final_desired_variable_dataset$STUCUR, c("1" = "Yes", "2" = "No")) # add dict translation values

# Convert SUMHRS var
final_desired_variable_dataset$SUMHRS <- as.numeric(final_desired_variable_dataset$SUMHRS)

# Convert and translate TEN1 var
final_desired_variable_dataset$TEN1 <- as.character(final_desired_variable_dataset$TEN1)
final_desired_variable_dataset$TEN1 <- revalue(final_desired_variable_dataset$TEN1, c("1" = "Owned outright", "2" = "Being bought with mortgage or loan", 
                                                                                      "3" = "Part rent, part mortgage", "4" = "Rented", "5" = "Rent free or squatted", 
                                                                                      "6" = "Squatting" 
)) # add dict translation values
# Consider changing this variable to rented/owned/other? not sure, might be useless anyway

# Convert WORTH var
final_desired_variable_dataset$WORTH <- as.numeric(final_desired_variable_dataset$WORTH)

# Change var order for better comprehension
final_desired_variable_dataset <- final_desired_variable_dataset[,c("AGE", "SEX", "LNGLST", "LIMACT", "DISEA",  "STUCUR", "MARDY6", "LIVPTSP", "TEN1", "COUNTRY", "GOR9D", "ETHUKEUL", 
                                                                    "RELIG11", "HIQUL15D", "ILODEFR", "EMPMON", "SUMHRS", "BENFTS", "HAPPY", "WORTH", "SATIS", "ANXIOUS")]

# MODIFY VARIABLES #

# Now that the values are set and translated we need to tackle the issue of NA values. Any NA values present when it comes to modelling the data will cause the entire
# observation to be discounted. For this reason we need to understand what constitutes an NA response for each variable where they are present and come to a logical
# conclusion as to what to do with the NA's. For some variables, an NA response is entirely valid, the question was not asked as the respondent did not fit the
# categories relevant to the question, for example, benefits question is only asked to those under 70's except where 70+ and employed. For these we can simply replace
# the NA with "Not Applicable", "0", "no", whatever is relevant for the variable.
# For others, the reason it was NA might be more complex or entirely unexplained. Some questions should be asked regardless of responses to other questions or the respondents
# status. For these consideration will need to be made alongside other variables to understand why it is NA. Some may be easily explained and can then be changed, others
# may require imputation of results. If a considerable (2 or more) variables remain NA at the end of all analysis and changes, we can consider removing these responses
# from the data set entirely as their inclusion will not be beneficial. 

# On review of the questions, variable information and other resources available (ref the docs), we can infer that NA for LIVPTSP can be safely changed to 'No'. 
# NA implies that the person is not married/in civil partnership and not in a multi person household therefore the questions combining for LIVPTSP do not apply and 
# were not asked. Ultimately the result to "living with partner? y/n" becomes No if they they are not in an official relationship and not in multi person household.

final_desired_variable_dataset$LIVPTSP[is.na(final_desired_variable_dataset$LIVPTSP)]<-"No"
# Replace any NA values in LIVPTSP with "No"

# RELIG11 NA values are all for persons who chose not to answer the question (-8 value) for this reason we can change these NA to add a new category, "No Response"

final_desired_variable_dataset$RELIG11[is.na(final_desired_variable_dataset$RELIG11)]<-"No Response"
# Replace any NA values in RELIG11 with "No Response"

# HIQUL15D value derived from several qualification questions only asked to respondents under the age of 70 or in full time employment. Looking at the NA values we see that most NA are over
# the age of 69 and are not in work (ILODEFR) and can therefore conclude that these are in fact true NA's, the question wasn't asked as person does not fit required
# question parameters. For this reason we can change all matching that categorization to "Not Applicable" ensuring the observation is not dropped when modelling.
# There are however a small number (34) which still remain NA - this may be due to issues collating data for questions this is derived from or the answer not being provided. 
# As a category for 'don't know' already exists for HIQUL15D, the sensible thing, given the relatively low number of these, is to set remaining NA to 'don't know' as
# it is not possible to derive a truly accurate value.
# Again, as a category for 'Don't know' already exist

final_desired_variable_dataset$HIQUL15D[final_desired_variable_dataset$ILODEFR == "Inactive" & final_desired_variable_dataset$AGE > 69 | final_desired_variable_dataset$ILODEFR == "ILO Unemployed" & final_desired_variable_dataset$AGE > 69] <- "Not applicable"
# Replace HIQUL15D with "Not applicable" where ILODEFR = "ILO Unemployed" or "Inactive" & AGE > 69

final_desired_variable_dataset$HIQUL15D[is.na(final_desired_variable_dataset$HIQUL15D)]<-"Don't know"
# Replace 34 remaining NA values in HIQUL15D with "Don't know"

# EMPMON and SUMHRS are both questions that should only be asked to those currently employed. NA values should therefore relate only to people not in employment. The variable
# ILODEFR provides an indication of employment status. This means that if ILODEFR = "ILO Unemployed" or "Inactive", the values for both EMPMON and SUMHRS can be set to "0",
# the person is not employed and did not work any hours. 
# There is 1 observation however that is ILODEFR = "Inactive" but has hours for "SUMHRS", this is clearly an error, if hours were worked, the person is not inactive 
# in reference week, this single observation will be changed to SUMHRS = "0" as EMPMON is also NA further suggesting that the person does infact not work and the 
# work hours were misunderstood or input incorrectly .This is only 1 observation so effect is very minor. All others that are changed hold NA values for both 
# EMPMON and SUMHRS where ILODEFR = "ILO Unemployed" or "Inactive".
# Believe justified in coming to this conclusion as clearly majority of other records fit this pattern.
# Therefore, in our analysis, EMPMON = 0 will refer to those employed for less than 1 month continuously OR not in employment.

final_desired_variable_dataset$SUMHRS[final_desired_variable_dataset$ILODEFR == "Inactive" | final_desired_variable_dataset$ILODEFR == "ILO Unemployed" ] <- 0
# Replace SUMHRS with 0 where ILODEFR = "ILO Unemployed" or "Inactive".

final_desired_variable_dataset$EMPMON[final_desired_variable_dataset$ILODEFR == "Inactive" | final_desired_variable_dataset$ILODEFR == "ILO Unemployed" ] <- 0
# Replace EMPMON with 0 where ILODEFR = "ILO Unemployed" or "Inactive".

# BENFTS question applies to all respondents that are in paid or unpaid work or are aged between 16 and 69. Despite this, there are 82 records which are NA for persons
# falling in to the suitability categories. We can first change any true NA values (those aged over 69 and not in work) to 'Not Applicable'. Remaining NA will be removed
# in later step.

final_desired_variable_dataset$BENFTS[final_desired_variable_dataset$ILODEFR == "Inactive" & final_desired_variable_dataset$AGE > 69 | final_desired_variable_dataset$ILODEFR == "ILO Unemployed" & final_desired_variable_dataset$AGE > 69] <- "Not applicable"
# Replace BENFTS with 'Not Applicable' where AGE > 69 and ILODEFR = "ILO Unemployed" or "Inactive".

# DISEA is a derived variable where the value comes from two questions. The original root question is represented by LNGLST, a question that uses the following logic to see
# if it needs to be asked;
# (Age<=64) OR
# (Age<75 AND first contact) OR
# (Age>=75 AND Hprmb=1 AND first contact)
# if LNGLST is NA and respondent aged 65 or above, then we can infer that the logic above did not apply and, therefore, the question is truly not applicable and, 
# as such, DISEA is "Not Applicable". Those aged below 65 should have a response for the question and therefore will require imputation.
# LNGLST asks whether the respondent has health problems lasting or expected to last more than 1 year.
# LNGLST can also hold the value 3 = "Don't know" and 4 = "Refusal" both resulting in DISEA = NA. There are only a very small number of these and therefore is little benefit
# in delving further or adding additional categories to DISEA, these will instead also be changed to "Not Applicable" as it is not possible to infer an accurate value from
# unknown data.

final_desired_variable_dataset$LNGLST <- as.character(final_desired_variable_dataset$LNGLST)
final_desired_variable_dataset$LIMACT <- as.character(final_desired_variable_dataset$LIMACT)
# convert to characters as needed for next step

final_desired_variable_dataset$DISEA[is.na(final_desired_variable_dataset$LNGLST) & final_desired_variable_dataset$AGE >=65 | final_desired_variable_dataset$LNGLST == "3" | final_desired_variable_dataset$LNGLST == "4" ] <- "Not applicable"
# if LNGLST = NA and age greater than or equal to 65 or LNGLST = 3 or 4 , DISEA = "Not Applicable".

# Following these changes, the only NA values for DISEA are those who have answered '1' to LNGLST but provided no answer for LIMACT this amounts to 42 respondents.
# We will attempt to impute a value for these later rather than setting to 'Not applicable' as the initial LNGLST question does correctly apply to them, a valid response
# was given to this question, but no answer to the followup question LIMACT provided.

# Following data correction, if a respondent still has 2 or more NA responses they will be removed from the dataset. Values can be imputed however, when doing so for multiple
# variables, the value of the overall observation for that respondent reduces (ref?). This helps maintain confidence in the overall sample.
# This is a removal of 63 lines for a final total of 109135 observations

final_desired_variable_dataset$count_na <- rowSums(is.na(final_desired_variable_dataset[,5:22]))
# Add a column to data frame counting total NA values across most variables. First 4 skipped
# as we do not want to include the variables LIMACT and LNGLST in this count as these only remain to assist DISEA imputation later. Other 2 have no NA anyway.

final_desired_variable_dataset <- final_desired_variable_dataset[!(final_desired_variable_dataset$count_na >= 2),]
# Filters out any where count_na greater than or equal to 2 removing 63 rows

final_desired_variable_dataset$count_na <- NULL
# Removes now unrequired count_na column from dataframe

# Following these modifications, majority of variables are complete observations or near to. DISEA, TEN1, ETHUKEUL EMPMON, SUMHRS & BENFTS also still have some 
# NA but at a very low volume.
# On review of the data it can be seen that MARDY6 shares near perfect colinearity with LIVPTSP, that is to say, all but 1 person who is Married/Cohabiting/Civil Partner
# doesn't show as living with partner, LIVPTSP = NO, this is likely an error due to the extreme low number compared to the sample size. For this reason we will drop
# LIVPTSP from the final data set as including both it and MARDY6 serves no benefit and the anomaly would only cause issues.

final_desired_variable_dataset$LIVPTSP <- NULL

write.csv(final_desired_variable_dataset,"final_desired_variable_dataset_post_mods.csv", row.names = FALSE) # write dataset containing the selected fields

# CLEANUP #

rm(final_desired_variable_dataset, pct_var_final) # Remove now un required dataframes to save memory
gc() # clear memory after removal

# SPLIT AND IMPUTE VALUES #

# Following all of the work to correct and modify variables, 6 remain with NA values DISEA, TEN1, ETHUKEUL EMPMON, SUMHRS & BENFTS. Using the data available, we will
# impute values for these remaining NA amounts to 664 in total which works out at just 0.61% total imputation.
# Data should first be split as it will be for fitting the model prior to any imputation, failure to do this introduces 'data leakage' from test set to train (ref) which may
# unfairly bias model results as the test data may be affected by the train data which should not be the case (ref?)
# By first imputing values for the training data, the same logic and method can then be applied to the test data avoiding data leakage. This forms the pre-processing steps
# that would be carried out on any new set of prior to feeding into the model.

# First we split the data to training and testing sets

# Read in data from CSV
wellbeing_dataset <- read.csv("final_desired_variable_dataset_post_mods.csv", stringsAsFactors=TRUE)
# Data read back in with character variables as factors

wellbeing_dataset2 <- wellbeing_dataset

# drop remaining NA rows from dataset and just use completed. This results in a total of 664 lines removed (0.61%) which is
# pretty negligible. Likelihood of cutting these values really affecting overall observations and biasing seems low?
# Experiments with imputation via MICE introduced unwanted predicted values. For example, some NA values are changed however the respondent is not applicable to answer
# the adjusted variable. On the balance of things, given the volume of data available, we are OK to just remove this small amount of NA's rather than introduce potential
# bias and clear errors into the dataset.

wellbeing_dataset2$LNGLST <- NULL
wellbeing_dataset2$LIMACT <- NULL
# remove these variables first as not intended for model.

wellbeing_dataset2 <- na.omit(wellbeing_dataset2)
# removes any observations with na values present in any variable. If going this route, this should take place prior to
# the split as otherwise this would likely change the % split from 70/30.

write.csv(wellbeing_dataset2,"final_desired_variable_dataset_post_mods_narmv.csv", row.names = FALSE) # write dataset containing the selected fields

# Due to the large size of our data set we will use a 70/30 split for train/test (ref as to why this is good?).

set.seed(123) # set seed to make the results reproducible
rows <- nrow(wellbeing_dataset2) # get count of data rows to make the split more flexible
test2.subset <- sample(rows, rows*0.30) # take random 30% of rows
train2 <- wellbeing_dataset2[-test2.subset, ] # assign 70% to 'train'
test2 <- wellbeing_dataset2[test2.subset, ] # assign remaining 30% to 'test'
train2.subset <- setdiff(1:108471, test2.subset) # subset for potential later use - Gets all numbers NOT in test.subset

################################# end of split if doing deletion method rather than imputation!!!!!! ########################

wellbeing_dataset$LIMACT[is.na(wellbeing_dataset$LIMACT)] <- 0
wellbeing_dataset$LNGLST[is.na(wellbeing_dataset$LNGLST)] <- 0

### second split if attempting imputation ###

set.seed(321) # set seed to make the results reproducible
rows <- nrow(wellbeing_dataset) # get count of data rows to make the split more flexible
test.subset <- sample(rows, rows*0.30) # take random 30% of rows
train <- wellbeing_dataset[-test.subset, ] # assign 70% to 'train'
test <- wellbeing_dataset[test.subset, ] # assign remaining 30% to 'test'
train.subset <- setdiff(1:109135, test.subset) # subset for potential later use - Gets all numbers NOT in test.subset

##!!! MIGHT BE BETTER TO REMOVE REMAINING ROWS WITH NA VALUES DUE TO PREVIOUS WORK MAPPING AND CORRECTING THESE COLUMNS
## FOR EXAMPLE, BENFTS Q ONLY APPLIES TO THOSE AGED OVER 69 IF THEY ARE IN WORK. IMPUTATION MAY (AND HAS IN TEST) INCORRECTLY
## CLASSIFIED SOME AS "NOT APPLICABLE" WHO ARE UNDER 69 WHICH WOULD BE WHOLLY INCORRECT.
## WITH DEFAULT SETTINGS WE WOULD ALSO BE USING THE 4 WELLBEING VARIABLES AS FACTORS IN PREDICTIONS FOR IMPUTATION WHICH
## WOULD FURTHER BIAS THE DATA AS THESE WOULD BE UNKNOWN WHEN IT COMES TO ML MODEL MAKING PREDICTIONS FOR THOSE 4 VALUES!
## THE POTENTIAL FOR INCORRECT ENTIRES COUPLED WITH THE INHERANT BIAS INTRODUCED FROM IMPUTATION LIKELY OUTWEIGHS ANY BENEFITS
## DUE TO THE RELATIVE LOW NUMBER OF % MISSINGNESS?!
## ALTERNATIVELY, COULD THOSE INCORRECTLY CLASSIFIED BE CHANGED MANUALLY AS A FINAL STEP? IT'S A VERY SMALL AMOUNT?!!!!

# Next we can use the MICE (Multivariate Imputation by Chained Equations) package to impute our missing values. MICE creates multiple imputations (replacement values) 
# for multivariate missing data based on Fully Conditional Specification, where each incomplete variable is imputed by a separate model (ref).
# We will first do this for the train data and then for the test data

trainImp <- mice(train, m = 5, maxit = 20, seed = 500)
#trainImp <- mice(train, m = 5, maxit = 20, meth = meth, seed = 500)
# create imputed data set with above arguments
# default method for int values is pmm (predictive mean matching) and polyreg (polytomous regression imputation) for unordered categorical data (factor > 2 levels)

# !! above imputation takes an extremely long time. This has been saved out as 'imptest.csv' and 'imptrain.csv' if needed !!

# m is the number of imputations, generally speaking, the more the better. Originally (following Rubin, 1987) 5 was considered to be enough (hence the default). 
# So from an accuracy point of view, 5 may be sufficient. However, this was based on an efficiency argument only. 
# In order to achieve better estimates of standard errors, more imputations are needed. 
# These days there is a rule of thumb to use whatever the average percentage rate of missingness is - so if there is 30% missing data on average in a dataset, 
# use 30 imputations - see Bodner (2008) and White et al (2011) for further details.
# Our % missingness is very low so we will stick with the default imputations of 5

# maxit is the number of iterations for each imputation. mice uses an iterative algorithm. It is important that the imputations for all variables reach convergence, 
# otherwise they will be inaccurate. By inspecting the trace plots generated by plot() this can be visually determined. 
# Unlike other Gibbs sampling methods, far fewer iterations are needed - generally in the region of 20-30 or less as a rule of thumb. 
# When the trace lines reach a value and fluctuate slightly around it, convergence has been achieved. 
# We will start with 20 as the process will be quite slow when it comes to our factors using polyreg.

trainC <- complete(trainImp)
# apply imputed values. This isn't really correct as analysis should be done separately on each iteration of imputed data
# set, this is purely for testing. The above will just add values from the 1st iteration
# using a single set of imputed values does not account for the uncertainty associated with imputed values, which is what multiple imputation is all about.

# how to test and decide which set to use? Consider this, see how 1-5 vary visually and make a decision. Pool function? read up and decide?

testImp <- mice(test, m = 5, maxit = 20, seed = 400)
#testImp <- mice(test, m = 5, maxit = 20, meth = meth, seed = 400)
# create imputed data set with above arguments
# default method for int values is pmm (predictive mean matching) and polyreg (polytomous regression imputation) for unordered categorical data (factor > 2 levels)

testC <- complete(testImp)
# apply imputed values. This isn't really correct as analysis should be done separately on each iteration of imputed data
# set, this is purely for testing. The above will just add values from the 1st iteration.
# using a single set of imputed values does not account for the uncertainty associated with imputed values, which is what multiple imputation is all about.

trainC$LNGLST <- NULL
trainC$LIMACT <- NULL
testC$LNGLST <- NULL
testC$LIMACT <- NULL
# Removes now unrequired LNGLST and LIMACT columns from dataframes. Only used to assist generation of a more accurate DISEA value

# above approach or change remaining NA to 'missing'? Not sure of the value for this creating a new level for little gain
# given the low % missing and majority seem MCAR (missing completely at random) given the pre-processing steps that
# have taken place to improve the data completeness to this point.

######################## Data Visualisations #####################

# Now that the data has been cleansed, desired variables identified and NA values corrected or removed, we can take a closer look at the composition of the data 
# and do some initial visualisations to get an idea of the balance of data and the values represented.

wellbeing_vars <- wellbeing_dataset2 %>%  dplyr::select(c("ANXIOUS",  "SATIS", "WORTH", "HAPPY"))
wellbeing_vars <- melt(wellbeing_vars)

ggplot(wellbeing_vars, aes(x = value, y = fct_inorder(variable), fill = variable)) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.5) +
  labs(x = "Value",
       y = "Variable") +
  scale_fill_hue(l=40) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme(legend.position =" none")

# It can be observed from the above plot that the distribution of values for the wellbeing variables is very similar with the only variation being in the ANXIOUS
# variable. Answers to these questions are provided on a scale of 0 to 10, 0 is 'not at all' and 10 is 'completely'. The survey questions for each are;
# Overall, how satisfied are you with your life nowadays?
# Overall, to what extent do you feel the things you do in your life are worthwhile? 
# Overall, how happy did you feel yesterday?
# Overall, how anxious did you feel yesterday?
# With those questions in mind, we can see that the majority of people sit towards the upper end or 'positive' sides of the scale. The way ANXIOUS is worded we would
# expect the reverse, 0 being not at all anxious and therefore a positive measure of wellbeing. As with the other measures, responses for ANXIOUS do trend towards the
# positive end of the scale but with much more variation when compared to the other 3 variables. This suggests that reported feelings of anxiety vary more from person to person
# but, for the most part, the average level of anxiety reported is low.

# manipulate and extract values to create a nice table view of frequency of each value (probably a cleaner way of doing this but it works!)
counts <- ddply(wellbeing_vars, .(wellbeing_vars$variable, wellbeing_vars$value), nrow)
names(counts) <- c("variable", "value", "freq")

counts2 <- cbind(counts,
      reshape(counts, idvar = "value", timevar = "variable", direction = "wide")[,-1])

counts2$variable <- NULL
counts2$freq <- NULL
counts2 <- counts2[-c(12:44), ]
rownames(counts2) <- c("0","1","2","3","4","5","6","7","8","9","10")
colnames(counts2) <- c("Value", "ANXIOUS", "SATIS", "WORTH", "HAPPY")

# calc and add column for % of each value by variable
counts2 <- counts2 %>% mutate(A_pct = paste0(round(ANXIOUS/sum(ANXIOUS)*100,2),"%"))
counts2 <- counts2 %>% mutate(S_pct = paste0(round(SATIS/sum(SATIS)*100,2),"%"))
counts2 <- counts2 %>% mutate(W_pct = paste0(round(WORTH/sum(WORTH)*100,2),"%"))
counts2 <- counts2 %>% mutate(H_pct = paste0(round(HAPPY/sum(HAPPY)*100,2),"%"))

# reorder columns and rename
counts2 <- counts2 %>%  dplyr::select(c("Value", "HAPPY", "H_pct", "WORTH", "W_pct", "SATIS", "S_pct", "ANXIOUS", "A_pct"))
colnames(counts2) <- c("Value", "Count", "%", "Count", "%", "Count", "%", "Count", "%")

# table with additional headers for the wellbeing category
kable(counts2, row.names = FALSE, align = 'c')%>% # display in a table
  row_spec(0,bold=TRUE) %>%
  add_header_above(c(" " = 1, "HAPPY" = 2, "WORTH" = 2, "SATIS" = 2, "ANXIOUS" = 2)) %>%
  kable_styling(latex_options = 'hold_position', full_width = F)

# Taking a closer look at the frequency of each value further highlights the distribution of values across the 4 well being measures.
# As suggested by the box plots, we can see how values for HAPPY (58.78%), WORTH (65.69%) and SATIS (66.04%) are, in the majority, between 7-9 
# with ANXIOUS (78.51%) displaying majority between 0-5 with more of a spread across all values when compared to the other 3 variables. 
# 8 represents the greatest concentration of values for HAPPY (25.06%), WORTH (31.67%) and SATIS (32.49%) with ANXIOUS having a majority at 0 (28.54%).
# With these values in mind, we can expect that any classification model will likely have more difficulty correctly identifying values outside of the norm as
# there are less data points representing them. That is unless any predictor variables have a highly significant impact on the reported well being values. There is
# some potential that a model for ANXIOUS would perform better than the other 3 due to the wider spread of data points across the 11 possible values.

# Early indications from the data suggest that, for the most part, reported subjective wellbeing for the respondents as a whole is positive. 

# Looking at some of the identified variables of interest in isolation will help paint a picture of the overall distribution of values across the dataset, providing an
# idea of the population and individuals represented by the data. This can also be compared against UK population statistics to see how closely representation compares.

# generate a list of frequency of all variables in the data set
freqList = lapply(wellbeing_dataset2,
                  function(x) {
                    
                    my_lst = data.frame(table(x))
                    names(my_lst) = c("level", "n")
                    
                    return(my_lst) 
                  }
)

# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
DISEAfreq <- freqList$DISEA
DISEAfreq <- DISEAfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of DISEA value frequencies
ggplot(data=DISEAfreq, aes(x = fct_reorder(level, n), y = n, fill = level)) +
  geom_bar(stat= "identity", width= 1, show.legend=FALSE) +
  scale_fill_hue(l=40) +
  geom_text(size=5, color = "white", data= DISEAfreq, show.legend=FALSE, aes(x=fct_reorder(level, n),y=n,label=lab),vjust=0,hjust=1.15, ) +
  labs(x = "Disability Status", y = "Frequency") +
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(DISEAfreq$n)))
  
# Plot shows that majority of respondents are not considered equality act disabled. As explored in earlier data section, this question is not applicable to some respondents
# due to age and/or employment status etc. Recent UK estimates from the Family Resources Survey (https://commonslibrary.parliament.uk/research-briefings/cbp-9602/) 
# suggest that 22% of the total UK population are disabled. The ONS survey data does not include children and the DISEA question does not apply to those over pension age 
# (providing they are not in work). Despite this, the value of 25.66% reported is not all that dissimilar and therefore appears to align with and give a good representation 
# of the UK populations disability status.

# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
AGEfreq <- freqList$AGE
AGEfreq <- AGEfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of age value frequencies
ggplot(data=AGEfreq, aes(x = level, y = n, alpha = n)) +
  geom_bar(stat= "identity", width= 1, fill = "#013220", show.legend=FALSE) +
  scale_fill_hue(l=40) +
  labs(x = "Age", y = "Frequency") +
  scale_x_discrete(breaks=seq(20, 100, 5)) +
  scale_y_continuous(breaks=seq(0, max(AGEfreq$n + 100), 500), expand = c(0, 0), limits = c(0, max(AGEfreq$n + 100)))

# Plot displays the frequency of age across respondents ranging from the youngest (16) to the eldest (99). It is clear from this plot that a large proportion of respondents
# are aged between 50 and 74 with a fairly sharp drop off from 75 and over. The median age of respondents (59) and the mean (57.4) further highlights the visual suggestion
# that majority of respondents are around middle age.This should be considered when evaluating potential variable relationships and ML model performance.

# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
SEXfreq <- freqList$SEX
SEXfreq <- SEXfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of sex value frequencies
ggplot(data=SEXfreq, aes(x = fct_reorder(level, n, .desc = TRUE), y = n, fill = level)) +
  geom_bar(stat= "identity", width= 1, show.legend=FALSE) +
  scale_fill_hue(l=40) +
  geom_text(size=5, color = "white", data= SEXfreq, show.legend=FALSE, aes(x=fct_reorder(level, n),y=n,label=lab),vjust=1.25,hjust=0.5, ) +
  labs(x = "Sex", y = "Frequency") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(SEXfreq$n)))

# There are more 11.46% more females represented in the data than males. UK census data from 2011 (https://www.ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/demographics/male-and-female-populations/latest)
# indicates females account for 51% of the population to 48% males. Census data includes children and therefore cannot be directly compared, however, the overall higher proportion
# of females is represented in the ONS data albeit to a much higher degree.

# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
ILODEFRfreq <- freqList$ILODEFR
ILODEFRfreq <- ILODEFRfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of ILODEFR value frequencies
ggplot(data=ILODEFRfreq, aes(x = fct_reorder(level, n, .desc = TRUE), y = n, fill = level)) +
  geom_bar(stat= "identity", width= 1, show.legend=FALSE) +
  scale_fill_hue(l=40) +
  geom_text(size=5, color = "white", data= ILODEFRfreq, show.legend=FALSE, aes(x=fct_reorder(level, n),y=n,label=lab),vjust=1.25,hjust=0.5, ) +
  labs(x = "Economic Activity", y = "Frequency") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(ILODEFRfreq$n)))

# Survey data uses the International Labour Organization (ILO) definition of employment (https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/methodologies/aguidetolabourmarketstatistics section 9.)
# This defines unemployed as those without a job who have actively sought work in the last four weeks and are available to start work in the next two weeks; 
# or are out of work, have found a job and are waiting to start it in the next two weeks. Inactive therefore represents all those not in employment and not ILO unemployed.
# Clearly majority of data represents employed and inactive persons with just a comparatively low number of respondents ILO unemployed.
# The number of inactive is not surprising when considered alongside the age demographic represented within the data, a large number of respondents are past the UK state
# pension age (currently 66) and therefore general expectation is that majority of these people would no longer be in work.
# A dataset on UK ILO unemployment for those aged 16-64 (https://www.ons.gov.uk/employmentandlabourmarket/peoplenotinwork/unemployment/timeseries/lf2q/lms) reports a
# total of 4.65% unemployment. Applying the same age range to the data presented in this study returns a 2.69% ILO unemployment rate demonstrating a lower representation
# than that of the UK population.

# Plot of EMPMON bins = 30 with ILODEFR "ILO Unemployed" or "Inactive" removed
ggplot(subset(wellbeing_dataset2, ILODEFR %in% "In Employment"), aes(x=EMPMON)) +
  scale_x_continuous(breaks=seq(0, 900, 50)) +
  labs(x = "Months Cont. Employed", y = "Frequency") +
  geom_histogram(color="Gray", fill="Dark Green", bins = 30)

# Histogram displays the distribution of EMPMON values across respondents.
# Data has been filtered to remove those unemployed or inactive as these hold a default value of 0 and account for 49.25% of all respondents as shown in the ILODEFR plot.
# Plot shows that the majority of respondents (55.06%) have been continuously employed for 100 months or less with the frequency reducing somewhat consistently
# as number of months employed increases. The mean average months continuously employed (130.1) is a little under 11 years, (https://academic.oup.com/ej/article-abstract/106/435/334/5159021?redirectedFrom=fulltext) 
# found average UK job tenure between 1975-92 to be just under nine years, a 2017 report (https://www.cipd.co.uk/Images/7904-megatrends-insecurity-report-final_tcm18-61556.pdf)
# found that this had remained largely unchanged with just a small (2%) increase in those employed for 10 years continously or more.
# Survey data therefore represents a higher average value of continuous employment than established UK figures.

# Plot of SUMHRS bins = 20 with ILODEFR "ILO Unemployed" or "Inactive" removed as well as 0 values
ggplot(subset(wellbeing_dataset2, ILODEFR %in% "In Employment" & !(SUMHRS %in% '0')), aes(x=SUMHRS)) +
  scale_x_continuous(breaks=seq(0, 80, 10)) +
  labs(x = "Total Hrs Worked in Reference Week", y = "Frequency") +
  geom_histogram(color="Gray", fill="Dark Green", bins = 20)

# Histogram shows the distribution of SUMHRS values across respondents.
# As with the EMPMON plot, ILO unemployed and Inactive respondents have been removed.
# SUMHRS is a total of hours worked in primary and secondary jobs during the survey reference week. For this reason, there will be some who are employed but worked 0
# hours in reference week potentially due to sickness, annual leave or other absence from work. These values have been removed from this plot in order to provide a
# better base of comparison to established UK statistics and a clearer view of usual hours worked for respondents represented in this data.
# Plot shows large proportion of respondents sit within the middle of the scale or around 35-45hrs per week. Whilst there is no strict definition of a 'full-time' 
# worker in the UK this is generally considered to be 35+ hours per week (https://www.gov.uk/part-time-worker-rights#:~:text=There%20is%20no%20specific%20number,pension%20opportunities%20and%20benefits)
# therefore, a considerable proportion of the working respondents would be considered working full-time hours.
# (https://www.statista.com/statistics/280763/average-working-hours-uk/) suggests an average of around 35.2 hours worked per week for the period Jan-Dec 21, slightly
# higher observed here (34.6). The difference being minor suggests a good representation of average UK hours for the data.

# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
HIQUL15Dfreq <- freqList$HIQUL15D
HIQUL15Dfreq <- HIQUL15Dfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of HIQUL15D value frequencies
ggplot(data=HIQUL15Dfreq, aes(x = fct_reorder(level, n), y = n, fill = level)) +
  geom_bar(stat= "identity", width= 1, show.legend=FALSE) +
  scale_fill_hue(l=40) +
  coord_flip() +
  geom_text(size=5, color = "black", data= HIQUL15Dfreq, show.legend=FALSE, aes(x=fct_reorder(level, n),y=n,label=lab),vjust=0.5,hjust=-0.005, ) +
  labs(x = "Qualification Level", y = "Frequency") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(HIQUL15Dfreq$n + 2750)))

# Plot shows that the majority of those surveyed (applicable for the question) hold qualifications above GCSE A*-C levels. As with the DISEA variable, a considerable
# proportion (25.58%) of respondents did not fit the question criteria as HIQUL15D value is derived from several qualification questions which are only asked to respondents 
# under the age of 70 or 70+ and in full time employment.
# Comparing this to key points highlighted in the 2011 UK census, degree and higher education obtainment for those aged 16+ (27%) is lower than that presented in the above
# plot (https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/keystatisticsandquickstatisticsforlocalauthoritiesintheunitedkingdom/2013-12-04). 

ETHUKEULfreq <- freqList$ETHUKEUL
ETHUKEULfreq <- ETHUKEULfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Plot of ETHUKEUL value frequencies on a log2 scale
ggplot(data=ETHUKEULfreq, aes(x = fct_reorder(level, n), y = n, fill = level)) +
  geom_bar(stat= "identity", width= 1, show.legend=FALSE) +
  scale_fill_hue(l=40) +
  coord_flip() +
  geom_text(size=5, color = "white", data= ETHUKEULfreq, show.legend=FALSE, aes(x=fct_reorder(level, n),y=n,label=lab),vjust=0.5,hjust=1.05, ) +
  labs(x = "Ethnicity", y = "Frequency") + 
  scale_y_continuous(trans = "log2", expand = c(0, 0))

# A log2 scale has been used for clearer visibility and easier comparisons between smaller ethnicity populations.
# It is clear to see that white respondents account for the vast majority of responses in the data set which is largely to be expected in a survey spanning all regions
# of the UK. 2011 UK census data (https://www.ethnicity-facts-figures.service.gov.uk/uk-population-by-ethnicity/national-and-regional-populations/population-of-england-and-wales/latest)
# has 86% of the population as white with Asian ethnic groups accounting for the second largest proportion (7.5%) followed by Black ethnic groups (3.3%).
# When combining percentages for Asian ethnic groups in the data (3.29%) we can see how proportions follow the same size order as the 2011 census data.
# Clearly the data does not directly correlate with the UK census findings and therefore may not be fully representative of the overall UK population,
# however, mitigation such as UK census being inclusive of children and covering a different period of time should be considered.

# Initial impressions from these plots demonstrates population similarity with other UK wide studies with some limitations due to question applicability for some factors.
# Data being representative of the UK population helps add validity to findings as to factors affecting wellbeing which may be identified by this study.

# Final kable table of all categorical variables and their frequency and distribution for reference in report? Will be quite large but probably worth including.
# row for each category and variable as a sub row heading if possible? value and % of variable. Separate one for each variable? Not sure what is best.

# Prepare all sub tables of frequencies for display in kable
# Extract out the DF from list of DFs as easier to work with and add a column for % frequency
MARDY6freq <- freqList$MARDY6
MARDY6freq <- MARDY6freq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
COUNTRYfreq <- freqList$COUNTRY
COUNTRYfreq <- COUNTRYfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
GOR9Dfreq <- freqList$GOR9D
GOR9Dfreq <- GOR9Dfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
RELIG11freq <- freqList$RELIG11
RELIG11freq <- RELIG11freq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
BENFTSfreq <- freqList$BENFTS
BENFTSfreq <- BENFTSfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
TEN1freq <- freqList$TEN1
TEN1freq <- TEN1freq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))
STUCURfreq <- freqList$STUCUR
STUCURfreq <- STUCURfreq %>% mutate(lab = paste0(round(n/sum(n)*100,2),"%"))

# Append a new column to identify category
HIQUL15Dfreq$Category <- "HIQUL15D"
DISEAfreq$Category <- "DISEA"
ILODEFRfreq$Category <- "ILODEFR"
SEXfreq$Category <- "SEX"
STUCURfreq$Category <- "STUCUR"
MARDY6freq$Category <- "MARDY6"
COUNTRYfreq$Category <- "COUNTRY"
GOR9Dfreq$Category <- "GOR9D"
RELIG11freq$Category <- "RELIG11"
BENFTSfreq$Category <- "BENFTS"
ETHUKEULfreq$Category <- "ETHUKEUL"
TEN1freq$Category <- "TEN1"

# sort data by % order desc
HIQUL15Dfreq <- HIQUL15Dfreq[order(-HIQUL15Dfreq$n),]
DISEAfreq <- DISEAfreq[order(-DISEAfreq$n),]
ILODEFRfreq <- ILODEFRfreq[order(-ILODEFRfreq$n),]
SEXfreq <- SEXfreq[order(-SEXfreq$n),]
STUCURfreq <- STUCURfreq[order(-STUCURfreq$n),]
MARDY6freq <- MARDY6freq[order(-MARDY6freq$n),]
COUNTRYfreq <- COUNTRYfreq[order(-COUNTRYfreq$n),]
GOR9Dfreq <- GOR9Dfreq[order(-GOR9Dfreq$n),]
RELIG11freq <- RELIG11freq[order(-RELIG11freq$n),]
BENFTSfreq <- BENFTSfreq[order(-BENFTSfreq$n),]
ETHUKEULfreq <- ETHUKEULfreq[order(-ETHUKEULfreq$n),]
TEN1freq <- TEN1freq[order(-TEN1freq$n),]

# combine freq data, clarify headings, move column
FREQallcate <- rbind(HIQUL15Dfreq, DISEAfreq, ILODEFRfreq, SEXfreq, STUCURfreq, MARDY6freq, COUNTRYfreq, GOR9Dfreq, RELIG11freq, BENFTSfreq, ETHUKEULfreq, TEN1freq)
colnames(FREQallcate) <- c("Level", "Frequency", "%", "Category")
FREQallcate <- FREQallcate %>% relocate(Category, .before = Level)

# Tables containing distribution of values across all categorical variables. The 3 numerical variables (AGE, EMPMON and SUMHRS) have been omitted due to high range of
# possible values.
# table with additional row groupings for the categorical variables
kable(FREQallcate, row.names = FALSE, align = 'c')%>% # display in a table
  row_spec(0,bold=TRUE) %>%
  column_spec (1,border_right = T, bold = T) %>%
  collapse_rows(columns = 1) %>%
  kable_styling(latex_options = 'hold_position', full_width = F)

# Looking at the identified variables of interest against the wellbeing variables can provide an early suggestion of potential relationships. 

a <- ggplot(wellbeing_dataset2, aes(x = HAPPY, y = DISEA, fill = DISEA))
a + geom_violin() +
  geom_boxplot(width=0.05) +
  scale_fill_hue(l=40) +
  theme(legend.position = "none") +
  labs(x="HAPPY Value", y="Disability Status") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Plot shows distribution of happiness values for the 3 potentially disability statuses. Height of each portion indicates the density of that value, the higher the point,
# the more observations exist at that value.

# It can be observed that "Not Equality Act Disabled" and "Not applicable" share much the same distribution of values with both having median
# values of 8 and the same interquartile range (IQR) 7-9. Density of values for each point is also clearly very similar with few outliers existing from values 3-0. 
# "Equality Act Disabled" on the other hand visibility differs from both. Density of values is similar between values 5-9 indicating a broader distribution, this is 
# backed up by the IQR of 5-9. With a median HAPPY value of 7, general feelings of happiness are clearly lower for this category of respondent.

c <- ggplot(wellbeing_dataset2, aes(x = WORTH, y = DISEA, fill = DISEA))
c + geom_violin() +
  geom_boxplot(width=0.05) +
  scale_fill_hue(l=40) +
  theme(legend.position = "none") +
  labs(x="WORTH Value", y="Disability Status") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Distribution of values for all 3 categories is similar with all sharing the median value of 8. Again IQR for "Not Equality Act Disabled" and "Not applicable"
# is the same at 7-9. A reduction in density can be seen at points other than the median compared to the HAPPY plot indicating slightly less variation in values.
# "Equality Act Disabled" IQR spans values 6-9 again demonstrating a greater distribution of values across the points, this is further evidenced in the
# height of each section from 6-2 exceeding that of the other two categories. Again, this plot suggests overall feelings of worth are reduced for disabled respondents
# albeit to lesser of an extent posed in the HAPPY plot.

d <- ggplot(wellbeing_dataset2, aes(x = SATIS, y = DISEA, fill = DISEA))
d + geom_violin() +
  geom_boxplot(width=0.05) +
  scale_fill_hue(l=40) +
  theme(legend.position = "none") +
  labs(x="SATIS Value", y="Disability Status") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Again IQR's for "Not Equality Act Disabled" and "Not applicable" remain stable between 7-9 with a median of 8, a minor increase in density can be seen at all non-median
# points compared to WORTH demonstrating slightly more variation across the respondent values.
# "Equality Act Disabled" respondents again clearly differs, IQR of 6-8 and a median of 7 has similarities with the HAPPY plot but demonstrates less distribution across the values
# as a whole suggesting generally higher feelings of satisfaction for this category in comparison to happiness and worth.

b <- ggplot(wellbeing_dataset2, aes(x = ANXIOUS, y = DISEA, fill = DISEA))
b + geom_violin() +
  geom_boxplot(width=0.05) +
  scale_fill_hue(l=40) +
  theme(legend.position = "none") +
  labs(x="ANXIOUS Value", y="Disability Status") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# As with the other 3 plots, the IQR and median values for "Not Equality Act Disabled" and "Not applicable" are the same for the ANXIOUS variable, height of the sections
# and expanded IQR across the scale demonstrates a much greater variation in values compared to the other wellbeing variables. 
# Disabled respondents again trend towards the negative end of the scale reporting increased feelings of anxiety on comparison. The median value sits 2 points below the
# other categories suggesting a more significant correlation here than observed in the other 3 wellbeing variables.

# Looking at all four wellbeing variables alongside the DISEA factor demonstrates a clear difference in observed values between the 3 options. For the most part, 
# "Not Equality Act Disabled" and "Not applicable" share much the same distribution of values with only minor differences in density observed for some points.
# "Equality Act Disabled" respondents consistently reported lower (or higher for anxiety) values on comparison for all 4 wellbeing variables. WORTH demonstrated
# this to a lesser extent than the others whilst ANXIOUS negatively deviated more so than the other 3 variables. As a whole these plots suggest a marked lower subjective
# wellbeing for disabled individuals on the balance of evidence.
# One important note to consider is that some of the 'Not applicable' category may in fact be disabled but were not eligible for the question in the survey due to age
# and/or employment status as noted in the data section.

# get mean values and prep data for plotting
age_means <- aggregate(cbind(wellbeing_dataset2$HAPPY, wellbeing_dataset2$ANXIOUS, wellbeing_dataset2$WORTH, wellbeing_dataset2$SATIS) ~ AGE, data = wellbeing_dataset2, FUN = mean)
colnames(age_means) <- c("AGE", "HAPPY", "ANXIOUS", "WORTH", "SATIS")
age_means <- melt(age_means, id="AGE")
colnames(age_means) <- c("AGE", "Variable", "Value")

ggplot(age_means, aes(x = AGE, y = Value, color = Variable)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x="Age", y="Mean Average Wellbeing Value") +
  scale_y_continuous(limits = c(2,9), expand = c(0, 0), breaks = seq(0, 10, 1)) +
  scale_x_continuous(limits = c(15,100), expand = c(0, 0), breaks = seq(15, 100, 5))

# plot of mean wellbeing value by age.

# Above shows mean values for each age point 16-99. A regression line has been added for each wellbeing category providing a suggestion of how values fluctuate as
# age increases. 
# Firstly, looking at ANXIOUS, the regression line suggests feelings of anxiety reduce as respondent age increases. There are some visible outliers 
# at the extremes of the Age scale. Notable deviations from the line as a whole are fairly minimal however, there appears to be a section continuously below the line
# beginning around age 60 to 85.
# Looking a little closer at the WORTH variable, the line noticeably trends down as age increases with clear drops towards the high end of the scale. Values are mostly
# stable from around ages 30-60 at which point the values increase before eventual drop off towards end of scale.
# SATIS looks to reduce as age increases but not as significantly as seen in WORTH. Again, a portion of values above the regression line begins from around 65 to 85 age range.
# As with the other variables, a noticeable drop off in SATIS takes place at the higher end of the age scale.
# Average HAPPY value looks to increase alongside age. Noticeable dips to the average exist around 40-60 before an increase. As with the other variables, there are
# noticeable decreases towards the very end of the age scale.
# The pattern of improvement for values around ages 65-85 is observed for all 4 wellbeing variables, this could be indicative of a general subjective wellbeing improvement 
# post general UK retirement age of 65 up until the noticeable drop offs at 85+. HAPPY, WORTH and SATIS all follow a similar pattern with values starting towards the lower
# end of the scale before rising slightly, an overall dip then precedes the aforementioned increase from 65-85 before an eventual drop off at the end of the scale.
# ANXIETY variable deviates from the pattern of the other three with a seemingly more consistent drop in value as age increases with only the odd significant deviation
# from the line. This is suggestive of feelings of anxiety being higher in younger years reducing gradually as one gets older potentially reflecting more stability and less
# concerns for the future.
# It is important to consider that values for the youngest and oldest respondents account for a much smaller overall proportion of the data (as seen in age distribution plot),
# this will increase volatility of the values and can therefore not be considered as significant as suggestions for the rest of the population.

# Copy wellbeing dataframe to convert wellbeing values to factor for plotting
wellbeing_dataset3 <- wellbeing_dataset2
wellbeing_dataset3$HAPPY <- as.factor(wellbeing_dataset3$HAPPY)
wellbeing_dataset3$WORTH <- as.factor(wellbeing_dataset3$WORTH)
wellbeing_dataset3$SATIS <- as.factor(wellbeing_dataset3$SATIS)
wellbeing_dataset3$ANXIOUS <- as.factor(wellbeing_dataset3$ANXIOUS)

# Calculate mean values of wellbeing variables by sex
HAPPYmeanS <- ddply(wellbeing_dataset2, "SEX", summarise, grp.mean=mean(HAPPY))
WORTHmeanS <- ddply(wellbeing_dataset2, "SEX", summarise, grp.mean=mean(WORTH))
SATISmeanS <- ddply(wellbeing_dataset2, "SEX", summarise, grp.mean=mean(SATIS))
ANXIOUSmeanS <- ddply(wellbeing_dataset2, "SEX", summarise, grp.mean=mean(ANXIOUS))

# make new frame of data for turning long
wellbeing_dataset4 <- wellbeing_dataset3[, c("SEX", "HAPPY", "WORTH", "SATIS", "ANXIOUS")]

# make data long for facet plotting
SEX_long <- wellbeing_dataset4 %>%
  gather(Description, Value, HAPPY:ANXIOUS)

# convert to value for next step
SEX_long$Value <- as.numeric(SEX_long$Value)

# add new column to df with value categories high/low/mid
SEX_long <- SEX_long %>%
  mutate(Value_group = case_when(
    Value > 5 ~ "High",
    Value < 5 ~ "Low",
    Value == 5 ~ "Mid")
  )

# make factors and re-order for plot
SEX_long$Description <- factor(SEX_long$Description, levels=c("HAPPY", "SATIS", "WORTH", "ANXIOUS"))
SEX_long$Value <- factor(SEX_long$Value, levels=c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
SEX_long$Value_group <- factor(SEX_long$Value_group, levels=c("High", "Mid", "Low"))

# stacked bar percentage chart of wellbeing vars by SEX
ggplot(SEX_long, aes(x = SEX, fill = Value)) +
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent, breaks = seq(0, 1, .2), expand = c(0, 0)) +
  scale_fill_hue(l=40) +
  labs(x="Sex", y="Percent") +
  facet_grid(.~Description)

# describe the plot and also mention the mean value to confirm any assumptions made. Is mean value for one higher than the other?
# Would plot mean on graph but doesn't really make sense on % scale.

# The above stacked bar chart shows the four wellbeing value distributions by sex. At a glance there is no dramatic dissimilarity between any one value section comparing
# males and females, no single section is significantly larger than it's neighbor. There are however small differences in some of the value sections providing an indicator
# of which sex reported the higher average value. The more aligned the coloured sectors are, the greater the similarity in distribution. Despite some differences, notably
# in the higher value sections, distributions for HAPPY and SATIS for male and female are overall quite well aligned. Comparing this to the WORTH bars, a more noticeable
# difference in alignment is observed suggesting that feelings of worth for males is lower than females. Dissimilarity is also observed in the ANNXIOUS bars, this time
# suggesting that feelings of anxiety are greater for females compared to males.
# These observations can be confirmed by looking at the mean values for each wellbeing measure. 

# Combine means for SEX values as defined earlier
SEXmeans <- HAPPYmeanS
SEXmeans$SATIS <- cbind(SATISmeanS$grp.mean)
SEXmeans$WORTH <- cbind(WORTHmeanS$grp.mean)
SEXmeans$ANXIOUS <- cbind(ANXIOUSmeanS$grp.mean)

# rename columns for comprehension
colnames(SEXmeans) <- c("SEX", "HAPPY", "SATIS", "WORTH", "ANXIOUS")

# round any numeric values
SEXmeans <- SEXmeans %>% 
    mutate_if(is.numeric, round, digits = 2)

# Table of mean wellbeing values by SEX
kable(SEXmeans, row.names = FALSE, align = 'c')%>% # display in a table
  row_spec(0,bold=TRUE) %>%
  column_spec (1,border_right = T, bold = T) %>%
  kable_styling(latex_options = 'hold_position', full_width = F) 

# As suggested in the stacked bar plot, there are differences in the average values for all of the measures with more significance observed for WORTH and ANXIOUS
# values.

# Grouping the SWB values for into high (10-6), mid (5) and low (0-4) categories provides another angle of comparison for SWB measures between the sexes.

# stacked bar percentage chart of wellbeing vars by SEX
ggplot(SEX_long, aes(x = SEX, fill = Value_group)) +
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent, breaks = seq(0, 1, .2), expand = c(0, 0)) +
  scale_fill_hue(l=40) +
  guides(fill=guide_legend(title="Group")) +
  labs(x="Sex", y="Percent") +
  facet_grid(.~Description)

# Simplifying values into groups enables a less detailed but visually clearer comparison of overall feelings for each measure. Comparing to previous plot and the mean
# values, it remains that HAPPY and SATIS values for males is generally higher than that of females. ANXIOUS again reflects previous observations demonstrating higher
# overall feelings of anxiety for females than males with this measure demonstrating the clearest overall difference. 
# Interestingly, when categorising in this manner,
# the observation for WORTH appears less significant than previously suggested with the two bars being near identical. This means that variations in observed values 
# between sexes must take place within the high and/or low ranges specifically rather than being a result of more mid, low or high values. 

# HAPPY boxplot by TEN1
ggplot(wellbeing_dataset2, aes(x = HAPPY, y = TEN1, fill = TEN1)) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.5) +
  labs(x = "HAPPY Value",
       y = "Accommodation Status") +
  scale_fill_hue(l=40) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme(legend.position =" none")

# Looking at HAPPY values by accommodation status it can be seen that majority of distributions are similar and all share a mean value of 8. Categories for "Rented" and
# "Part rent, part mortgage" demonstrate a wider spread of values towards the lower end on comparison suggesting slightly lower happiness levels. Whiskers for these
# extending beyond the others further demonstrates increased variability outside of the quartiles.

# WORTH boxplot by TEN1
ggplot(wellbeing_dataset2, aes(x = WORTH, y = TEN1, fill = TEN1)) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.5) +
  labs(x = "WORTH Value",
       y = "Accommodation Status") +
  scale_fill_hue(l=40) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme(legend.position =" none")

# Distributions for these values are all much the same suggesting little to no effect of accommodation status on WORTH values. Small variations can be seen in the
# volume of outliers for "Part rent, part mortage".

# SATIS boxplot by TEN1
ggplot(wellbeing_dataset2, aes(x = SATIS, y = TEN1, fill = TEN1)) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.5) +
  labs(x = "SATIS Value",
       y = "Accommodation Status") +
  scale_fill_hue(l=40) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme(legend.position =" none")

# "Rented" category displays the lowest mean average value at 7 compared to 8 for all other categories, further variance of values is also displayed by the whiskers.
# Variation can also be seen for "Part rent, part mortgage" and "Being bought with mortgage or loan" categories which share similar distributions, these plots suggest a
# lower concentration of values above the mean for these categories.

# ANXIOUS boxplot by TEN1
ggplot(wellbeing_dataset2, aes(x = ANXIOUS, y = TEN1, fill = TEN1)) +
  geom_boxplot(outlier.size = 2, outlier.alpha = 0.5) +
  labs(x = "ANXIOUS Value",
       y = "Accommodation Status") +
  scale_fill_hue(l=40) +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme(legend.position =" none")

# As observed in other plots for the ANXIOUS category, values are much more distributed across the scales providing evidence of more variability of feelings of anxiety
# compared to the 3 other SWB variables. As with the HAPPY plot, distributions for all but "Rented" and "Part rent, part mortgage" categories are the same. 
# "Rented" and "Part rent, part mortgage" have mean values below the others at 3 suggesting higher anxiety for renters, upper quartile also extends further to 6
# providing further evidence for this observation.
# Findings should be considered alongside the frequency values for each category. "Rent free or squatted" and "Part rent, part mortgage" represent a much lower
# proportion of the respondents on comparison to the other categories and therefore observations do not carry as much weight. With "Rented" accounting for 21.49%
# of all respondents, any variation from the average can be considered as more significant. Initial evidence suggests that home ownership, be that "Owned outright"
# or "Being bought with mortgage or loan" results in higher SWB values for all categories bar WORTH when compared to "Rented".
# Another important consideration is that home ownership tends to be higher for those aged 65 and up in England. (https://www.statista.com/statistics/321065/uk-england-home-owners-age-groups/)
# With this in mind, comparisons can be made between relationships observed for these plots to those shown in the "mean wellbeing by age" plot which generally
# demonstrated higher SWB values for those 65 and up. Increased overall financial stability obtained by owning ones house, which is more likely from 65+, may contribute
# to higher feelings of SWB?

########## Analysis ############

# ANCOVA - Analysis of covariance #

# Further statistical analysis will allow for more informed and detailed conclusions to be made on the potential relationships between variables and SWB values.
# ANCOVA analysis will provide a view on strength of interactions between our independent variables and the four SWB dependent variables whilst controlling for the effects of 
# each independent variable selected (Rutherford, Andrew).
 
# ANCOVA will run on all available variables with the exception of COUNTRY. This is because COUNTRY and GOR9D (UK Region) has collinearity, COUNTRY = Scotland, Northern
# Ireland and Wales GOR9D = the same. Leaving both in would cause errors when attempting to summarise the results.
# (https://www.introspective-mode.org/assumption-multicollinearity/)

# Run ANCOVA (Analysis of covariance) test using all available variables for each swb (subjective wellbeing) category 
hfit = aov(HAPPY~AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, wellbeing_dataset2)
wfit = aov(WORTH~AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, wellbeing_dataset2)
sfit = aov(SATIS~AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, wellbeing_dataset2)
afit = aov(ANXIOUS~AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D++ILODEFR+EMPMON+SUMHRS+BENFTS, wellbeing_dataset2)

# View results of ANCOVA models
summary.lm(hfit)
summary.lm(wfit)
summary.lm(sfit)
summary.lm(afit)

# view results at factor group level adding to new object
hfittab <- Anova(hfit, type = "III", singular.ok = TRUE)
wfittab <- Anova(wfit, type = "III", singular.ok = TRUE)
sfittab <- Anova(sfit, type = "III", singular.ok = TRUE)
afittab <- Anova(afit, type = "III", singular.ok = TRUE)

# A p-value of less than 0.05 suggests significance of a given variable towards the value of the continuous dependent variable, in this case our 4 well being variables. 

## HAPPY ANCOVA ##

# create table showing results of ANCOVA for HAPPY omitting the (Intercept) value
colnames(hfittab) <- c("SS", "df", "$F$", "$p$")

hfittab$Signif <- ""
hfittab$Signif[hfittab[4] < 0.05 ] <- "Yes"
hfittab$Signif[hfittab[4] > 0.05 ] <- "No"

kable(hfittab, booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)%>%
  save_kable("HappyAnc1.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/HappyAnc1.html", "HappyAnc1.png")

# Initial ANCOVA results on HAPPY variable suggests significance of AGE, DISEA, STUCUR, MARDY6, TEN1, GOR9D, ETHUKEUL, RELIG11, HIQUL15D, ILODEFR variables to the HAPPY value

# Insignificant variables, those with the highest p value, were then dropped one by one and the model re-reviewed until left with just those having a significant p-value 

hfit2 = update(hfit, ~.-SEX)
hfit2 = update(hfit2, ~.-BENFTS)
hfit2 = update(hfit2, ~.-SUMHRS)
hfit2 = update(hfit2, ~.-EMPMON)
hfittab2 <- Anova(hfit2, type = "III", singular.ok = TRUE)

# create table showing results of final ANCOVA for HAPPY omitting the (Intercept) value
colnames(hfittab2) <- c("SS", "df", "$F$", "$p$")

# create table of values descending by p value
kable(hfittab2[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)%>%
  save_kable("HappyAnc2.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/HappyAnc2.html", "HappyAnc2.png")

# From this final table we can see that the same variables remained significant whilst no longer controlling for the values of those removed. Value for GOR9D suggests
# slightly less significance when compared to the others.

hfittab3 <- summary.lm(hfit2)
hfittab3 <- as.data.frame(hfittab3$coefficients)

# create table showing summary results of final ANCOVA for HAPPY omitting the (Intercept) value
colnames(hfittab3) <- c("Estimate", "Std. Error", "$t$", "$p$")

hfittab3$Signif <- "No"
hfittab3$Signif[hfittab3[4] < 0.05 ] <- "Yes"

summary.lm(hfit2)

kable(hfittab3[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F) %>%
  save_kable("HappyAnc3.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/HappyAnc3.html", "HappyAnc3.png")

# By viewing a summary of coefficients for the final ANCOVA model, we can see the significance of each possible category option towards the HAPPY value.
# Estimate indicates the estimated effect of each selection on the HAPPINESS variable, that is to say for each increase in AGE there is a 0.006 estimated increase
# to the happiness value DISEA = Not Equality Act Disabled 0.836 point higher happiness on average than disabled

## WORTH ANCOVA ##

# create table showing results of ANCOVA for WORTH omitting the (Intercept) value
colnames(wfittab) <- c("SS", "df", "$F$", "$p$")

wfittab$Signif <- ""
wfittab$Signif[wfittab[4] < 0.05 ] <- "Yes"
wfittab$Signif[wfittab[4] > 0.05 ] <- "No"

Anova(wfit, type = "III", singular.ok = TRUE)

kable(wfittab, booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# Initial ANCOVA results on WORTH variable suggests significance of all variables to the WORTH value so we do not need to re model and remove any variables. Values for SUMHRS and
# ETHUKEUL being slightly less significant than the others

wfittab3 <- summary.lm(wfit)
wfittab3 <- as.data.frame(wfittab3$coefficients)

# create table showing summary results of final ANCOVA for WORTH omitting the (Intercept) value
colnames(wfittab3) <- c("Estimate", "Std. Error", "$t$", "$p$")

wfittab3$Signif <- "No"
wfittab3$Signif[wfittab3[4] < 0.05 ] <- "Yes"

kable(wfittab3[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F) %>%
  save_kable("WorthAnc3.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/WorthAnc3.html", "WorthAnc3.png")

## SATIS ANCOVA ##

# create table showing results of ANCOVA for SATIS omitting the (Intercept) value
colnames(sfittab) <- c("SS", "df", "$F$", "$p$")

sfittab$Signif <- ""
sfittab$Signif[sfittab[4] < 0.05 ] <- "Yes"
sfittab$Signif[sfittab[4] > 0.05 ] <- "No"

kable(sfittab, booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# Initial ANCOVA results on SATIS variable suggests significance of all variables to the SATIS value apart from EMPMON

# Insignificant variables, those with the highest p value, were then dropped one by one and the model re-reviewed until left with just those having a significant p-value 

sfit2 = update(sfit, ~.-EMPMON)
sfittab2 <- Anova(sfit2, type = "III", singular.ok = TRUE)

# create table showing results of final ANCOVA for HAPPY omitting the (Intercept) value
colnames(sfittab2) <- c("SS", "df", "$F$", "$p$")

kable(sfittab2[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# From this final table we can see that the same variables remained significant whilst no longer controlling for the values of those removed. Value for SEX suggests
# slightly less significance when compared to the others followed by ETHUKEUL and SUMHRS.

sfittab3 <- summary.lm(sfit2)
sfittab3 <- as.data.frame(sfittab3$coefficients)

# create table showing summary results of final ANCOVA for SATIS omitting the (Intercept) value
colnames(sfittab3) <- c("Estimate", "Std. Error", "$t$", "$p$")

sfittab3$Signif <- "No"
sfittab3$Signif[sfittab3[4] < 0.05 ] <- "Yes"

kable(sfittab3[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F) %>%
  save_kable("SatisAnc3.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/SatisAnc3.html", "SatisAnc3.png")

## ANXIOUS ANCOVA ##

# create table showing results of ANCOVA for ANXIOUS omitting the (Intercept) value
colnames(afittab) <- c("SS", "df", "$F$", "$p$")

afittab$Signif <- ""
afittab$Signif[afittab[4] < 0.05 ] <- "Yes"
afittab$Signif[afittab[4] > 0.05 ] <- "No"

kable(afittab, booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# Initial ANCOVA results on ANXIOUS variable suggests significance of all variables apart from STUCUR and EMPMON

# Insignificant variables, those with the highest p value, were then dropped one by one and the model re-reviewed until left with just those having a significant p-value 

afit2 = update(afit, ~.-STUCUR)
afit2 = update(afit2, ~.-EMPMON)
afittab2 <- Anova(afit2, type = "III", singular.ok = TRUE)

# create table showing results of final ANCOVA for ANXIOUS omitting the (Intercept) value
colnames(afittab2) <- c("SS", "df", "$F$", "$p$")

kable(afittab2[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# From this final table we can see that the same variables remained significant whilst no longer controlling for the values of those removed. Value for RELIG11 and SUMHRS suggests
# slightly less significance when compared to the others.

afittab3 <- summary.lm(afit2)
afittab3 <- as.data.frame(afittab3$coefficients)

summary.lm(afit2)

# create table showing summary results of final ANCOVA for ANXIOUS omitting the (Intercept) value
colnames(afittab3) <- c("Estimate", "Std. Error", "$t$", "$p$")

afittab3$Signif <- "No"
afittab3$Signif[afittab3[4] < 0.05 ] <- "Yes"

kable(afittab3[-1, ], booktabs = T, digits = c(3,3,3,32))%>% # rounds first 3 columns to 3 decimal, retains pvalue science notation at 32 dec
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F) %>%
  save_kable("AnxAnc3.html");  webshot::webshot("C:/Users/Liam/Desktop/Coursework/PROJ518/R Code/AnxAnc3.html", "AnxAnc3.png")

# By viewing the summary of the final ANCOVA model, we can see the significance of each possible category option towards the ANXIOUS value.
# Estimate indicates the estimated effect of each selection on the ANXIOUS variable, that is to say for each increase in AGE there is a 0.006 estimated decrease
# to the ANXIOUS value (WHICH MEANS LESS ANXIOUS AS 10 = EXTREMELY ANXIOUS) ? Males less anxious than females, disabled more anxious than not disabled

## distribution - model assumptions/diagnostics of ANCOVA (expect skewed but not a problem due to data size so conclusions still valid) ##

# You never look at the results of the test until AFTER you check that the assumptions of the model were met. 
# So next we will examine if our data meet the assumptions of 1) residuals following a normal distribution and 2) equal variance among groups
# (the residuals from each group should be relatively similar).

# The difference between the observed and predicted values, called the residuals, is a measure of the error associated with each observation. 
# It is the variation that is not explained by the explanatory variable. We plot the residuals in various ways to examine normality and homogeneity of variances.

# To test whether the residuals are normally distributed (model assumption 1 above), you will use 2 pieces of information:

# 1. a density plot of residuals
# 2. a Q-Q plot of residuals

#code to create the density plot of residuals
par(mfrow=c(2,2))
plot(density(hfit2$residuals), main = "HAPPY")
plot(density(wfit$residuals), main = "WORTH")
plot(density(sfit2$residuals), main = "SATIS")
plot(density(afit2$residuals), main = "ANXIOUS")

# Examine the density plot. It should follow more or less a bell-shaped curve. It can be hard to determine whether or not your density plot is "normal" as it takes 
# practice. If there are any weird bumps or the curve is clearly skewed to one side or bimodal, chances are your residuals are not normal. 
# ANCOVA is quite robust to this assumption so even if your data don't look like a perfect bell curve, the assumption can still be met. 
# Let's use more information before we decide if this meets the normality assumption.

# The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess if a set of data plausibly came from some theoretical distribution such as a normal 
# or exponential. Here's an example of a Normal Q-Q plot when both sets of quantiles truly come from Normal distributions.

# Q-Q plots take your sample data, sort it in ascending order, and then plot them versus quantiles calculated from a theoretical distribution
# If the points fall pretty closely along the line, the data are normal.

#code to create a Q-Q plot
par(mfrow=c(2,2))
qqnorm(hfit2$residuals, main = "HAPPY")
qqline(hfit2$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

qqnorm(wfit$residuals, main = "WORTH")
qqline(wfit$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

qqnorm(sfit2$residuals, main = "SATIS")
qqline(sfit2$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

qqnorm(afit2$residuals, main = "ANXIOUS")
qqline(afit2$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))

# This plot does not look good. The points should fall pretty much along a straight line. With the density plot and the Q-Q plot, it doesn't seem like the data are normal. 
# Let's keep checking our assumptions before deciding on whether our data meet the assumptions.

# To test whether the error terms for each group have similar variance (homogeneity of variance, assumption 2 above), we will use a plot of the fitted.values 
# vs. the predicted values.

#code to create a fitted values vs residuals plot
par(mfrow=c(2,2))
plot(hfit2$residuals~hfit2$fitted.values, main = "HAPPY", xlab = "Fitted Values", ylab = "Residuals")
lines(lowess(hfit2$fitted.values, hfit$residuals), col="blue")

plot(wfit$residuals~wfit$fitted.values, main = "WORTH", xlab = "Fitted Values", ylab = "Residuals")
lines(lowess(wfit$fitted.values, wfit$residuals), col="blue")

plot(sfit2$residuals~sfit2$fitted.values, main = "SATIS", xlab = "Fitted Values", ylab = "Residuals")
lines(lowess(sfit2$fitted.values, sfit$residuals), col="blue")

plot(afit2$residuals~afit2$fitted.values, main = "ANXIOUS", xlab = "Fitted Values", ylab = "Residuals")
lines(lowess(afit2$fitted.values, afit$residuals), col="blue")

# For this plot, you are looking for no patterns. If you see a cone where the vertical variation on one side of the figure is smaller than the vertical 
# variation on the other side, the variation is not homogeneous and you need to transform your data. The plot in this example suggests such a problem. 
# The points on the left side cover a much smaller range of values and are more clustered than the points on the right side. 
# If your plot looks like this, your data violate the assumption of homogeneity of variance. In this case, you need to transform your data.

########### RF Model ################

# RF MODEL #                           

# Random forest is a classification algorithm utilising numerous decision trees to inform predictions. It can also be used to understand the value and significance of
# each model variable to the overall accuracy of predictions. This gives an indication of the effect of each variable on SWB measures.

# As with our ANCOVA analysis, COUNTRY will be removed as it has colinearity with GOR9D and therefore will serve little benefit to the model and reduce overall
# computational performance when running.

# Initially the model will be trained on our 75930 lines of train data. The trained prediction can then be ran against both the train and test data to evaluate how
# accurate the predictions made were. Test consists of 32541 lines.

set.seed(322) # set seed to make the results reproducible

# Convert wellbeing variables to factors as needed for RF
test2$HAPPY <- as.factor(test2$HAPPY)
train2$HAPPY <- as.factor(train2$HAPPY)
test2$WORTH <- as.factor(test2$WORTH)
train2$WORTH <- as.factor(train2$WORTH)
test2$SATIS <- as.factor(test2$SATIS)
train2$SATIS <- as.factor(train2$SATIS)
test2$ANXIOUS <- as.factor(test2$ANXIOUS)
train2$ANXIOUS <- as.factor(train2$ANXIOUS)

# Random forest algorithm features 2 key parameters which can be adjusted to tweak how the model runs and potentially improve accuracy of predictions, 'mtry' and 
# 'ntree'. mtry is the number of randomly selected predictors to consider at each split of the decision tree, ntree is the total number of trees the RF will consist of. 
# The 'tuneRF' command of the randomForest package in R can be used to find the optimal value for the first parameter, mtry. Starting with the default mtry value, 
# multiple models will be evaluated using different values of mtry looking for the overall best OOB error, or overall prediction accuracy 
# Default mtry value is dependent on the number of features on the model and is calculated as the square root of the total features rounded down, 
# in our case we are using 14 features so this would be 3.

floor(sqrt(14))

# In our test, each iteration of the test will increase or decrease the value of mtry requiring a minimal improvement of at least 0.05 prediction accuracy to continue attempts to
# improve the value. A forest of 500 prediction trees will be grown to evaluate this value.
# This will be ran independently for our 4 wellbeing models.

# The number of trees grown by default is 500. This value can be optimised to improve computational speed against potential gains of prediction accuracy.
# The optimal value is going to vary from model to model but, generally speaking, improvements are made quickly and accuracy drops off after the first hundred or so trees
# for this reason it's best to find a value that sits in a good spot for computational speed without sacrificing accuracy. 
# For these models a value of 200 was chosen but optimal value can differ depending on the data.

tHAPPY <- tuneRF(train2[, c(1:15)], train2$HAPPY,
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE,
            improve = 0.05)

# get optimal mtry value
tr = as.data.frame(tHAPPY)
t = tr[which.min(tr$OOBError),]
mtryH = t$mtry

# HAPPY model trialed at mtry = 1 (74.9% ER), mytry = 3 (75.9%) and mtry = 6 (78.6% ER) with mtry = 1 providing the best performance overall.
# this improvement is quite minimal however only providing 1% more accuracy to the default overall.

# number of trees was tested at a value of 200.

new.bag.treeH <- randomForest(HAPPY ~ AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, data = train2,
                             importance = TRUE, do.trace = TRUE, mtry = mtryH, ntree = 200)

plot(new.bag.treeH, main = "")
# A basic plot of the prediction model shows how the error rate stabilises quite quickly prior to 100 trees so there would be little to no benefit in exceeding this
# number if computational speeds suffered.

tWORTH <- tuneRF(train2[, c(1:15)], train2$WORTH,
                 stepFactor = 0.5,
                 plot = TRUE,
                 ntreeTry = 200,
                 trace = TRUE,
                 improve = 0.05)

# get optimal mtry value
tr = as.data.frame(tWORTH)
t = tr[which.min(tr$OOBError),]
mtryW = t$mtry

new.bag.treeW <- randomForest(WORTH ~ AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, data = train2,
                             importance = TRUE, do.trace = TRUE, mtry = mtryW, ntree = 200)

tSATIS <- tuneRF(train2[, c(1:15)], train2$SATIS,
                 stepFactor = 0.5,
                 plot = TRUE,
                 ntreeTry = 200,
                 trace = TRUE,
                 improve = 0.05)

# get optimal mtry value
tr = as.data.frame(tSATIS)
t = tr[which.min(tr$OOBError),]
mtryS = t$mtry

new.bag.treeS <- randomForest(SATIS ~ AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, data = train2,
                             importance = TRUE, do.trace = TRUE, mtry = mtryS, ntree = 200)

tANXIOUS <- tuneRF(train2[, c(1:15)], train2$ANXIOUS,
                 stepFactor = 0.5,
                 plot = TRUE,
                 ntreeTry = 200,
                 trace = TRUE,
                 improve = 0.05)

# get optimal mtry value
tr = as.data.frame(tANXIOUS)
t = tr[which.min(tr$OOBError),]
mtryA = t$mtry

new.bag.treeA <- randomForest(ANXIOUS ~ AGE+SEX+DISEA+STUCUR+MARDY6+TEN1+GOR9D+ETHUKEUL+RELIG11+HIQUL15D+ILODEFR+EMPMON+SUMHRS+BENFTS, data = train2,
                             importance = TRUE, do.trace = TRUE, mtry = mtryA, ntree = 200)

# combine mtry tuning matrices and rename rows and columns
tALL <- (cbind(tHAPPY[,2], tWORTH[,2], tSATIS[,2], tANXIOUS[,2]))
rownames(tALL) <- c("mtry = 1", "mtry = 3", "mtry = 6")
colnames(tALL) <- c("HAPPY", "WORTH", "SATIS", "ANXIOUS")
tALL <- tALL * 100
tALL <- round(tALL, digits = 2)

# create table showing results of mtry tuning for all models. displays error rate as %
kable(tALL, booktabs = T)%>%
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# Table shows that after optimisation an mtry value of 1 provided the lowest overall error rate for all SWB RF models. This also shows that the model isn't particularly good at
# predicting values based on the variables available with the best model, SATIS, accurately predicting 32.5% of the time.
# Performance on final models and test data may differ somewhat from these figures.

# what is this actually showing, does it have relevance?

########## Evaluation measures for training data

save(new.bag.treeA, new.bag.treeS, new.bag.treeW, new.bag.treeH, file = "rfmodels.RData")

# make predictions and confusion matrix for train2
bag.predH <- predict(new.bag.treeH, train2)
trainconfH <- confusionMatrix(bag.predH, train2$HAPPY)

bag.predW <- predict(new.bag.treeW, train2)
trainconfW <- confusionMatrix(bag.predW, train2$WORTH)

bag.predS <- predict(new.bag.treeS, train2)
trainconfS <- confusionMatrix(bag.predS, train2$SATIS)

bag.predA <- predict(new.bag.treeA, train2)
trainconfA <- confusionMatrix(bag.predA, train2$ANXIOUS)

########## Evaluation measures for test data

# make predictions and confusion matrix for test
test.bag.predH <- predict(new.bag.treeH, test2)
testconfH <- confusionMatrix(test.bag.predH, test2$HAPPY)

test.bag.predW <- predict(new.bag.treeW, test2)
testconfW <- confusionMatrix(test.bag.predW, test2$WORTH)

test.bag.predS <- predict(new.bag.treeS, test2)
testconfS <- confusionMatrix(test.bag.predS, test2$SATIS)

test.bag.predA <- predict(new.bag.treeA, test2)
testconfA <- confusionMatrix(test.bag.predA, test2$ANXIOUS)

# put errors of these and train results into a table for display and comparison

rfALL <- (cbind(trainconfH$overall[1], trainconfW$overall[1], trainconfS$overall[1], trainconfA$overall[1]))
rfALL <- (rbind(rfALL, c(testconfH$overall[1], testconfW$overall[1], testconfS$overall[1], testconfA$overall[1])))
rownames(rfALL) <- c("Train", "Test")
colnames(rfALL) <- c("HAPPY", "WORTH", "SATIS", "ANXIOUS")
rfALL <- rfALL * 100
rfALL <- round(rfALL, digits = 2)

# create table showing results of RF classification accuracy
kable(rfALL, booktabs = T)%>%
  row_spec(0,bold=TRUE) %>% 
  kable_styling(latex_options = 'hold_position', full_width = F)

# We can see from this table that overall accuracy for predictions with the model is not great as was suggested during the mtry tuning. Predictions for SATIS were the most accurate aggain
# at 32.5%. HAPPY, ANXIOUS and WORTH models all performed slightly better against the test data than the train data.
# Looking closer at the findings we can see the model does well in predicting certain values, especially those which there are more data points for, which stands to
# reason.
# Initial exploration of the data via boxplots and tables showed the distribution of each wellbeing variable, it was clear to see that the vast majority of figures sat around
# the same areas, these areas were those better predicted by the model with the outliers being much harder to predict from the variables available to the model.
# This suggests that extreme values on either side of the scale do not appear to follow set patterns or have clear similarities across prediction variables for respondents
# in the dataset. This also suggests that no one variable, or combination of variables, has the ability to fully predict a SWB score and that relationships between them are more subtle
# and complex than a linear effect.

########## Visualisation of RF classifier
plot(new.bag.treeH)
plot(new.bag.treeW)
plot(new.bag.treeS)
plot(new.bag.treeA)
# NOT NEEDED

# Using the obtained random forest, comment on the relative importance of the variables for prediction accuracy.

# Random forests also allows for a view on the overall importance of each variable to the overall accuracy of predictions made. This gives an indication of how much of an
# effect each variable has on the continuous dependent variable, in this case the SWB measure being predicted. 

# -MeanDecreaseAccuracy: gives a rough estimate of the loss in prediction performance when that particular variable is omitted from the training set. 
# Caveat: if two variables are somewhat redundant, then omitting one of them may not lead to massive gains in prediction performance, 
# but would make the second variable more important.

# -MeanDecreaseGini: GINI is a measure of node impurity. Think of it like this, if you use this feature to split the data, how pure will the nodes be? 
# Highest purity means that each node contains only elements of a single class. Assessing the decrease in GINI when that feature is omitted leads to an 
# understanding of how important that feature is to split the data correctly.

varImpPlot(new.bag.treeH, main = "", bg = 'Black') # Plot of variable importance - HAPPY

# The first half of the plot displays the overall mean accuracy loss should that variable be omitted from the model. This provides an indication of how important the variable is to an 
# accurate prediction of the SWB measure, therefore a high value of decrease suggests a strong relationship between the independent and dependent variable.

# Plot for the HAPPY RF model shows several variables around a similar level of effect on accuracy namely DISEA, BENFTS, HIQUL15D and AGE. Previous plots for DISEA and AGE during data
# exploration suggested a relationship with HAPPY which is backed up here.
# Removal of the DISEA variable would lead to highest drop in accuracy and GOR9D the lowest which suggests a weaker relationship between location and happiness when compared to the other
# available predictors.

# Second half of the plot displays overall mean GINI drop if the variable is omitted from the model. GINI provides a measure of node impurity, this means that if you were to use one feature 
# to split the data, how pure would the resulting nodes be? The higher the node purity, the more common the values shared across variables included in the model.

# Removing AGE would have the biggest effect on node impurity indicating that age helps to group common data together. This is reflective of information previously explored such as 
# likelihood of home ownership by age. STUCUR, the indicator of student status, comes out the lowest - this isn't surprising as previous distribution findings suggests that students account 
# for a small overall portion of the data.

varImpPlot(new.bag.treeW, main = "", bg = 'Black') # Plot of variable importance - WORTH

# WORTH plot indicates a correlation between home ownership status, TEN1, and feelings of worth, this is not reflective of previous box plot which demonstrated little variation of values across TEN1
# category. As in the HAPPY plot, removal of any of TEN1, AGE, DISEA, BENFTS, SUMHRS or HIQUL15D leads to a mean decrease of 12 or more accuracy.
# AGE is again provides the biggest boost to mean GINI with some variation across the other variables when compared to the plot for HAPPY.

varImpPlot(new.bag.treeS, main = "", bg = 'Black') # Plot of variable importance - SATIS

# MARDY6 provides the biggest contribution to accurate predictions of satisfaction. Again we see AGE, DISEA, BENFTS, SUMHRS, HIQUL15D and TEN1 at 12 or above mean accuracy decrease, EMPMON
# also features at this level for SATIS.
# Again, inclusion of AGE provides the biggest boost to node purity this time followed by DISEA.

varImpPlot(new.bag.treeA, main = "", bg = 'Black') # Plot of variable importance - ANXIOUS

# For ANXIOUS, DISEA comes out as the highest contributor to accurate predictions with all other variables falling below 12. This reflects previous analysis suggestive of a strong
# relationship between DISEA and ANXIOUS with respondents who are "Equality Act Disabled" averaging 2 points higher in the measure than the other 2 options.
# Again, inclusion of AGE provides the biggest boost to node purity this time followed by HIQUL15D.

# Overall findings relating to prediction accuracy closely correspond with the relationships identified in earlier visualisations with the exception of WORTH which, from the boxplot
# alone, was not identified. ANCOVA results however did indicate an effect on WORTH for majority of potential TEN1 values.
# AGE was clearly the biggest contributor to node purity which is not suprising given how some questions do not relate to the majority of persons above a certain age. It does also potentially
# highlight a relationship between age and other variables for example TEN1 as explored previously.
