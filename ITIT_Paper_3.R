######### Figures for ITIT third paper, using current cleaned data #################

itit_df <- read_csv("~/Downloads/itit_df.csv") 

itit_df <- itit_df %>%
  rename(survey_date = finished)

itit_df2 <- itit_df %>%
  mutate(across(where(is.character), as.factor)) %>%  # Convert character columns to factors
  filter(baseline_date >= "2022-04-01") %>%     
  filter(travel_date <= survey_date) %>%              # Ensure travel_date is before survey_date
  filter(age > 17) %>%                               # Filter out minors (age <= 17)
  filter(travel_duration > 1)                        # Filter out trips of 1 day or less

# flextable defaults
set_flextable_defaults(
  font.family = "Arial Narrow",
  font.size = 10,
  font.color = "black",
  table.layout = "autofit",
  digits = 1,
  decimal.mark =",",
  big.mark = " ", # for 100 000 etc.
  theme_fun = "theme_booktabs")


############################################################################################
# table 1

#makes variable counting the number of days of the trip
itit_df2 <- itit_df2 %>%
  group_by(trip_id) %>%
  mutate(survey_day = row_number()) %>%
  ungroup() 

# Replace travel_duration with the count of rows per trip_id
itit_df2 <- itit_df2 %>%
  group_by(trip_id) %>%
  mutate(travel_duration = n()) %>%  
  ungroup()

### check that making survey day was successful 
check_inconsistency <- itit_df2 %>%
  filter(survey_day > travel_duration)



itit_df2 <- itit_df2 %>%
  mutate(survey_date = case_when(
    is.na(gastro_any) ~ NA_Date_,  # Set to NA for dates
    TRUE ~ survey_date         # Keep existing value
  ))

itit_df2 %>%
  select(user_id,trip_id,age,gender,continent_clean,travel_purpose,smoking_status,health_chronic,survey_day,travel_date,survey_date) %>% 
  group_by(trip_id) %>% 
  mutate(na_sum=sum(is.na(survey_date)))  %>% 
  slice_tail(n=1) %>% #slice the last day of travel for each trip (duration of travel)
  arrange(travel_date) %>% 
  mutate(response_rate_all=ifelse(is.na(survey_day)==T,0,1-(na_sum/survey_day))) %>% # Calculate response rate for all travellers
  mutate(response_rate_active=ifelse(is.na(survey_day)==T,NA,1-(na_sum/survey_day))) %>% # Calculate response rate for active travellers
  select(-c(survey_date,na_sum)) %>% 
  ungroup() %>% 
  group_by(user_id) %>% 
  mutate(response_rate_all=ifelse(response_rate_all==0,0,max(response_rate_all))) %>% 
  mutate(response_rate_active=ifelse(response_rate_active==0,0,max(response_rate_active))) %>% 
  mutate(trip_period = as.numeric(n())) %>% # Number of trips during study period
  mutate(trip_period=ifelse(is.na(survey_day)==T,0,trip_period)) %>% # If survey_day is NA, set number of trips to 0
  slice_max(survey_day) %>% #maximum day of travel for each user
  ungroup() %>%
  select(-c(user_id,trip_id,travel_date)) %>%
  mutate(travel_purpose=fct_lump_min(travel_purpose, 10, other_level = "Other")) %>% # Lump travel purposes less than 10
  mutate(travel_purpose=fct_infreq(travel_purpose)) %>% # Order travel purposes by frequency
  mutate(trip_period=as.factor(case_when(trip_period==0~"No active participation", # Create a new variable for number of trips
                                         trip_period==1~"Questionnaires filled for 1 trip",
                                         trip_period>1~"Questionnaires filled for 2 or more trips"))) %>%
  mutate(smoking_status=fct_collapse(smoking_status,"Current smoker"=c("Daily","Monthly","Weekly"),
                                     "Never smoked"=c("Not smoking"))) %>%
  mutate(health_chronic=fct_collapse(health_chronic,"Yes"=c("Diabetes","Heart diseases","High blood pressure","Immunosuppression","Multiple"),
                                     "No"=c("None"))) %>% 
  #mutate(smoking_status)
  tbl_summary(by=travel_purpose,
              statistic=list(all_continuous() ~ c("{mean} ({sd})","{min}-{max}")),
              type=list(all_continuous()~ 'continuous2'),
              label = list(gender ~ "Gender",
                           age  ~ "Age [years]",
                           smoking_status~ "Smoking status",
                           health_chronic ~ "Comorbidities",
                           survey_day~ "Duration of travel [days]",
                           continent_clean~ "United Nations continent name",
                           trip_period~ "Number of trips during study period",
                           response_rate_all~ "Overall response rate",
                           response_rate_active~ "Active travellers response rate"))  %>% 
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>% 
  remove_row_type(variables=c("response_rate_active"),type = "missing") %>% 
  #modify_spanning_header(c("stat_1", "stat_2","stat_3","stat_4") ~ "**Travel groups**") %>% 
  modify_footnote(c("stat_4") ~ "Includes specific groups of travelers who do not fit into the previously defined categories. These travelers attended mass gathering events such as the Hajj, Olympics, or World Cup, or were involved in research, education, humanitarian work, or other activities") %>%
  modify_table_styling(columns = label,
                       rows = label == "Overall response rate",
                       footnote = "Includes participants who completed the baseline questionnaire but did not complete any subsequent surveys.") %>% 
  modify_table_styling(columns = label,
                       rows = label == "Active travellers response rate",
                       footnote = "Includes participants who completed at least one survey.") %>% 
  modify_table_body(~ .x %>%
                      dplyr::slice(-24)) %>% 
  as_flex_table() %>% 
  autofit()  %>% 
  save_as_docx(path=here( "table_1.docx"),
               pr_section = prop_section(page_size = page_size(orient = "landscape",width = 20, height = 18),
                                         type = "continuous",
                                         page_margins = page_mar()))


