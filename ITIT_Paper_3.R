######### Figures for ITIT third paper, using current cleaned data #################

library(tidyverse)
library(here)
library(lubridate)
library(gtsummary)
library(flextable)
library(officer)
library(rnaturalearth) #world map
library(ggspatial) #annotation_arrow map
library(tidymodels)
library(mice)
library(miceadds)
library(lme4)
library(broom.mixed)
library(janitor)
library(XML)
library(sjmisc)
library(sjPlot)
library(buildmer)
library(parallel)

# comment

itit_df <- read_csv("~/Downloads/itit_df_clean 2.csv") 

itit_df <- read_csv(here("itit_df_clean 2.csv"), col_types = cols(
  .default = col_guess(), 
  `follow-up_1_diagnosis` = col_character(),
  `follow-up_2_diagnosis` = col_character(),      
  `follow-up_1_diagnosis_date` = col_character(),
  `follow-up_2_diagnosis_date` = col_character(),
  `snow_1h` = col_character()
))



itit_df <- itit_df %>%
  rename(survey_date = finished)

itit_df$travel_date <- as.Date(itit_df$travel_date, format = "%d.%m.%y")  
itit_df$survey_date <- as.Date(itit_df$survey_date) 
itit_df$baseline_date <- as.Date(itit_df$baseline_date) 

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


## Checked the number of survey filled by Ukrainians -- very low response rate, so removed from analysis
# ITIT_ukr <- itit_df2 %>% 
#   filter((travel_purpose)=="Refugee/Ukraine")
# ITIT_ukr <- ITIT_ukr %>% 
#   filter(is.na(gastro_any)==FALSE)


##Remove Ukrainians 
itit_df2 <- itit_df2 %>% 
  filter(travel_purpose != "Refugee/Ukraine")


itit_df2 %>%
  select(user_id, trip_id, age, gender, continent_clean, travel_purpose, smoking_status, health_chronic, survey_day, travel_date, survey_date) %>% 
  group_by(trip_id) %>% 
  mutate(na_sum = sum(is.na(survey_date)))  %>% 
  slice_tail(n = 1) %>% # Slice the last day of travel for each trip (duration of travel)
  arrange(travel_date) %>% 
  mutate(response_rate_all = ifelse(is.na(survey_day) == T, 0, 1 - (na_sum / survey_day))) %>% # Calculate response rate for all travellers
  mutate(response_rate_active = ifelse(is.na(survey_day) == T, NA, 1 - (na_sum / survey_day))) %>% # Calculate response rate for active travellers
  select(-c(survey_date, na_sum)) %>% 
  ungroup() %>% 
  group_by(user_id) %>%
  # Calculate the overall response rate for each user
  mutate(response_rate_all = max(response_rate_all, na.rm = TRUE)) %>%
  mutate(response_rate_active = max(response_rate_active, na.rm = TRUE)) %>%
  mutate(trip_period = as.numeric(n())) %>%  # Number of trips during study period
  mutate(trip_period = ifelse(is.na(survey_day) == T, 0, trip_period)) %>%  # If survey_day is NA, set number of trips to 0
  slice_max(survey_day) %>%  # Maximum day of travel for each user
  ungroup() %>%
  select(-c(user_id, trip_id, travel_date)) %>%
  mutate(travel_purpose = fct_lump_min(travel_purpose, 10, other_level = "Other")) %>% # Lump travel purposes less than 10
  mutate(travel_purpose = fct_infreq(travel_purpose)) %>%  # Order travel purposes by frequency
  mutate(trip_period = as.factor(case_when(
    trip_period == 0 ~ "No active participation", 
    trip_period == 1 ~ "Questionnaires filled for 1 trip", 
    trip_period > 1 ~ "Questionnaires filled for 2 or more trips"
  ))) %>%
  mutate(smoking_status = fct_collapse(smoking_status, "Current smoker" = c("Daily", "Monthly", "Weekly"), "Never smoked" = c("Not smoking"))) %>%
  mutate(health_chronic = fct_collapse(health_chronic, "Yes" = c("Diabetes", "Heart diseases", "High blood pressure", "Immunosuppression", "Multiple"), "No" = c("None"))) %>% 
  tbl_summary(
    by = travel_purpose,
    statistic = list(all_continuous() ~ c("{mean} ({sd})", "{min}-{max}")),
    type = list(all_continuous() ~ 'continuous2'),
    label = list(
      gender ~ "Gender",
      age ~ "Age [years]",
      smoking_status ~ "Smoking status",
      health_chronic ~ "Comorbidities",
      survey_day ~ "Duration of travel [days]",
      continent_clean ~ "United Nations continent name",
      trip_period ~ "Number of trips during study period",
      response_rate_all ~ "Overall response rate",
      response_rate_active ~ "Active travellers response rate"
    )
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  italicize_levels() %>%
  remove_row_type(variables = c("response_rate_active"), type = "missing") %>% 
  modify_footnote(c("stat_4") ~ "Includes specific groups of travelers who do not fit into the previously defined categories. These travelers attended mass gathering events such as the Hajj, Olympics, or World Cup, or were involved in research, education, humanitarian work, or other activities") %>%
  modify_table_styling(columns = label, rows = label == "Overall response rate", footnote = "Includes participants who completed the baseline questionnaire but did not complete any subsequent surveys.") %>% 
  modify_table_styling(columns = label, rows = label == "Active travellers response rate", footnote = "Includes participants who completed at least one survey.") %>% 
  modify_table_body(~ .x %>% dplyr::slice(-24)) %>% 
  as_flex_table() %>% 
  autofit() %>% 
  save_as_docx(path = here("table_1V2.docx"), pr_section = prop_section(page_size = page_size(orient = "landscape", width = 20, height = 18), type = "continuous", page_margins = page_mar()))




#######################################################################################
####Makes stacked par plots looking at continent of travel and reason for travel, both split by male and female, and not

data_summary <- data_ready %>%
  mutate(travel_purpose = case_when(
    travel_purpose %in% c("Work", "Education", "Other") ~ "Other",
    TRUE ~ travel_purpose  # Keep other categories as they are
  )) %>%
  group_by(continent_clean, travel_purpose, gender) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(continent_clean = fct_reorder(continent_clean, count, .fun = sum, .desc = FALSE),
         travel_purpose = fct_reorder(travel_purpose, count, .fun = sum, .desc = TRUE))





# Create the stacked bar plot split by gender
ggplot(data_summary, aes(x = continent_clean, y = count, fill = travel_purpose)) +
  geom_bar(stat = "identity") +  # Create a stacked bar plot
  facet_wrap(~ gender) +         # Facet by gender
  scale_fill_viridis_d(option = "inferno", direction = -1) +       # Optionally set color scale
  theme_minimal() +       # Use a minimal theme
  coord_flip() + #makes it horizontal
  labs(
    title = "Reason for Travel by Continent and Gender",
    x = "Continent",
    y = "Count of Surveys",
    fill = "Reason for Travel",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )


# Create the stacked bar plot overall

ggplot(data_summary, aes(x = continent_clean, y = count, fill = travel_purpose)) +
  geom_bar(stat = "identity") +  # Create a stacked bar plot
  scale_fill_viridis_d(option = "inferno", direction = -1) +       # Use a discrete color scale
  theme_minimal() +
  coord_flip() + 
  labs(
    title = "Reason for Travel by Continent (All Genders)",
    x = "Continent",
    y = "Count of Surveys",
    fill = "Reason for Travel",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


#### Calculate percentages for puttign on the plot
data_summary2 <- data_summary %>%
  group_by(continent_clean, travel_purpose) %>%
  summarize(count = sum(count), .groups = "drop") %>%  # Sum counts for all genders
  group_by(continent_clean) %>%
  mutate(
    percent_within_continent = count / sum(count) * 100  # Percentage of travel_purpose within the continent
  ) %>%
  ungroup() %>%
  mutate(
    percent_overall_continent = ave(count, continent_clean, FUN = sum) / sum(count) * 100  # Overall percentage by continent
  )

# Create the plot
ggplot(data_summary2, aes(x = continent_clean, y = count, fill = travel_purpose)) +
  geom_bar(stat = "identity") +  # Create a stacked bar plot
  # Add text for within-continent percentages
  geom_text(
    aes(
      label = paste0(sprintf("%.1f", percent_within_continent), "%"),
      y = count / 2  # Place in the center of the bar segments
    ),
    position = position_stack(vjust = 0.5),  # Stack positioning for the bar segments
    size = 3,  # Adjust text size
    color = "white"
  ) +
  # Add text for overall percentages by continent
  geom_text(
    aes(
      label = paste0("Total: ", sprintf("%.1f", percent_overall_continent), "%"),
      y = sum(count)  # Place at the end of the stacked bars
    ),
    size = 4,  # Slightly larger size for overall percentages
    color = "black",
    hjust = -0.2  # Adjust horizontal alignment to appear outside the bars
  ) +
  scale_fill_viridis_d(option = "inferno", direction = -1) +  # Use a discrete color scale
  theme_minimal() +
  coord_flip() + 
  labs(
    title = "Reason for Travel by Continent (All Genders)",
    x = "Continent",
    y = "Count of Surveys",
    fill = "Reason for Travel",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

##########################################################################################
### Map of symptoms by country, both absolute number, and percentage 



## Yes and No for body other are inverted.. fix 
itit_df2$body_other <- ifelse(itit_df2$body_other == "Yes", "No", 
                              ifelse(itit_df2$body_other == "No", "Yes", itit_df2$body_other))

## only keeps real daily surveys that were filled out 
itit_df3 <- itit_df2 %>% 
  filter(is.na(gastro_any)==FALSE)


itit_df3 <- itit_df2 %>% 
  filter(is.na(gastro_any)==FALSE) %>% 
  select(trip_id,travel_purpose,continent_clean,country_clean, gender,
         nausea:constipation,
         cough:out_of_breath_running,
         rash:itchy_red_eyes,
         fever:body_other,
         pain_joint:location_swelling)  %>% 
  mutate(across(c(nausea:location_swelling), ~case_when(. %in% c("None","none", "No") ~ 0,
                                                        is.na(.) == TRUE ~ NA,
                                                        TRUE ~ 1))) %>% 
  rowwise() %>% 
  mutate(gastrointestinal=as.numeric(if_any(nausea:constipation, ~.x != 0)),
         respiratory=as.numeric(if_any(cough:out_of_breath_running, ~.x != 0)),
         dermatologic=as.numeric(if_any(rash:itchy_red_eyes, ~.x != 0)),
         general=as.numeric(if_any(fever:location_swelling, ~.x != 0)),
         overall=as.numeric(if_any(c(nausea:constipation,cough:out_of_breath_running,rash:itchy_red_eyes,fever:location_swelling), ~.x != 0)))%>% 
  ungroup() %>% 
  relocate(gastrointestinal,.before=nausea) %>% 
  relocate(respiratory,.before=cough) %>% 
  relocate(dermatologic,.before=rash) %>% 
  relocate(general,.before=fever) %>% 
  relocate(overall,.before=gastrointestinal) %>% 
  drop_na() %>% 
  group_by(trip_id) %>% 
  mutate(survey_day=row_number()) %>% 
  mutate(across(c(overall:location_swelling), ~sum(., na.rm = TRUE))) %>% 
  slice_max(survey_day) %>% 
  ungroup() -> data_ready




### counts symptoms by country, both total and percentage of travellers 
symptom_by_country <- data_ready %>%
  group_by(country_clean) %>% 
  summarize(
    gastrointestinal_percentage = mean(gastrointestinal >0, na.rm = TRUE) * 100 ,
    respiratory_percentage = mean(respiratory > 0, na.rm = TRUE) * 100 ,
    skin_percentage = mean(dermatologic > 0, na.rm = TRUE) * 100 ,
    general_percentage = mean(general > 0, na.rm = TRUE) * 100, 
    gastrointestinal_count = sum(gastrointestinal > 0, na.rm = TRUE),  # Count non-zero gastrointestinal values
    respiratory_count = sum(respiratory > 0, na.rm = TRUE),            
    skin_count = sum(dermatologic > 0, na.rm = TRUE),                   
    general_count = sum(general > 0, na.rm = TRUE)     
  )


world <- ne_countries(scale = "medium", returnclass = "sf")

# Ensure matching column names
symptom_by_country <- symptom_by_country %>% rename(name = country_clean)


### check if any of the names did not match the world dataset
unmatched_countries <- symptom_by_country %>%
  filter(!name %in% world$name)

print(unmatched_countries)


## fixes country names 
symptom_by_country <- symptom_by_country %>%
  mutate(name = case_when(
    name == "Bolivia, Plurinational State of" ~ "Bolivia",
    name == "Dominican Republic" ~ "Dominican Rep.",
    name == "Korea, Republic of" ~ "South Korea",
    name == "Lao People's Democratic Republic" ~ "Laos",
    name == "Russian Federation" ~ "Russia",
    name == "Tanzania, United Republic of" ~ "Tanzania",
    name == "Türkiye" ~ "Turkey",
    name == "United States" ~ "United States of America",
    name == "Viet Nam" ~ "Vietnam",
    TRUE ~ name  # Leave other names unchanged
  ))

# Merge data with the world map
world_data <- left_join(world, symptom_by_country, by = "name")



# Reshape the data to long format
world_data_long <- world_data %>%
  pivot_longer(
    cols = c(gastrointestinal_percentage, respiratory_percentage, skin_percentage, general_percentage),
    names_to = "group",
    values_to = "percentage"
  )


# Create the map of symptom percentage by country 
ggplot(data = world_data_long) +
  geom_sf(aes(fill = percentage), color = "gray50") +
  scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "gray90", name = "Percentage (%)") +
  theme_minimal() +
  labs(
    title = "Health Issue Percentages by Country",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "bottom"
  ) +
  facet_wrap(~ group, ncol = 2, labeller = labeller(
    type = c(
      gastrointestinal_percentage = "Gastrointestinal",
      respiratory_percentage = "Respiratory",
      skin_percentage = "Skin",
      general_percentage = "General"
    )
  ))

# Reshape the data to long format
world_data_long2 <- world_data %>%
  pivot_longer(
    cols = c(gastrointestinal_count, respiratory_count, skin_count, general_count),
    names_to = "group",
    values_to = "count"
  )


## nmakes map of absolute number of symptoms per country
ggplot(data = world_data_long2) +
  geom_sf(aes(fill = count), color = "gray50") +
  scale_fill_viridis_c(option = "rocket", direction = -1, na.value = "gray90", name = "Number of positive surveys") +
  theme_minimal() +
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90)) +
  labs(
    title = "Symptoms by Country",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "bottom"
  ) +
  facet_wrap(~ group, ncol = 2, labeller = labeller(
    type = c(
      gastrointestinal_percentage = "Gastrointestinal",
      respiratory_percentage = "Respiratory",
      skin_percentage = "Skin",
      general_percentage = "General"
    )
  ))







#####################################################################################
### Makes treemap of diagnoses, first takign diagnosis column and cleaning it, then counting the number of users with the diagnosis, and the location and plottting it

Followup <- itit_df2[, c(1, 2, 10, 11, 151, 152)]

Followup <- Followup %>%
  mutate(
    ##### Create a combined diagnosis column using a range of consecutive column indices (49:59)
    combined_diag = do.call(paste, c(Followup[, 5:6], sep = ", ")),
    
    # Remove the prefix from the combined diagnosis column
    diagnosis = gsub("NA", "", combined_diag)
  )

Followup2 <- Followup  %>% filter(diagnosis != ", ", !is.na(diagnosis), str_trim(diagnosis) != "")  %>%
  group_by(diagnosis, trip_id) %>%
  # Summarize the number of occurrences
  summarize(n = n(), .groups = "drop")



Followup_grouped <- Followup %>%
  mutate(
    diagnosis = gsub("NA,? ?", "", diagnosis),  # Remove NA values
    diagnosis = gsub(", $", "", diagnosis),   
    diagnosis = gsub("^, ?", "", diagnosis)   # Remove trailing commas
  ) %>%
  # Remove rows with empty diagnosis
  filter(diagnosis != "", !is.na(diagnosis), str_trim(diagnosis) != "") %>%
  # Expand rows with multiple diagnoses separated by commas
  separate_rows(diagnosis, sep = ",") %>%
  mutate(diagnosis = trimws(diagnosis)) %>%   # Trim whitespace around diagnosis
  # Group by diagnosis, trip_id, and country_clean
  group_by(diagnosis, trip_id, continent_clean)  %>%
  # Summarize the number of occurrences
  summarize(n = n(), .groups = "drop")


diagnosis_counts <- Followup_grouped %>%
  count(diagnosis, continent_clean)  # Creates a column `n` with counts of each unique `diagnosis`



library(sunburstR)


hierarchical_data <- diagnosis_counts %>%
  mutate(path = paste(continent_clean, diagnosis, sep = "-")) %>%  # Combine levels into a hierarchy
  select(path, n)  # Keep only the hierarchical path and the values

# Create the sunburst plot
sunburst(hierarchical_data, count = TRUE )



#############Creates a circular bar plot divided by continents 


# Set a number of 'empty bars' to add at the end of each group
empty_bar <- 0  # Adjust as needed
to_add <- data.frame(
  diagnosis = rep(NA, empty_bar * length(unique(diagnosis_counts$continent_clean))),
  continent_clean = rep(unique(diagnosis_counts$continent_clean), each = empty_bar),
  n = NA
)

# Append empty rows and arrange by continent
data <- rbind(diagnosis_counts, to_add) %>% arrange(continent_clean)

# Create an ID column
data$id <- seq(1, nrow(data))

# Get label positions and angles
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# Make the plot
p <- ggplot(data, aes(x = as.factor(id), y = n, fill = continent_clean)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  ylim(-max(data$n, na.rm = TRUE) * 1.2, max(data$n, na.rm = TRUE) * 1.2) +  # Dynamic y-axis
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  coord_polar() +
  geom_text(data = label_data, aes(x = id, y = n + 0.5, label = diagnosis, hjust = hjust),
            color = "black", fontface = "bold", alpha = 0.7, size = 3, angle = label_data$angle, inherit.aes = FALSE)

# Display the plot
p

###################################################################
#Plot the treemap 

library(treemap) 
treemap(diagnosis_counts,
        index=c("continent_clean","diagnosis"),
        vSize="n",
        type="index"
) 


treemap(
  diagnosis_counts,
  index = c("continent_clean", "diagnosis"),  # Group by continent and diagnosis
  vSize = "n",                                # Size each box based on `n`
  vColor = "continent_clean",                 # Color boxes by continent
  type = "categorical",                       # Use categorical coloring
  fontsize.labels = c(0, 10),                 # Set group labels to 0 to hide them; diagnosis labels to 10
  fontcolor.labels = "white"                # Set label color to white
  # Ensure labels are drawn
)




ggplot(diagnosis_counts, aes(area = n, fill = continent_clean,
                             label = paste(diagnosis, n, sep = "\n"))) +
  geom_treemap(aes(group = continent_clean)) +  # Group diagnoses by continent
  geom_treemap_text(colour = "white", place = "centre", size = 15) +
  theme_minimal() +  # Cleaner theme
  theme(
    legend.position = "right",  # Adjust legend position
    legend.title = element_text(size = 12),  # Customize legend title size
    legend.text = element_text(size = 10)    # Customize legend text size
  ) +
  labs(
    fill = "Continent"  # Customize the legend title
  )


##########################################################################################
####Makes stacked par plots looking at continent of travel and reason for travel, both split by male and female, and not

data_summary <- data_ready %>%
  mutate(travel_purpose = case_when(
    travel_purpose %in% c("Work", "Education", "Other") ~ "Other",
    TRUE ~ travel_purpose  # Keep other categories as they are
  )) %>%
  group_by(continent_clean, travel_purpose, gender) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(continent_clean = fct_reorder(continent_clean, count, .fun = sum, .desc = FALSE),
         travel_purpose = fct_reorder(travel_purpose, count, .fun = sum, .desc = TRUE))





# Create the stacked bar plot split by gender
ggplot(data_summary, aes(x = continent_clean, y = count, fill = travel_purpose)) +
  geom_bar(stat = "identity") +  # Create a stacked bar plot
  facet_wrap(~ gender) +         # Facet by gender
  scale_fill_viridis_d(option = "inferno", direction = -1) +       # Optionally set color scale
  theme_minimal() +       # Use a minimal theme
  coord_flip() + #makes it horizontal
  labs(
    title = "Reason for Travel by Continent and Gender",
    x = "Continent",
    y = "Count of Surveys",
    fill = "Reason for Travel",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

# Create the stacked bar plot overall
ggplot(data_summary, aes(x = continent_clean, y = count, fill = travel_purpose)) +
  geom_bar(stat = "identity") +  # Create a stacked bar plot
  scale_fill_viridis_d(option = "inferno", direction = -1) +       # Use a discrete color scale
  theme_minimal() +   # Use a minimal theme
  labs(
    title = "Reason for Travel by Continent (All Genders)",
    x = "Continent",
    y = "Count of Surveys",
    fill = "Reason for Travel",
    caption = "Data source: Your Dataset"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


#################################################################
### CART trees etc.
library(rpart)
library(rpart.plot)
library(caret)


itit_df2  %>% 
  filter(is.na(survey_day)==FALSE) %>% 
  select(trip_id,survey_day,continent_clean,gender,travel_duration, travel_purpose, age, health_chronic, smoking_status, trip_number,
         nausea:constipation,
         cough:out_of_breath_running,
         rash:itchy_red_eyes,
         fever:body_other,
         pain_joint:location_swelling) %>%
  mutate(across(c(nausea:location_swelling), ~case_when(. %in% c("None","none", "No") ~ 0,
                                                        is.na(.) == TRUE ~ NA,
                                                        TRUE ~ 1))) %>% 
  rowwise() %>% 
  mutate(gastrointestinal=as.numeric(if_any(nausea:constipation, ~.x != 0)),
         respiratory=as.numeric(if_any(cough:out_of_breath_running, ~.x != 0)),
         dermatologic=as.numeric(if_any(rash:itchy_red_eyes, ~.x != 0)),
         general=as.numeric(if_any(fever:location_swelling, ~.x != 0)),
         overall=as.numeric(if_any(c(nausea:constipation,cough:out_of_breath_running,rash:itchy_red_eyes,fever:location_swelling), ~.x != 0))) %>% 
  ungroup() %>% 
  relocate(gastrointestinal,.before=nausea) %>% 
  relocate(respiratory,.before=cough) %>% 
  relocate(dermatologic,.before=rash) %>% 
  relocate(general,.before=fever) %>% 
  relocate(overall,.before=gastrointestinal) %>% 
  drop_na() %>% 
  group_by(trip_id) %>% 
  mutate(survey_day=row_number()) %>% 
  mutate(across(c(overall:location_swelling), ~sum(., na.rm = TRUE))) %>% 
  slice_max(survey_day) %>% 
  ungroup() -> data_ready




str(data_ready)


data_ready <- na.omit(data_ready)

data_ready <- data_ready %>%
  mutate(symptom_yn = factor(
    ifelse(overall >= 1, "Yes", "No"),  # Assign "Yes" or "No"
    levels = c("No", "Yes")  # Explicitly set the levels
  ))


cart_model <- rpart(
  symptom_yn ~  gender + continent_clean + travel_purpose + travel_duration + age + health_chronic + smoking_status + trip_number,
  data = data_ready,
  method = "class"  
)

# Visualize the tree
rpart.plot(cart_model)



# Predict on new data
predicted <- predict(cart_model, newdata = data_ready, type = "class")

# Add predictions back to the dataset
data_ready$predicted_symptom_category <- predicted

confusionMatrix(
  data = predicted,
  reference = data_ready$symptom_yn
)




train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Set a grid of hyperparameters to search
tune_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.01))

# Train the CART model with hyperparameter tuning
tuned_cart <- train(
  symptom_yn ~  gender + continent_clean + travel_purpose + travel_duration + age + health_chronic + smoking_status + trip_number,
  data = data_ready,
  method = "rpart",
  trControl = train_control,
  tuneGrid = tune_grid
)

# Best model
print(tuned_cart$bestTune)

# Visualize the best tree
library(rpart.plot)
rpart.plot(tuned_cart$finalModel)




train_control <- trainControl(method = "cv", number = 10)

# Train the model with CV
cart_cv <- train(
  symptom_yn ~  gender + continent_clean + travel_purpose + travel_duration + age + health_chronic + smoking_status + trip_number,
  data = data_ready,
  method = "rpart",
  trControl = train_control
)

print(cart_cv)



rpart.plot(tuned_cart$finalModel, type = 3, extra = 102, fallen.leaves = TRUE)


# Variable importance
varImp(tuned_cart)

## travel_duration, age, and continet_Europe, then Asia  contributed the most






#### ROC curve
library(pROC)

# Get predicted probabilities for the positive class (e.g., "Yes")
pred_probs <- predict(tuned_cart, newdata = data_ready, type = "prob")[, 2]

# Create the ROC curve
roc_curve <- roc(data_ready$symptom_yn, pred_probs)

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for CART Model")
abline(a = 0, b = 1, lty = 2, col = "red")  # Add diagonal reference line

# Calculate the AUC
auc_value <- auc(roc_curve)
print(auc_value)







##bootstrapping
# Define bootstrapping control
train_control_boot <- trainControl(
  method = "boot",  # Bootstrapping
  number = 100,     # Number of bootstrap samples
  classProbs = TRUE,  # Needed for AUC
  summaryFunction = twoClassSummary  # Evaluate using AUC
)

# Train the CART model with bootstrapping
cart_boot <- train(
  symptom_yn ~  gender + continent_clean + travel_purpose + travel_duration + age + health_chronic + smoking_status + trip_number,
  data = data_ready,
  method = "rpart",
  trControl = train_control_boot,
  metric = "ROC"  # Optimize for AUC
)

# View results
print(cart_boot)


# Extract ROC values from bootstrapping
auc_values <- cart_boot$resample$ROC

# Summarize AUC
mean_auc <- mean(auc_values)
ci_auc <- quantile(auc_values, c(0.025, 0.975))  # 95% confidence interval

cat("Mean AUC:", mean_auc, "\n")

#Mean AUC: 0.6462628 

cat("95% CI for AUC:", ci_auc, "\n")

##95% CI for AUC: 0.5788188 0.7023128 



### Random forest
rf_model <- train(
  symptom_yn ~  gender + continent_clean + travel_purpose + travel_duration + age + health_chronic + smoking_status + trip_number,
  data = data_ready,
  method = "rf",
  trControl = train_control_boot,
  metric = "ROC"
)


print(rf_model)

## compares CART and random forest 
cat("CART AUC:", mean(cart_boot$resample$ROC), "\n")
cat("Random Forest AUC:", mean(rf_model$resample$ROC), "\n")