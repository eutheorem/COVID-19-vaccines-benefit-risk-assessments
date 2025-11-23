library(tidyverse)

###
### 1. Identifying research groups based on shared authors
###

### Loading custom functions from another R script file. 
source("costom_revtools.R")

### Setting file paths of publication record files. 
file_path <- paste0("nbib_files/")

file_names <- paste0(file_path, list.files(path = file_path))

### Loading publication metadata files. 
complete_bib_data <- read_bibliography2(file_names) 

### Making a publication record that was not found on PubMed.
bourdon <- c(title = "A Reanalysis of the FDA's Benefit-Risk Assessment of Moderna's mRNA-1273 COVID Vaccine: For 18-25-Year-Old Males, Risks Exceeded Benefits Relative to Hospitalizations", 
             year = 2024, abstract = NA, keywords = NA, 
             author_full = "Bourdon, Paul S and Duriseti, Ram and Gromoll, H. Christian and Dalton, Dyana K. and Bardosh, Kevin and Krug, Allison E", 
             affiliation = "USA.", doi = "10.48550/arXiv.2410.11811", journal = "arXiv", year_accepted = 2024)

### Selecting required columns, and adding the additional publication record. 
bib_data <- complete_bib_data %>% 
  # Later in figures, publications will be ordered based on publication year, 
  # as it will make it easier to predict which publications are based on more recent data.
  # However, there is one paper which was accepted and became available in 2022, but was only published in 2024. 
  # Because of this, year (as "year_accepted" column) is considered to be the year of acceptance rather than publication when possible. 
  mutate(year_accepted = ifelse(is.na(publication_history_status) | !grepl("accepted", publication_history_status), 
                                year, 
                                publication_history_status)) %>% 
  # Selecting required columns.
  select("title", "year", "abstract", "keywords", "author_full", "affiliation", "doi", "journal", "year_accepted") %>% 
  # Deleting unnecessary text
  mutate(year_accepted = gsub("^.*\\[revised\\] and ", "", year_accepted)) %>% 
  mutate(year_accepted = gsub("/.*$", "", year_accepted)) %>% 
  # Adding the additional publication record. 
  rbind(., bourdon) %>% 
  # Giving unique ID number to each publication. 
  mutate(ID = 1:nrow(.))

### Removing row names.
rownames(bib_data) <- NULL

### Extracting names of authors so that each author name is stored in a column
authors_separated <- bib_data %>% 
  # Counting the number of authors in each publication
  mutate(author_count = str_count(author_full, " and ") + 1) %>% 
  # Each author name is stored in an individual column
  separate_wider_delim(., col = author_full, delim = " and ", 
                       names = paste("au", seq(1:max(.$author_count)), sep = "_"), 
                       too_few = "align_start", cols_remove = FALSE) %>% 
  select(- author_count) %>% 
  mutate(across(starts_with("au_"), ~ gsub(" [A-Z]\\b", "", .))) %>% 
  # Sometimes, there is an additional "." at the end of author's name, which is removed.
  mutate(across(starts_with("au_"), ~ gsub("\\.$", "", .))) %>% 
  # Making a column for publication labels used for figures. 
  mutate(pub_label = paste0(str_extract(au_1, "[A-Za-z]*"), " et al., ", year))

### Identifying authors who contributed to more than one publication.
prolific_authors <- authors_separated %>% 
  # Converting to a long format
  pivot_longer(cols = starts_with("au_"), values_to = "author_name") %>% 
  # Removing rows without entry in the author_name column
  filter(!is.na(author_name)) %>% 
  # Adding a column showing the number of publications of each author
  left_join(., as.data.frame(table(.$author_name)), by = c("author_name" = "Var1")) %>% 
  # Selecting authors with more than one publication
  filter(Freq > 1)

### Identifying research groups. 
### When an author contributes to two publications, these two publications are considered to be from the same group.
modified_bib_data <- prolific_authors %>% 
  select(author_name, ID) %>% 
  # Left_joining the author name - publication ID data frame, 
  # so that each row contains an author name and IDs of two publications that he/she authored
  left_join(., ., by = c("author_name"), suffix = c("_1", "_2"), relationship = "many-to-many") %>% 
  select(!author_name) %>% 
  distinct() %>% 
  # Removing duplicate rows (e.g., 1 - 2 and 2 - 1)
  filter(ID_1 < ID_2) %>% 
  # Removing further duplicate entries (e.g., when 1 - 2 and 1 - 3 are there, 2 - 3 is not needed.)
  filter(!ID_1 %in% ID_2) %>% 
  # Giving the research group ID numbers based on the ID_1 column
  mutate(research_group = ID_1) %>% 
  pivot_longer(starts_with("ID_"), names_to = "label", values_to = "ID") %>% 
  select(!label) %>% 
  distinct() %>% 
  ### Adding the research group column to the publication record data. 
  left_join(authors_separated, ., by = "ID")



### 
### 2. Identifying countries of authors
###

### Making a vector containing names of all countries.
all_country_w <- c("Palestine", "Pakistan", "Kuwait", "Somalia", "Iraq", 
                   "Saudi Arabia", "Syria", "Israel", "Iran", "Jordan", 
                   "United Arab Emirates", "Lebanon", "Ethiopia", "Bahrain", 
                   "Afghanistan", "Djibouti", "Yemen", "Sudan", "Tunisia", 
                   "Algeria", "Mauritania", "Oman", "Spain", "Portugal", 
                   "Norway", "Netherlands", "Denmark", "Germany", "Switzerland", 
                   "Estonia", "Belgium", "Finland", "Hungary", "Nigeria", 
                   "Brazil", "Paraguay", "Russia", "Niger", "Turkey", 
                   "Peru", "Italy", "Kiribati", "Bolivia", "Georgia_c", "Japan", 
                   "Mexico", "Kazakhstan", "France", "Ireland", "Canada", 
                   "Chad", "Colombia", "Ivory Coast", "UK", "United Kingdom", "England", 
                   "Scotland", "Wales", "Northern Ireland", 
                   "Indonesia", "Czech Republic", "Bangladesh", "USA", "United States", "United States of America", 
                   "Egypt", "India", "Benin", "Cameroon", "Ghana", "Jamaica", "Armenia", 
                   "Cuba", "Haiti", "Romania", "Austria", "Qatar", "Philippines", "Gambia", 
                   "Luxembourg", "El Salvador", "Venezuela", "Guatemala", "Uruguay", "Cyprus", 
                   "China", "Honduras", "Nicaragua", "Panama", "Equatorial Guinea", "Mayotte", 
                   "San Marino", "Serbia", "Montenegro", "Greece", "Croatia", "Eritrea", 
                   "Australia", "South Africa", "Uganda", "Tajikistan", 
                   "Marshall Islands", "South Korea", "Samoa", "Morocco", "Azerbaijan", 
                   "Maldives", "Dominican Republic", "Kenya", "New Zealand", "Bulgaria", 
                   "East Timor", "Latvia", "Palau", "Papua New Guinea", "Libya", "Slovenia", 
                   "Kyrgyzstan", "Turkmenistan", "Sweden", "Congo Democratic Republic", 
                   "Lithuania", "Iceland", "Uzbekistan", "Myanmar", "Costa Rica", "Ecuador", 
                   "Bahamas", "Suriname", "Mauritius", "Ukraine", "Poland", "Sierra Leone", 
                   "Central African Republic", "Antigua and Barbuda", "Niue", "Malaysia", 
                   "Argentina", "Mongolia", "Nepal", "Sri Lanka", "Madagascar", "Togo", 
                   "Thailand", "Cook Islands", "North Korea", "Chile", "Andorra", "Moldova", 
                   "Mozambique", "Guyana", "Saint Lucia", "Netherlands", "Comoros", 
                   "North Macedonia", "Namibia", "Mali", "Trinidad and Tobago", "Botswana", "Tanzania", 
                   "Tuvalu", "Belarus", "Cape Verde", "Tokelau", "Dominica", "Malta", 
                   "Solomon Islands", "Bhutan", "Fiji", "Vietnam", "Guinea-Bissau", "Albania", 
                   "Senegal", "Malawi", "Liechtenstein", "Laos", "Brunei", "Burkina Faso", 
                   "Bosnia and Herzegovina", "Zimbabwe", "Cambodia", "Slovakia", 
                   "Saint Kitts and Nevis", "Barbados", "Belize", "Angola", "Liberia", 
                   "Guinea", "Swaziland", "Saint Vincent and The Grenadines", "Gabon", 
                   "Congo", "Western Sahara", "Burundi", "Rwanda", "Lesotho", "Isle of Man", 
                   "Zambia", "Taiwan", "Micronesia", "Monaco", "Grenada", "Tonga", 
                   "Vanuatu", "Sao Tome and Principe", "Aruba", "Singapore", "Vatican City", 
                   "Seychelles", "Nauru")

### Identifying countries of authors in each publication record. 
countries <- modified_bib_data %>% 
  # Extracting all country names from the affiliation column.
  mutate(country = str_extract_all(affiliation, paste0(all_country_w, "\\.", collapse = "|"))) %>% 
  unnest(cols = country) %>% 
  # Adjusting writing styles.
  mutate(country = gsub("\\.", "", country)) %>% 
  mutate(country = gsub("United Kingdom", "UK", country)) %>% 
  distinct() %>% 
  group_by(ID) %>% 
  # Converting to a wider format.
  mutate(country_no = paste0("country_", 1:length(country))) %>% 
  ungroup() %>% 
  pivot_wider(names_from = country_no, values_from = country) 

### Loading a summary file of studies. 
publications <- read.csv("publications.csv") %>% 
  # The pub_label column is excluded, because the column was originally made by the following code and 
  # and included in the publications.csv.
  select(!pub_label)

### Adding publication label (e.g., XXX et al., 2021), research group, and country columns to the summary file. 
combined_bib <- left_join(publications, 
                          countries %>% select(pub_label, doi, research_group, year_accepted, starts_with("country_")), 
                          by = "doi") %>% 
  mutate(year = ifelse(is.na(year_accepted), year, year_accepted)) %>% 
  # Excluding rows where "include" is FALSE, as these publications are either redundant, unavailable, or not on benefit-risk assessments.
  filter(include) %>% 
  # publications that are not from identified research groups are labelled as "other." 
  # Below, their research_group column will have entries like "other_1", "other_".
  mutate(research_group = ifelse(!is.na(research_group), paste0("group_", research_group), "other")) %>% 
  group_by(research_group) %>% 
  mutate(n_group = 1:n()) %>% 
  ungroup() %>% 
  mutate(research_group = ifelse(research_group == "other", paste(research_group, n_group, sep = "_"), research_group)) %>% 
  # If a publication record lacks a pub_label (i.e., the record was not on PubMed because they are grey literature), 
  # pub_label value is made from the affiliation name and year of publication.
  mutate(pub_label = ifelse(!is.na(pub_label), pub_label, paste0(affiliation, ", ", year))) %>% 
  # For publication record that share the same label, ID numbers are given as suffix. 
  group_by(pub_label) %>% 
  mutate(study_number_in_group = 1:n()) %>% 
  mutate(study_count = max(study_number_in_group)) %>% 
  ungroup() %>% 
  mutate(pub_label = ifelse(study_count == 1, pub_label, paste0(pub_label, "-", study_number_in_group))) %>% 
  select(!c(n_group, study_number_in_group, study_count)) %>% 
  mutate(country_1 = ifelse(is.na(country_1), country, country_1)) %>% 
  mutate(pub_label = ifelse(pub_label == "Tran et al., 2021", "Tran Kiem et al., 2021", pub_label))

###
### 3. Looking into Benefit-risk balance
###

### Loading a summary file of benefit-risk assessments
BR_data <- read.csv("BR_balance.csv") 

### Combining the benefit-risk assessment summary and the bombined bibliographic data.
combined_BR <- combined_bib %>% 
  full_join(., BR_data %>% select(!c(doi, comment)), by = "reference_number") %>% 
  # Making a new column that shows a summary of benefit-risk balance. 
  # The value of the column is one of "benefit", "benefit/comparable", "comparable", "harm/comparable", or "harm", 
  # depending on the highest (in the BR_high column) and lowest (BR_low) benefit-risk ratios of assessments. 
  mutate(BR_balance = ifelse(is.na(BR_high) | is.na(BR_low), NA, 
                             ifelse(BR_high == BR_low, BR_high, 
                                    ifelse(BR_high == "benefit" & BR_low == "comparable", "benefit/comparable", 
                                           ifelse(BR_high == "comparable" & BR_low == "harm", "harm/comparable", "vary"))))) %>% 
  # A new column showing abbreviated vaccine types is made, which will be used a part of graph axis label.
  mutate(vaccine_type2 = ifelse(is.na(vaccine_type), NA, 
                                ifelse(vaccine_type == "AstraZeneca", "(AZ)", 
                                       ifelse(vaccine_type %in% c("Pfizer", "Moderna", "Janssen", "mRNA"), 
                                              paste0("(", vaccine_type, ")"), "(mixed)")))) %>% 
  # preparing to draw a figure by making labels and converting column values to the factor data type 
  mutate(study_ID = paste(pub_label, gender, vaccine_type2, sep = "_"), 
         gender = factor(gender, levels = c("b", "f", "m")), 
         BR_balance = factor(BR_balance, levels = c("benefit", "benefit/comparable", "comparable", "vary", "harm/comparable", "harm")), 
         vaccine_type_simplified = factor(vaccine_type2, 
                                          levels = c("(AZ)", "(Janssen)", "(Moderna)", "(Pfizer)", "(mRNA)", "(mixed)"))) 

### Making a quick plot to see how the data look.
combined_BR %>% 
  filter(!is.na(min_age)) %>% 
  ggplot() + 
  geom_crossbar(aes(ymin = min_age, ymax = max_age, x = factor(study_ID, levels = unique(study_ID)), y = min_age, fill = BR_balance), fatten = 0) + 
  scale_fill_manual(values = c("blue", "skyblue", "green", "yellow", "pink", "red")) + 
  facet_grid(rows = vars(gender), scales = "free", space = "free", switch = "y") +
  coord_flip()
### BR_balance is all "benefit" above 60 years old. 
### For this reason, the age range above 65 is omitted.

### Shaping the data for graphs.
modified_BR <- combined_BR %>% 
  # overall max age is set as 64.8. 
  # Also, max_age is changed to max_age + 0.8, so that boxes have small space between them, 
  # rather than a wide space corresponding to one year.
  mutate(max_age = ifelse(max_age >= 65, 64.8, max_age + 0.8)) %>% 
  # Removing rows where min_age is bigger than 64.8. 
  filter(max_age > min_age) %>% 
  # Sorting orders of studies.
  group_by(reference_number) %>% 
  mutate(max_age_in_study = max(max_age), 
         min_age_in_study = min(min_age)) %>% 
  ungroup() %>% 
  arrange(max_age) %>% 
  arrange(min_age_in_study) %>% 
  arrange(desc(BR_balance)) %>% 
  arrange(max_age_in_study) %>% 
  arrange(desc(vaccine_type2), desc(year), desc(pub_label))

### Making a box plot.
BR_balance_plot <- modified_BR %>% 
  ggplot() + 
  geom_crossbar(aes(ymin = min_age, ymax = max_age, x = factor(study_ID, levels = unique(study_ID)), y = min_age, fill = BR_balance), 
                fatten = 0) + 
  scale_x_discrete(labels = function(x) gsub("_[bmf]_", " ", x)) + 
  theme(legend.position = "top", 
        legend.justification.top = c(2, 0), 
        legend.box.background = element_rect(color = "grey", linewidth = 0.5), 
        legend.key.size = unit(0.8, "line"), 
        legend.text = element_text(face = 2, size = 7), 
        legend.title = element_text(face = 2, size = 7), 
        plot.title = element_text(face = 2, size = 10, hjust = - 20, vjust = 0), 
        axis.text = element_text(size = 7), 
        axis.title = element_text(face = 2, size = 10), 
        strip.text = element_text(face = 2, size = 8)) + 
  labs(title = "Benefit-risk balance of COVID-19 vaccines by age groups", 
       x = "Studies", 
       y = "age groups") + 
  facet_grid(rows = vars(gender), scales = "free", space = "free", switch = "y", 
             labeller = as_labeller(c("b" = "female & male", "f" = "female", "m" = "male"))) +
  coord_flip()

fig_BR_balance <- BR_balance_plot + 
  scale_fill_manual(values = c("blue", "skyblue", "green", "yellow", "pink", "red"), 
                    name = "Balance", 
                    guide = guide_legend(title.position = "top"))

ggsave(fig_BR_balance, 
       filename = "figures/fig_BR_balance.png", 
       width = 150, height = 160, units = "mm", dpi = 300)

### Color-blind friendly plot
### Hex codes of Okabe-Ito colors
#>     orange light blue      green      amber       blue        red     purple 
#>  "#E69F00"  "#56B4E9"  "#009E73"  "#F5C710"  "#0072B2"  "#D55E00"  "#CC79A7" 
#>  #>       grey      black 
#>  "#999999"  "#000000"
#>  
fig_BR_balance_CBF <- BR_balance_plot + 
  scale_fill_manual(values = c("#0072B2", "#56B4E9", "#009E73", "#F5C710", "#CC79A7", "#D55E00"), 
                    name = "Balance", 
                    guide = guide_legend(title.position = "top"))

ggsave(fig_BR_balance_CBF, 
       filename = "figures/fig_BR_balance_CBF.png", 
       width = 150, height = 160, units = "mm", dpi = 300)




###
### The numbers mentioned in the article. 
###

### The number of publications excluded from the analysis
nrow(publications %>% filter(!include))

### The number of studies that consider following benefit measures
sum(grepl("hospitalization", combined_bib$benefit_measure))
sum(grepl("ICU", combined_bib$benefit_measure))
sum(grepl("prevented.*death", combined_bib$benefit_measure))


### The number of studies that consider following adverse effects
sum(grepl("TTS", combined_bib$complication))
sum(grepl("myocarditis|pericarditis", combined_bib$complication))
sum(grepl("anaphylaxis", combined_bib$complication))


### The number of research groups. 
### The numbers and percentages of studies and research groups reaching each conclusion. 
conclusion_counts <- combined_BR %>% 
  select(reference_number, authors_conclusion, research_group) %>% 
  distinct() %>% 
  mutate(n_studies = length(unique(reference_number)), 
         n_groups = length(unique(research_group))) %>% 
  group_by(authors_conclusion) %>% 
  mutate(by_studies = length(unique(reference_number)), 
         by_groups = length(unique(research_group))) %>% 
  ungroup() %>% 
  select(!c(reference_number, research_group)) %>% 
  distinct() %>% 
  mutate(by_studies_rate = by_studies / n_studies, 
         by_groups_rate = by_groups / n_groups)

### The numbers of countries of researchers
country_of_researchers <- combined_bib %>% 
  select(reference_number, starts_with("country_")) %>% 
  pivot_longer(starts_with("country_")) %>% 
  filter(!is.na(value)) %>% 
  select(value) %>% 
  distinct()

### This shows all the countries identified in the publication records. 
participating_countries <- combined_bib %>% 
  select(reference_number, starts_with("country_")) %>% 
  pivot_longer(starts_with("country_"), names_to = "country_no", values_to = "country") %>% 
  filter(!is.na(country)) %>% 
  select(country) %>% 
  distinct()

### The number of studies that are included in the plots.
studies_included_in_plots <- combined_BR %>% filter(!is.na(BR_balance)) %>% 
  select(reference_number) %>% 
  distinct()






