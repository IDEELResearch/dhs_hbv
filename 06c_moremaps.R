library(tidyverse)
options(scipen = 999)
# dataset agemoprov5 from 06_mapmap.R
agemoprov5 <- svyby(~prov2015,~hbvresult5+hc1, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
agemoprov5 <- agemoprov5 %>% mutate(level = paste0('hbv', level))
agemo_prov5t <- as.data.frame(t(as.data.frame(agemoprov5))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

agemo_prov5t <- agemo_prov5t %>% mutate(across(-c(level), as.numeric))
colnames(agemo_prov5t)

# New code:
# Function to automatically identify column pairs and singles
identify_column_pairs <- function(agemo_prov5t, level) {
  # Exclude the ID column
  agemo_prov5t <- agemo_prov5t[, !colnames(agemo_prov5t) %in% level, drop = FALSE]
  
  # Extract the variant (after the last period) of each column
  split_names <- strsplit(names(agemo_prov5t), "\\.")
  print(split_names)  # Add this line for debugging
  # Extract the number after the period
  numbers_after_period <- sapply(split_names, function(x) x[length(x)])
  print(numbers_after_period)  # Add this line for debugging
  
  # Create a list to store column groups
  column_groups <- split(names(agemo_prov5t), numbers_after_period)
  print(column_groups)  # Add this line for debugging
  
  return(column_groups)
}

# Function to sum column pairs (or handle singles) and add as new columns
sum_column_pairs <- function(agemo_prov5t, column_groups) {
  for (number_after_period in names(column_groups)) {
    columns <- column_groups[[number_after_period]]
    # Sum columns if there are multiple or handle single columns
    new_col_name <- paste0("sum_", number_after_period)
    if (length(columns) == 1) {
      agemo_prov5t[[new_col_name]] <- agemo_prov5t[, columns]
    } else if (length(columns) > 1) {
      agemo_prov5t[[new_col_name]] <- rowSums(agemo_prov5t[, columns, drop = FALSE])
    }
  }
  return(agemo_prov5t)
}

# Apply the function to sum column pairs based on dynamically identified groups
df_new <- sum_column_pairs(agemo_prov5t, identify_column_pairs(agemo_prov5t, "level"))

# Display the updated DataFrame
view(df_new)

# Function to automatically identify column groups
identify_column_groups <- function(df_new) {
  # Extract the number after the period for each column
  split_names <- strsplit(names(df_new), "\\.")
  group_numbers <- sapply(split_names, function(x) {
    num <- gsub("^\\D*(\\d+).*$", "\\1", x[length(x)])  # Extract digits before any non-digit characters
    as.numeric(num)  # Convert to numeric
  })
  
  # Print debug information
  print("Extracted group numbers:")
  print(group_numbers)
  
  # Identify unique group numbers
  unique_group_numbers <- unique(group_numbers)
  
  # Print debug information
  print("Unique group numbers:")
  print(unique_group_numbers)
  
  # Create a list to store column groups
  column_groups <- lapply(unique_group_numbers, function(num) {
    grep(paste0("\\.", num, "$"), names(df_new), value = TRUE)
  })
  
  return(column_groups)
}

column_groups <- identify_column_groups(df_new)
#Function to calculate proportions and add as new columns
calculate_proportions <- function(df_new, column_groups) {
  for (group_columns in column_groups) {
    # Extract group number
    group_num <- gsub(".*\\.(\\d+)$", "\\1", group_columns[1])
    
    # Check if group_num is NA
    if (is.na(group_num)) {
      print("Group number is NA")
      next
    }
    
    # Extract sum column (sum_x)
    sum_col <- paste0("sum_", group_num)
    
    # Debug print
    print(paste("Group number:", group_num))
    print(paste("Sum column:", sum_col))
    
    # Create proportion column name
    proportion_col <- paste0("proportion_", group_num)
    print(paste("Creating proportion column:", proportion_col))
    
    # Construct numerator column name
    numerator_col <- paste0("hbv1.", group_num)
    print(paste("Numerator column:", numerator_col))
    
    # Check if numerator column exists
    if (!(numerator_col %in% names(df_new))) {
      print(paste("Numerator column", numerator_col, "does not exist. Setting proportion to 0."))
      df_new[[proportion_col]] <- 0
    } else {
      # Calculate proportion directly
      df_new[[proportion_col]] <- ifelse(df_new[[sum_col]] == 0, 0, df_new[[numerator_col]] / df_new[[sum_col]])
    }
    
    # Debug print for proportion column
    print(paste("Proportion column:", df_new[[proportion_col]]))
  }
  return(df_new)
}

# Apply the function to calculate proportions
df_with_proportions <- calculate_proportions(df_new, column_groups)

# Display the updated DataFrame
view(df_with_proportions)

k1 <- df_with_proportions %>% select(c(level, starts_with("proportion")))
view(k1)
library(reshape2)
k2 <- melt(k1)
view(k2)

k2 <- k2 %>% mutate(  age = str_extract(variable, '[0-9]+') %>% as.numeric(),
                      birmo = 60 - age)
view(k2)
k2 <- k2 %>% mutate(hyphen = str_split_fixed(level, "prov2015", 2)[,2])

provprev <- k2 %>% group_by(hyphen) %>% summarise(avgprev = mean(value)) %>% mutate(belowavg = case_when(avgprev>1.2 ~ "Prev >1.2%",
                                                                                                           avgprev <=1.2 ~ "Avg/below 1.2%"))

k3 <- left_join(k2, provprev, by = "hyphen")
# get prov groupings 
#prov11 <- 
provmatch <-elig_kids_whbvres_wt_kr %>% group_by(hv024, prov2015) %>% count()
view(provmatch)

provmatch <- provmatch %>% mutate(oldprov = case_when(
  hv024 == 1 ~ "Kinshasa", 
  hv024 == 2 ~ "Bandundu",
  hv024 == 3 ~ "Bas-Congo",
  hv024 == 4 ~ "Equateur",
  hv024 == 5 ~ "Kasai-Occidental",
  hv024 == 6 ~ "Kasai-Oriental",
  hv024 == 7 ~ "Katanga",
  hv024 == 8 ~ "Maniema",
  hv024 == 9 ~ "Nord-Kivu",
  hv024 == 10 ~ "Orientale",
  hv024 == 11 ~ "Sud-Kivu"))

provmatch <- provmatch %>% dplyr::rename(hyphen = prov2015)
k3 <- left_join(k3, provmatch[,c("oldprov", "hyphen")], by = "hyphen")

k3 %>% 
  ggplot(aes(x=birmo, y = value, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Age in months")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  ggtitle("HBsAg by age in months by province")+
  facet_wrap(~reorder(oldprov, -avgprev), nrow = 2)
ggsave('./Plots/tang/6-59prev_byprov.png', width=12, height=6)

# consider rolling average over a few months-------
library(zoo) # moving averages        
k4 <- k3 %>%
  dplyr::arrange(desc(hyphen)) %>% 
  dplyr::group_by(hyphen) %>% 
  dplyr::mutate(prev_3mo = zoo::rollmean(value, k = 3, fill = NA),
                prev_6mo = zoo::rollmean(value, k = 6, fill = NA),
                prev_9mo = zoo::rollmean(value, k = 9, fill = NA),
                prev_12mo = zoo::rollmean(value, k = 12, fill = NA)) %>% 
  dplyr::ungroup()
view(k4)

threemorolling<-
k4 %>% 
  ggplot(aes(x=birmo, y = 100*prev_3mo, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence (%)", x="Age in months")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
#  scale_color_manual(values = c('#93003a', '#b81b4a', '#d5405e', '#eb6574', '#fb8a8c', '#ffb3a7', '#ffdac4', '#c0eade', '#9dced6', '#80b1cc', '#6694c1', '#4e78b5', '#325da9', '#00429d'))+
  scale_x_reverse(breaks = seq(12, 48, by = 12))+
  #scale_x_continuous(breaks = seq(12, 48, by = 12))+
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  ggtitle("Rolling average of HBsAg prevalence by province using three month age groupings, oldest to youngest")+
  facet_wrap(~reorder(oldprov, -avgprev))
threemorolling
ggsave('./Plots/tang/3moavgprev_byprov.png', width=12, height=6)

#for hbvreuslt1---------
agemoprov1 <- svyby(~prov2015,~hbvresult1+hc1, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
agemoprov1 <- agemoprov1 %>% mutate(level = paste0('hbv', level))
agemo_prov1t <- as.data.frame(t(as.data.frame(agemoprov1))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

agemo_prov1t <- agemo_prov1t %>% mutate(across(-c(level), as.numeric))
colnames(agemo_prov1t)

# New code:
# Function to automatically identify column pairs and singles
identify_column_pairs1 <- function(agemo_prov1t, level) {
  # Exclude the ID column
  agemo_prov1t <- agemo_prov1t[, !colnames(agemo_prov1t) %in% level, drop = FALSE]
  
  # Extract the variant (after the last period) of each column
  split_names <- strsplit(names(agemo_prov1t), "\\.")
  print(split_names)  # Add this line for debugging
  # Extract the number after the period
  numbers_after_period <- sapply(split_names, function(x) x[length(x)])
  print(numbers_after_period)  # Add this line for debugging
  
  # Create a list to store column groups
  column_groups <- split(names(agemo_prov1t), numbers_after_period)
  print(column_groups)  # Add this line for debugging
  
  return(column_groups)
}

# Function to sum column pairs (or handle singles) and add as new columns
sum_column_pairs1 <- function(agemo_prov1t, column_groups) {
  for (number_after_period in names(column_groups)) {
    columns <- column_groups[[number_after_period]]
    # Sum columns if there are multiple or handle single columns
    new_col_name <- paste0("sum_", number_after_period)
    if (length(columns) == 1) {
      agemo_prov1t[[new_col_name]] <- agemo_prov1t[, columns]
    } else if (length(columns) > 1) {
      agemo_prov1t[[new_col_name]] <- rowSums(agemo_prov1t[, columns, drop = FALSE])
    }
  }
  return(agemo_prov1t)
}

# Apply the function to sum column pairs based on dynamically identified groups
df_new <- sum_column_pairs1(agemo_prov1t, identify_column_pairs1(agemo_prov1t, "level"))

# Display the updated DataFrame
view(df_new)

# Function to automatically identify column groups
column_groups <- identify_column_groups(df_new)

#Function to calculate proportions and add as new columns
df_with_proportions <- calculate_proportions(df_new, column_groups)

# Display the updated DataFrame
view(df_with_proportions)

k1 <- df_with_proportions %>% select(c(level, starts_with("proportion")))
view(k1)
library(reshape)
k2 <- melt(k1)

view(k2)

k2 <- k2 %>% mutate(  age = str_extract(variable, '[0-9]+') %>% as.numeric(),
                      birmo = 60 - age,
                      hyphen = str_split_fixed(level, "prov2015", 2)[,2])

provprev1 <- k2 %>% group_by(hyphen) %>% summarise(avgprev = mean(value)) %>% mutate(belowavg = case_when(avgprev>1.2 ~ "Prev >1.2%",
                                                                                                           avgprev <=1.2 ~ "Avg/below 1.2%")) 

k3 <- left_join(k2, provprev1, by = "hyphen")

# provmatch made once above
k3 <- left_join(k3, provmatch[,c("oldprov", "hyphen")], by = "hyphen")
view(k3)
k3 %>% 
  ggplot(aes(x=birmo, y = value, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Birth cohort")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~reorder(oldprov, -avgprev))
ggsave('./Plots/tang/6-59prev_byprov1.png', width=12, height=6)

# consider rolling average over a few months-------
library(zoo) # moving averages        
k4 <- k3 %>%
  dplyr::arrange(desc(hyphen)) %>% 
  dplyr::group_by(hyphen) %>% 
  dplyr::mutate(prev_3mo = zoo::rollmean(value, k = 3, fill = NA),
                prev_6mo = zoo::rollmean(value, k = 6, fill = NA),
                prev_9mo = zoo::rollmean(value, k = 9, fill = NA),
                prev_12mo = zoo::rollmean(value, k = 12, fill = NA)) %>% 
  dplyr::ungroup()
view(k4)

k4 %>% 
  ggplot(aes(x=birmo, y = prev_9mo, color = fct_reorder(hyphen, desc(avgprev))))+ # 3,6,9,12 rolling averages: prev_3mo, prev_6mo, prev_9mo, prev_12mo
  geom_line()+
  labs(y="HBsAg prevalence", x="Age in months")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  #  scale_color_manual(values = c('#93003a', '#b81b4a', '#d5405e', '#eb6574', '#fb8a8c', '#ffb3a7', '#ffdac4', '#c0eade', '#9dced6', '#80b1cc', '#6694c1', '#4e78b5', '#325da9', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~reorder(oldprov, -avgprev))
ggsave('./Plots/tang/3moavgprev_byprov1.png', width=12, height=6)

# for hbvresult100------

agemoprov100 <- svyby(~prov2015,~hbvresult100+hc1, designf, svytotal, na.rm=T, survey.lonely.psu="adjust")  %>% rownames_to_column(var = "level")
agemoprov100 <- agemoprov100 %>% mutate(level = paste0('hbv', level))
agemo_prov100t <- as.data.frame(t(as.data.frame(agemoprov100))) %>% rownames_to_column(var = "prov") %>% row_to_names(row = 1) %>% filter(grepl("prov2015", level) & !grepl("se\\.", level))

agemo_prov100t <- agemo_prov100t %>% mutate(across(-c(level), as.numeric))
view(agemo_prov100t)
# Function to automatically identify column pairs and singles
identify_column_pairs100 <- function(agemo_prov100t, level) {
  # Exclude the ID column
  agemo_prov100t <- agemo_prov100t[, !colnames(agemo_prov100t) %in% level, drop = FALSE]
  
  # Extract the variant (after the last period) of each column
  split_names <- strsplit(names(agemo_prov100t), "\\.")
  print(split_names)  # Add this line for debugging
  # Extract the number after the period
  numbers_after_period <- sapply(split_names, function(x) x[length(x)])
  print(numbers_after_period)  # Add this line for debugging
  
  # Create a list to store column groups
  column_groups <- split(names(agemo_prov100t), numbers_after_period)
  print(column_groups)  # Add this line for debugging
  
  return(column_groups)
}

# Function to sum column pairs (or handle singles) and add as new columns
sum_column_pairs100 <- function(agemo_prov100t, column_groups) {
  for (number_after_period in names(column_groups)) {
    columns <- column_groups[[number_after_period]]
    # Sum columns if there are multiple or handle single columns
    new_col_name <- paste0("sum_", number_after_period)
    if (length(columns) == 1) {
      agemo_prov100t[[new_col_name]] <- agemo_prov100t[, columns]
    } else if (length(columns) > 1) {
      agemo_prov100t[[new_col_name]] <- rowSums(agemo_prov100t[, columns, drop = FALSE])
    }
  }
  return(agemo_prov100t)
}

# Apply the function to sum column pairs based on dynamically identified groups
df_new <- sum_column_pairs100(agemo_prov100t, identify_column_pairs100(agemo_prov100t, "level"))

# Display the updated DataFrame
view(df_new)

# Function to automatically identify column groups
column_groups <- identify_column_groups(df_new)

#Function to calculate proportions and add as new columns
df_with_proportions <- calculate_proportions(df_new, column_groups)

# Display the updated DataFrame
view(df_with_proportions)

k1 <- df_with_proportions %>% select(c(level, starts_with("proportion")))
view(k1)
library(reshape)
k2 <- melt(k1)

view(k2)

k2 <- k2 %>% mutate(  age = str_extract(variable, '[0-9]+') %>% as.numeric(),
                      birmo = 60 - age,
                      hyphen = str_split_fixed(level, "prov2015", 2)[,2])

provprev100 <- k2 %>% group_by(hyphen) %>% summarise(avgprev = mean(value)) %>% mutate(belowavg = case_when(avgprev>1.2 ~ "Prev >1.2%",
                                                                                                          avgprev <=1.2 ~ "Avg/below 1.2%")) 
view(provprev100)
k3 <- left_join(k2, provprev1, by = "hyphen")

# provmatch made once above
k3 <- left_join(k3, provmatch[,c("oldprov", "hyphen")], by = "hyphen")
view(k3)
k3 %>% 
  ggplot(aes(x=birmo, y = value, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Birth cohort")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~reorder(oldprov, -avgprev))
ggsave('./Plots/tang/6-59prev_byprov100.png', width=12, height=6)

# consider rolling average over a few months-------
library(zoo) # moving averages        
k4 <- k3 %>%
  dplyr::arrange(desc(hyphen)) %>% 
  dplyr::group_by(hyphen) %>% 
  dplyr::mutate(prev_3mo = zoo::rollmean(value, k = 3, fill = NA),
                prev_6mo = zoo::rollmean(value, k = 6, fill = NA),
                prev_9mo = zoo::rollmean(value, k = 9, fill = NA),
                prev_12mo = zoo::rollmean(value, k = 12, fill = NA)) %>% 
  dplyr::ungroup()
view(k4)

k4 %>% 
  ggplot(aes(x=birmo, y = prev_3mo, color = fct_reorder(hyphen, desc(avgprev))))+
  geom_line()+
  labs(y="HBsAg prevalence", x="Birth cohort")+
  scale_color_manual(values = c('#93003a', '#a70842', '#b81b4a', '#c82d54', '#d5405e', '#e15268', '#eb6574', '#f4777f', '#fb8a8c', '#ff9e99', '#ffb3a7', '#ffc6b6',  '#d8f6e1', '#c0eade', '#addcda', '#9dced6', '#8ebfd1', '#80b1cc', '#73a2c6', '#6694c1', '#5a86bb', '#4e78b5', '#406aaf', '#325da9', '#204fa3', '#00429d'))+
  #  scale_color_manual(values = c('#93003a', '#b81b4a', '#d5405e', '#eb6574', '#fb8a8c', '#ffb3a7', '#ffdac4', '#c0eade', '#9dced6', '#80b1cc', '#6694c1', '#4e78b5', '#325da9', '#00429d'))+
  theme(panel.background = element_blank(),
        legend.title = element_blank())+
  facet_wrap(~reorder(oldprov, -avgprev))
ggsave('./Plots/tang/3moavgprev_byprov100.png', width=12, height=6)



