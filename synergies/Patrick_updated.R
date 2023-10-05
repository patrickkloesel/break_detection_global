---
title: "Patricks task"
author: "Elina Dilger"
date: "16-08-2023"
output: ''
---

# 1) Data preparation ---------------------------------------------------------  

library(dplyr)
library(readxl)

data <- readRDS(".\\Policy_out_no_trend_year_var_filtered.RDS")
oecd <- read_xlsx(".\\Kopie von CAPMF_Comp_Sub_10022023.xlsx")

# List of data frames to be joined out of the RDS file
break_dfs <- list(data[[3]][[1]], data[[3]][[2]], data[[3]][[3]], 
                  data[[3]][[4]], data[[3]][[5]], data[[3]][[6]], 
                  data[[3]][[7]], data[[3]][[8]])

# Use Reduce and full_join to join all data frames in the list
breaks <- Reduce(function(x, y) full_join(x, y), break_dfs)
#View(breaks)

# Do the same for the policies attributed
policies_2y_dfs <- list(data[[6]][[1]], data[[6]][[2]], data[[6]][[3]], 
                        data[[6]][[4]], data[[6]][[5]], data[[6]][[6]], 
                        data[[6]][[7]], data[[6]][[8]])

policies_2y <- Reduce(function(x, y) full_join(x, y), policies_2y_dfs)
#View(policies_2y)

# Define a mapping of old policy names to new policy names
# with conditions
policy_mapping <- data.frame(
  old_name = c("AirEmissionStandards", "Auction", "BanPhaseOutCoal",
               "BansPhaseOut_ICE", "BansPhaseOutFF_Heating",
               "BuildingEnergyCodes", "Carbon tax", "CarbonTax_B",
               "Congestion charge", "Emissions trading scheme",
               "EnergyEfficiencyMandates", "ETS_B", "ETS_E", "ETS_I",
               "ExciseTax_T", "ExciseTaxes_B", "ExciseTaxes_E",
               "Expenditure_RailVersusRoad", "FFS_B", "FFS_I", "FFS_T",
               "Financial support", "Financing mechanism",
               "FinancingMechanism_B", "FinancingMechanism_I", "FiT",
               "Labels_Appliances", "Labels_PassengerCars", "MEPS",
               "MEPS_Appliances", "MEPS_ElectricMotors", "MEPS_T",
               "PlanningRenewablesExpansion", "RECs", "Renewable subsidy",
               "Subsidy", "Taxation"),
  new_name = c("airpollutionstandard", "subsidy_e", "ban_e", "ban_t",
               "ban_b", "buildingcode", "taxation_i", "taxation_b",
               "taxation_t", "taxation_e", "energyefficiency_i",
               "taxation_b", "taxation_e", "taxation_i", "taxation_t",
               "taxation_b", "taxation_e", "investment_t",
               "fuelsubsidyreform_b", "fuelsubsidyreform_i",
               "fuelsubsidyreform_t", "subsidy_b", "subsidy_i",
               "subsidy_b", "subsidy_i", "subsidy_e", "label_b",
               "label_t", "MEPS_t", "MEPS_b", "MEPS_i", "MEPS_t",
               "planning_e", "renewable_standard", "subsidy_e",
               "subsidy_b", "taxation_t")
)

# Function to map old policy names to new policy names with conditions
map_policy_names <- function(old_name, module) {
  new_name <- policy_mapping$new_name[policy_mapping$old_name == old_name]
  if (length(new_name) == 0) {
    return(old_name)  # If no mapping found, return the original name
  } else {
    if (module == "Industry" && new_name == "taxation_i") {
      return("taxation_i")
    } else if (module == "Buildings" && new_name == "subsidy_b") {
      return("subsidy_b")
    } else {
      return(new_name)
    }
  }
}

library('purrr')
# Apply the mapping function to the policies_2y DataFrame
policies_2y_modified <- policies_2y %>%
  mutate(Policy = map2_chr(Policy, Module, map_policy_names))

#View(policies_2y_modified)

# 2) Calculate the average effect size ----------------------------------------
# Group policies_2y by unique_break_identifier and count the occurrences
break_counts <- policies_2y_modified %>%
  group_by(unique_break_identifier) %>%
  summarize(count = n())

# Filter policies_2y to include only rows with unique_break_identifiers that 
# appear only once
filtered_unique_breaks <- break_counts %>%
  filter(count == 1) %>%
  pull(unique_break_identifier)

filtered_policies <- policies_2y_modified %>%
  filter(unique_break_identifier %in% filtered_unique_breaks)

# Now filtered_policies contains only the policies attributed to breaks with 
# only one policy attributed
#View(filtered_policies)

# Group filtered_policies by policy and calculate the average effect size for 
# each policy type
average_effect_sizes <- filtered_policies %>%
  group_by(Policy) %>%
  summarize(average_effect_size = mean(coeff, na.rm = TRUE))

# Print the computed average effect sizes for each policy type
View(average_effect_sizes)

# 3) How often is any policy is actually attributed to a break? ---------------
# Step 1: find out how often a policy appears

strings <- c('subsidy_b', 'ban_b', 'fuelsubsidyreform_b', 
              'taxation_b', 'MEPS_b', 'label_b', 'buildingcode', 
              'taxation_e', 'renewable_standard', 'ban_e', 'planning_e', 
              'subsidy_e', 'fuelsubsidyreform_e', 'airpollutionstandard', 
              'subsidy_i', 'taxation_i', 'MEPS_i', 'energyefficiency_i', 
              'fuelsubisdyreform_i', 'ban_t', 'MEPS_t', 'taxation_t',
              'subsidy_t', 'investment_t', 'label_t', 'fuelsubsidyreform_t', 
              'speedlimit')
integers <- c(67, 16, 13, 35, 35, 40, 42, 30, 31, 30, 48, 82, 14, 54, 29, 
            32, 27, 46, 35, 12, 17, 83, 76, 62, 18, 23, 4)

appearance <- data.frame(Policy = strings, numbers = integers)
#View(appearance)


# Count how often each policy type appears
policy_counts <- policies_2y_modified %>%
  group_by(Policy) %>%
  summarise(Count = n())

# Print the policy counts
print(policy_counts)

# If needed, convert the "number" and "Count" columns to numeric
appearance$number <- as.numeric(appearance$number)
policy_counts$Count <- as.numeric(policy_counts$`Count`)

# Calculate ratios of detection to occurrence for each policy type
detection_ratios <- appearance %>%
  left_join(policy_counts, by = "Policy") %>%
  mutate(ratio = Count / number)

# Print the detection ratios
print(detection_ratios)


# 4) Calculate expected effect sizes ------------------------------------------
# multiplying average effect size and detection ratio
expected_effect_sizes <- detection_ratios %>%
  left_join(average_effect_sizes, by = "Policy") %>%
  mutate(Expected_Effect_Size = 
           ratio * average_effect_size)

View(expected_effect_sizes)

# 5) Calculate expected effect sizes for potential policy mixes ---------------
library(combinat)  # For generating combinations

# Step 1: Create a vector of unique policy types
unique_policy_types <- unique(policies_2y_modified$Policy)

# Step 2: Initialize an empty dataframe to store the results
expected_effect_sizes_mixes <- data.frame()

# Step 3: Loop over policy combinations (up to three policies)
for (num_policies in 1:3) {
  policy_combinations <- combinat::combn(unique_policy_types, num_policies, 
                                         simplify = FALSE)
  
  for (comb in policy_combinations) {
    # Filter policies attributed to breaks for the current combination
    filtered_policies_comb <- filtered_policies %>%
      filter(Policy %in% comb)
    
    # Calculate the sum of effect sizes for the current combination
    sum_effect_size <- sum(filtered_policies_comb$coeff, na.rm = TRUE)
    
    # Store the result in the expected_effect_sizes_mixes dataframe
    expected_effect_sizes_mixes <- rbind(
      expected_effect_sizes_mixes, 
      data.frame(Policy_Mix = paste(comb, collapse = "+"), 
      Expected_Effect_Size = sum_effect_size)
      )
  }
}

View(expected_effect_sizes_mixes)

# 6) Calculate the "synergy" of policy mixes ----------------------------------

# Step 1: Identify breaks with more than one policy
breaks_with_multiple_policies <- policies_2y_modified %>%
  group_by(unique_break_identifier) %>%
  filter(n() > 1) %>%
  distinct(unique_break_identifier)

# Step 2: Filter the policies_2y dataframe for identified breaks
multi_policy_breaks <- policies_2y_modified %>%
  filter(unique_break_identifier %in% 
           breaks_with_multiple_policies$unique_break_identifier)

# Step 3: Extract policies and effect sizes for identified breaks
multi_policy_break_info <- multi_policy_breaks %>%
  group_by(unique_break_identifier) %>%
  summarize(Policies = paste(unique(Policy), collapse = "+"),
            Total_Effect_Size = sum(coeff, na.rm = TRUE))

#View(multi_policy_break_info)

# Calculate the synergy by subtracting expected_effect_size from 
# Total_Effect_Size
synergies <- multi_policy_break_info %>%
  left_join(expected_effect_sizes_mixes, by = c("Policies" = "Policy_Mix")) %>%
  mutate(Synergy = Total_Effect_Size - Expected_Effect_Size) %>%
  select(unique_break_identifier, Policies, Total_Effect_Size, 
         Expected_Effect_Size, Synergy)

View(synergies)

# 7) Print results ------------------------------------------------------------
library(gridExtra)
table <-tableGrob(synergies)

# save as pdf
pdf('/Users/Elina/Downloads/synergies.pdf', width = 16, height = 12)
grid.arrange(table)
dev.off()
