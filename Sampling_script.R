library(dplyr)
library(lubridate)

# Load your data
study_samples <- Tanzania_Dengue  # 385 sequences
world_samples <- Global_Dengue # 26,449 sequences

# Set sampling parameters
total_desired <- 5000  # Adjust this number as needed
study_samples_count <- 385
world_samples_to_select <- total_desired - study_samples_count

# Group world samples by country and year
grouped_world_samples <- world_samples %>%
  mutate(year = year(Collection_Year)) %>%
  group_by(Country, year)

# Sampling function
sample_sequences <- function(data, n) {
  if (nrow(data) <= n) {
    return(data)
  } else {
    return(sample_n(data, n))
  }
}

# Perform sampling on world samples
sampled_world_sequences <- grouped_world_samples %>%
  group_modify(~ sample_sequences(.x, 
                                  n = max(1, round(world_samples_to_select * nrow(.x) / nrow(world_samples))))) %>%
  ungroup()

# Combine sampled world sequences with study samples
final_sampled_sequences <- bind_rows(
  study_samples %>% mutate(source = "study"),
  sampled_world_sequences %>% mutate(source = "world")
)

# Check the results
print(nrow(final_sampled_sequences))
print(table(final_sampled_sequences$Country, final_sampled_sequences$source))


#create a table of accession numbers to download
Accessions <- final_sampled_sequences %>% select(Accession)
 export(Accessions, "data/Accessions.txt")
