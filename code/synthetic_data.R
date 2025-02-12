# Generate synthetic dataset
# See https://www.synthpop.org.uk/about-synthpop.html

pacman::p_load(
        here,
        tidyverse,
        synthpop 
)

# Read in the real, anonymized dataset ####
database_complete <- readRDS(here::here("output", "database_complete_anon.rds")) |> 
        select(-c(serocov_pop:sp4_avril_2022, date_soumission))
my.seed <- 2025

# Generate Synthetic data ####

## Step 1 - split the dataset by questionnaire ####
# (each questionnaire has its own variable structure that should be maintained)
db_list <- split(database_complete, f = ~questionnaire)

## Step 2 - synthesise data for each questionnaire ####
sds_list <- db_list |> 
        map(synthpop::syn,
            seed = my.seed, 
            method = "cart", # default method
            k = 1000 # Make a thousand entries for each questionnaire
            )

## Step 3 - combine the synthetic datasets ####
database_synthetic <- map(sds_list, "syn") %>% # extracts "syn" elements of each questionnaire
        bind_rows() # bind them all into single dataframe

# Save it for downstream use ####
saveRDS(database_synthetic, here::here("output", "database_synthetic.rds"))