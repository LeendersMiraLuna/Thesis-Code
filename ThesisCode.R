# Load libraries
library(readxl)       # For reading Excel files
library(data.table)   # For fast data manipulation
library(clustMixType) # For k-prototypes/k-modes
library(FactoMineR)   # For FAMD / MCA
library(factoextra)   # For visualizing results (e.g., scree plots)
library(cluster)      # For silhouette analysis (pam, daisy)
library(DescTools)    # For Cramér's V
library(caret)        # For nearZeroVar (low-variance detection)
library(openxlsx)     # For writing Excel
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---- Step 1: Read your mapping file ----
mapping <- read.csv(
  "C:/Users/mira.leenders/OneDrive - Obasi/Documenten/dataset sheet1.csv",
  sep = ";",
  stringsAsFactors = FALSE
)

# ---- Step 2: Load your main dataset ----
df <- fread(
  "C:/Users/mira.leenders/OneDrive - Obasi/Documenten/R studio/DATASET THESIS.csv",
  stringsAsFactors = FALSE
)

# ---- Step 3: Fix the gwi-siz-fce prefix and drop unwanted columns ----
# Replace "_" with "-" in the prefix
setnames(
  df,
  old = grep("^gwi_siz_fce", names(df), value = TRUE),
  new = gsub("^gwi_siz_fce", "gwi-siz-fce", grep("^gwi_siz_fce", names(df), value = TRUE))
)

# Remove any gwi-siz-fce.q33, q32, q102, weighting or s2 columns
df <- df[, !grepl("^gwi-siz-fce\\.(q(33|32|102)|weighting|s2)", names(df)), with = FALSE]

# ---- Step 4: Rename columns based on your mapping file ----
# Build the new_name in your mapping
mapping$new_name <- paste(mapping[[3]], mapping[[5]], sep = "_")

# Create the named vector for renaming
name_mapping <- setNames(mapping$new_name, mapping[[1]])

# Apply renaming where possible
setnames(
  df,
  old = intersect(names(name_mapping), names(df)),
  new = name_mapping[ intersect(names(name_mapping), names(df)) ]
)

# ---- (Optional) Quick sanity check ----
cat("Mapped columns:", sum(names(df) %in% mapping$new_name), "\n")
head(names(df), 20)



###### OPTIONAL---- Step 6: Frequency tables 
Freq_select_cols <- grep("^gwi_siz_fce\\.q9b?_", names(df2), value = TRUE)

freq_list <- lapply(Freq_select_cols, function(col) {
  tbl <- as.data.frame(table(df2[[col]], useNA = "ifany"))
  names(tbl) <- c("Value", "Frequency")
  tbl$Column <- col
  tbl
})
freq_table <- rbindlist(freq_list)
# View(freq_table)


#####DEVICES USED EVER####

# Filter columns starting with "devices_used_ever_"
device_cols <- df %>% 
  select(starts_with("Devices Used Ever_"))

# Create frequency tables for each selected column
freq_tables <- map(device_cols, ~ table(.x, useNA = "ifany"))

# Print all frequency tables
freq_tables

# Turn frequency tables into a tidy data frame
freq_df <- freq_tables %>%
  imap_dfr(~ as.data.frame(.x) %>% 
             rename(Value = .x, Frequency = Freq) %>% 
             mutate(Column = .y))

# View result
print(freq_df)



# ---- Step 7: Device‐group reductions ----
# Define the device groups
playstation_cols <- c(
  "Devices Used Ever_PlayStation 5",
  "Devices Used Ever_PlayStation 4 Pro",
  "Devices Used Ever_PlayStation 4",
  "Devices Used Ever_Other PlayStation consoles(i.e., all previous consoles: PS Vita, PS3, PS2, PS1)"
)
xbox_cols <- c(
  "Devices Used Ever_Xbox Series X",
  "Devices Used Ever_Xbox Series S",
  "Devices Used Ever_Xbox One / Xbox One S / Xbox One X",
  "Devices Used Ever_Other Xbox consoles(i.e., all previous consoles: Xbox, Xbox 360)"
)
nintendo_cols <- c(
  "Devices Used Ever_Nintendo Switch",
  "Devices Used Ever_Other Nintendo consoles(i.e., all previous consoles: WiiU, Wii, GameCube, Nintendo 64, SNES, NES)",
  "Devices Used Ever_Nintendo handheld (e.g., 3DS, DS, Gameboy)"
)
pcmac_cols <- c(
  "Devices Used Ever_Apple Mac",
  "Devices Used Ever_A specialised PC built for gaming",
  "Devices Used Ever_PC"
)
mobile_cols <- c(
  "Devices Used Ever_Tablet",
  "Devices Used Ever_Smartphone"
)

# Compute counts (use backticks around non‐syntactic names)
df[, Playstation := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = playstation_cols]
df[, Xbox       := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = xbox_cols]
df[, Nintendo   := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = nintendo_cols]
df[, `PC/Mac`   := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = pcmac_cols]
df[, `Mobile devices`               := rowSums(.SD != "0" & .SD != "", na.rm = TRUE), .SDcols = mobile_cols]

# Optionally drop the original detail columns
cols_to_delete <- c(
  playstation_cols,
  xbox_cols,
  nintendo_cols,
  pcmac_cols,
  mobile_cols
)
df[, (cols_to_delete) := NULL]

#####END DEVICES USED EVER











####################################Games Played in last 12M and Regularly


library(data.table)
setDT(df)  # your main dataset

# Load mapping table
map <- fread("C:\\Users\\mira.leenders\\Downloads\\Game_Genre_Mapping.csv")  

# Create a helper function to extract the game name from column names
extract_game_name <- function(colname) {
  sub(".*_", "", colname)
}

# Get all game-related columns
game_cols <- grep("^Games Played (in Last 12 Months|Regularly)", names(df), value = TRUE)

# Build a table mapping columns to genres
col_map <- data.table(
  original_col = game_cols,
  game = sapply(game_cols, extract_game_name)
)

# Merge with genre mapping
col_map <- merge(col_map, map, by.x = "game", by.y = "Game", all.x = TRUE)

# Now split for each question type
for (qtype in c("Last 12 Months", "Regularly")) {
  # Subset to only those columns for the current question type
  cols_this_type <- col_map[grepl(qtype, original_col)]
  
  # Loop over genres
  for (g in unique(na.omit(cols_this_type$Genre))) {
    # Get the columns for this genre and type
    genre_cols <- cols_this_type[Genre == g & grepl(qtype, original_col), original_col]
    
    # Create the new genre summary column
    new_colname <- paste0(gsub(" ", "_", g), "_", gsub(" ", "_", qtype))
    df[, (new_colname) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE), .SDcols = genre_cols]
  }
}

View(df[, ..genre_cols])
# Show all genre columns added
genre_cols <- grep("_(Last_12_Months|Regularly)$", names(df), value = TRUE)
df[, ..genre_cols]  # Show them in your console




library(dplyr)

names(df) <- make.unique(names(df), sep = "_dup_")


# Columns for 'None of these'
none1 <- "Games Played in Last 12 Months (Part 1 of 2)_None of these"
none2 <- "Games Played in Last 12 Months (Part 2 of 2)_None of these"

# Add the new column
df$Games_None_of_these_Both <- ifelse(
  df[[none1]] == "None of these" & df[[none2]] == "None of these",
  "none of these",
  0
)

# Define the 10 exact column names you want to KEEP
keep_exact <- c(
  "Games Played in Last 12 Months (Part 1 of 2)_Candy Crush",
  "Games Played in Last 12 Months (Part 1 of 2)_FIFA",
  "Games Played in Last 12 Months (Part 2 of 2)_Mario Kart",
  "Games Played in Last 12 Months (Part 1 of 2)_Call of Duty",
  "Games Played in Last 12 Months (Part 2 of 2)_Minecraft",
  "Games Played in Last 12 Months (Part 1 of 2)_Fortnite",
  "Games Played in Last 12 Months (Part 2 of 2)_Pokemon",
  "Games Played in Last 12 Months (Part 1 of 2)_GTA V",
  "Games Played in Last 12 Months (Part 1 of 2)_Call of Duty Warzone",
  "Games Played in Last 12 Months (Part 2 of 2)_Mortal Kombat"
)

# Identify all columns starting with "Games Played in Last 12 Months"
all_game_cols <- grep("^Games Played in Last 12 Months \\(Part [12] of 2\\)_", names(df), value = TRUE)

# Remove all such columns EXCEPT the ones you want to keep
cols_to_remove <- setdiff(all_game_cols, keep_exact)

# Drop those columns directly from df
df[, (cols_to_remove) := NULL]




# Define the games you want to KEEP
keep_games <- c("Call of Duty", "Candy Crush", "FIFA", "Fortnite", "GTA V",
                "Mario Kart", "Minecraft", "Pokemon", "Roblox", "League of Legends", "None of these")

# Find all columns that start with 'Games Played Regularly_'
all_regular_cols <- grep("^Games Played Regularly_", names(df), value = TRUE)

# Extract the part after the '_'
game_names <- sub("^Games Played Regularly_", "", all_regular_cols)

# Find columns to KEEP (match on exact game name)
keep_cols <- all_regular_cols[game_names %in% keep_games]

# Drop all others
cols_to_remove <- setdiff(all_regular_cols, keep_cols)
df[, (cols_to_remove) := NULL]



##############################END GAMES PLAYED LAST 12 M and RGEULRALRY








##################################Esports Watched Ever

library(data.table)
library(stringr)

# 1. Load your mapping file
mapping <- fread("C:/Users/mira.leenders/Downloads/Final_Esports_Watched_Ever_Mapping.csv")

# 2. Identify all "Esports Watched Ever_" columns in df
watched_cols <- grep("^Esports Watched Ever_", names(df), value = TRUE)

# 3. Build a helper table and extract the game title
watched_dt <- data.table(column = watched_cols)
watched_dt[, `Original Esport Title` := 
             str_replace(column, "^Esports Watched Ever_", "")
]

# 4. Join in the genre mapping
#    (assumes mapping has columns "Original Esport Title" and "Grouped Genre / Category")
watched_dt <- merge(
  watched_dt,
  mapping,
  by = "Original Esport Title",
  all.x = TRUE
)

# 5. For each genre, count how many titles each respondent watched ever
for (g in unique(watched_dt$`Grouped Genre / Category`)) {
  # all source cols for this genre
  cols_for_genre <- watched_dt[`Grouped Genre / Category` == g, column]
  
  # make a syntactic new column name
  safe_genre    <- gsub("[ /]", "_", g)
  new_colname   <- paste0("Watched_Ever_", safe_genre)
  
  # add summary column to df
  df[, (new_colname) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),
     .SDcols = cols_for_genre
  ]
}

# (Optional) Inspect the new genre‐count columns
genre_cols <- grep("^Watched_Ever_", names(df), value = TRUE)
df[, ..genre_cols]



# Define esports titles to KEEP
keep_esports <- c("FIFA", "Fortnite", "Call of Duty", "League of Legends", 
                  "Call Of Duty Warzone", "Mortal Kombat 11", "PUBG", 
                  "Call of Duty Mobile", "League of Legends: Wild Rift", 
                  "Counter-Strike 2 / CS:GO")

# Get all 'Esports Watched Ever_' columns
all_esports_cols <- grep("^Esports Watched Ever_", names(df), value = TRUE)

# Extract game names after the underscore
esports_names <- sub("^Esports Watched Ever_", "", all_esports_cols)

# Keep only columns whose game title matches one of your list
keep_cols <- all_esports_cols[esports_names %in% keep_esports]

# Drop all other Esports Watched Ever_ columns
cols_to_remove <- setdiff(all_esports_cols, keep_cols)
df[, (cols_to_remove) := NULL]


#####################################ESPORTSWATCHEDEVER








library(data.table)

# Voorbeeld: je dataset heet df
# Zorg dat df een data.table is
df <- as.data.table(df)

# Hernoem exact de kolommen zoals opgegeven
setnames(df, old = c(
  "gwi-siz-fce.q24_1",
  "gwi-siz-fce.q24_2",
  "gwi-siz-fce.q24_3",
  "gwi-siz-fce.q24_4",
  "gwi-siz-fce.q24_5",
  "gwi-siz-fce.q24_6",
  "gwi-siz-fce.q24_7",
  "gwi-siz-fce.q24_8",
  "gwi-siz-fce.q24_9",
  "gwi-siz-fce.q24_10",
  "gwi-siz-fce.q24_11",
  "gwi-siz-fce.q24_12",
  "gwi-siz-fce.q24_13",
  "gwi-siz-fce.q24_14",
  "gwi-siz-fce.q24_15",
  "gwi-siz-fce.q24_16",
  "gwi-siz-fce.q24_17",
  "gwi-siz-fce.q24_18",
  "gwi-siz-fce.q24_19",
  "gwi-siz-fce.q24_20",
  "gwi-siz-fce.q24_21",
  "gwi-siz-fce.q24_22",
  "gwi-siz-fce.q24_23",
  "gwi-siz-fce.q24_24",
  "gwi-siz-fce.q24_25",
  "gwi-siz-fce.q24_26",
  "gwi-siz-fce.q24_27",
  "gwi-siz-fce.q24_28",
  "gwi-siz-fce.q24_29",
  "gwi-siz-fce.q24_30",
  "gwi-siz-fce.q24_31"
), new = c(
  "WatchingBehavior_Apex_Legends",
  "WatchingBehavior_Arena_of_Valor",
  "WatchingBehavior_BGMI_or_PUBG_Mobile",
  "WatchingBehavior_PUBG",
  "WatchingBehavior_Call_of_Duty_Mobile",
  "WatchingBehavior_Call_of_Duty_Warzone",
  "WatchingBehavior_Clash_Royale",
  "WatchingBehavior_Counter_Strike_2_or_CSGO",
  "WatchingBehavior_Dota_2",
  "WatchingBehavior_FIFA",
  "WatchingBehavior_Fortnite",
  "WatchingBehavior_Hearthstone",
  "WatchingBehavior_League_of_Legends",
  "WatchingBehavior_Mobile_Legends",
  "WatchingBehavior_Mortal_Kombat_11",
  "WatchingBehavior_NBA_2K",
  "WatchingBehavior_Overwatch",
  "WatchingBehavior_Rainbow_Six_Siege",
  "WatchingBehavior_Rocket_League",
  "WatchingBehavior_SIM_Racing",
  "WatchingBehavior_StarCraft_2",
  "WatchingBehavior_Super_Smash_Bros_Ultimate",
  "WatchingBehavior_Other_Fighting_Games",
  "WatchingBehavior_Teamfight_Tactics",
  "WatchingBehavior_Trackmania",
  "WatchingBehavior_Valorant",
  "WatchingBehavior_WarCraft_3",
  "WatchingBehavior_League_of_Legends_Wild_Rift",
  "WatchingBehavior_Age_of_Empires",
  "WatchingBehavior_Magic_The_Gathering_Arena",
  "WatchingBehavior_Call_of_Duty"
))






# Laad benodigde libraries
library(data.table)
library(dplyr)

# Laad je dataframe
df <- as.data.table(df)

# Zet hieronder je eigen mapping van titel naar genre
genre_mapping <- list(
  "Apex_Legends" = "Battle Royale",
  "Arena_of_Valor" = "MOBA",
  "BGMI_or_PUBG_Mobile" = "Battle Royale",
  "PUBG" = "Battle Royale",
  "Call_of_Duty_Mobile" = "FPS / Tactical Shooter",
  "Call_of_Duty_Warzone" = "FPS / Tactical Shooter",
  "Call_of_Duty" = "FPS / Tactical Shooter",
  "Clash_Royale" = "Auto Battler / Strategy",
  "Counter_Strike_2_or_CSGO" = "FPS / Tactical Shooter",
  "Dota_2" = "MOBA",
  "FIFA" = "Sports Sim",
  "Fortnite" = "Battle Royale",
  "Hearthstone" = "Other / Hybrid",
  "League_of_Legends" = "MOBA",
  "Mobile_Legends" = "MOBA",
  "Mortal_Kombat_11" = "Fighting Games",
  "NBA_2K" = "Sports Sim",
  "Overwatch" = "FPS / Tactical Shooter",
  "Rainbow_Six_Siege" = "FPS / Tactical Shooter",
  "Rocket_League" = "Racing / Vehicle Sim",
  "SIM_Racing" = "Racing / Vehicle Sim",
  "StarCraft_2" = "Auto Battler / Strategy",
  "Super_Smash_Bros_Ultimate" = "Fighting Games",
  "Other_Fighting_Games" = "Fighting Games",
  "Teamfight_Tactics" = "Auto Battler / Strategy",
  "Trackmania" = "Racing / Vehicle Sim",
  "Valorant" = "FPS / Tactical Shooter",
  "WarCraft_3" = "Auto Battler / Strategy",
  "League_of_Legends_Wild_Rift" = "MOBA",
  "Age_of_Empires" = "Auto Battler / Strategy",
  "Magic_The_Gathering_Arena" = "Auto Battler / Strategy"
)

# Definieer categorieën
low_answers <- c(
  "I won't tune in for any particular tournament, but might watch the occasional game out of interest",
  "I will only watch the finals of the biggest tournaments"
)

medium_answers <- c("I only watch the biggest tournaments")
high_answers <- c("I watch most tournaments I can", "I'll tune into every tournament I can")

# Initialiseer lege kolommen
df$Watching_Low <- ""
df$Watching_Medium <- ""
df$Watching_High <- ""

# Doorloop alle kolommen
for (col in names(genre_mapping)) {
  full_colname <- paste0("WatchingBehavior_", col)
  genre <- genre_mapping[[col]]
  
  df$Watching_Low <- ifelse(df[[full_colname]] %in% low_answers,
                            paste(df$Watching_Low, genre, sep = "; "), df$Watching_Low)
  
  df$Watching_Medium <- ifelse(df[[full_colname]] %in% medium_answers,
                               paste(df$Watching_Medium, genre, sep = "; "), df$Watching_Medium)
  
  df$Watching_High <- ifelse(df[[full_colname]] %in% high_answers,
                             paste(df$Watching_High, genre, sep = "; "), df$Watching_High)
}

# Optioneel: trim spaties en beginpuntkomma’s
df$Watching_Low <- trimws(gsub("^;\\s*", "", df$Watching_Low))
df$Watching_Medium <- trimws(gsub("^;\\s*", "", df$Watching_Medium))
df$Watching_High <- trimws(gsub("^;\\s*", "", df$Watching_High))






########TEAMSSUPPORTED
# 1) All supported‐team columns in one vector
all_supported <- grep("^Esports Team Supported_", names(df), value = TRUE)

# 2) The nine you want to keep
keep_teams <- c(
  "Esports Team Supported_Cloud9",
  "Esports Team Supported_Excel Esports",
  "Esports Team Supported_Fnatic",
  "Esports Team Supported_G2 Esports",
  "Esports Team Supported_PSG",
  "Esports Team Supported_Red Bull",
  "Esports Team Supported_T1",
  "Esports Team Supported_Team Liquid",
  "Esports Team Supported_Infinity", 
  "Esports Team Supported_FaZe Clan"
)

# 3) Make sure those nine actually exist in your df
keep_present <- intersect(keep_teams, all_supported)

# 4) Compute total count of *all* teams supported per row
df[ , total_supported := rowSums(
  .SD != "0" & .SD != "", 
  na.rm = TRUE
), .SDcols = all_supported
]

# 5) Compute count of just the 9 keep teams per row
if (length(keep_present) > 0) {
  df[ , keep_count := rowSums(
    .SD != "0" & .SD != "", 
    na.rm = TRUE
  ), .SDcols = keep_present
  ]
} else {
  # in the unlikely event none of your nine exist
  df[ , keep_count := 0L ]
}

# 6) Other = total_supported − keep_count
df[ , Other := total_supported - keep_count ]

# 7) Clean up temporary helpers
df[ , c("total_supported", "keep_count") := NULL ]






# Define the exact columns you want to keep
keep_teams <- c(
  "Esports Team Supported_Cloud9",
  "Esports Team Supported_Excel Esports",
  "Esports Team Supported_Fnatic",
  "Esports Team Supported_G2 Esports",
  "Esports Team Supported_PSG",
  "Esports Team Supported_Red Bull",
  "Esports Team Supported_T1",
  "Esports Team Supported_Team Liquid",
  "Esports Team Supported_Infinity", 
  "Esports Team Supported_FaZe Clan"
)

# Find all columns that start with 'Esports Team Supported_'
all_team_cols <- grep("^Esports Team Supported_", names(df), value = TRUE)

# Identify which ones to remove (all others not in keep_teams)
cols_to_remove <- setdiff(all_team_cols, keep_teams)

# Remove them safely
df[, (cols_to_remove) := NULL]

######################################### END ESPORTS TEAM SUPPORTED




#######################################NON ESPORTS GAMES 
# 1) grab the names
watched_cols <- grep(
  "^Non-Esports Games Watched \\(Part [12] of 2\\)_",
  names(df),
  value = TRUE
)

# 2) print them
print(watched_cols)


library(data.table)

# 1) ensure df is a data.table
setDT(df)

# 2) load your Non-Esports mapping (has columns Game, Category, Reason)
map_ne <- fread("C:\\Users\\mira.leenders\\Downloads\\Full_Non_Esports_Game_Grouping_With_Reasons.csv")

# 3) grab all the watched-game columns (parts 1 & 2)
watched_cols <- grep(
  "^Non-Esports Games Watched \\(Part [12] of 2\\)_",
  names(df),
  value = TRUE
)

# 4) build a little lookup: original_col → game → Category
col_map <- data.table(original_col = watched_cols)
col_map[, game := sub(".*_", "", original_col)]
col_map <- merge(
  col_map,
  map_ne[, .(Game, Category)],
  by.x = "game",
  by.y = "Game",
  all.x = TRUE
)

# 5) for each non-NA Category, sum up that respondent’s picks
for(cat in unique(na.omit(col_map$Category))) {
  cols_for_cat <- col_map[Category == cat, original_col]
  # make a clean name for the new column
  safe_cat <- gsub("[^A-Za-z0-9]", "_", cat)
  new_col  <- paste0(safe_cat, "_Watched")
  
  df[, (new_col) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),
     .SDcols = cols_for_cat]
}

# 6) inspect all the new category columns
genre_cols <- grep("_Watched$", names(df), value = TRUE)
head(df[, ..genre_cols])





# Define the titles to KEEP
keep_games <- c("FIFA", "Minecraft", "Call of Duty", "Fortnite", "Candy Crush",
                "Mario Kart", "GTA V", "Pokemon", "Resident Evil", "Mortal Kombat")

# Step 1: Identify all Non-Esports columns
all_non_esports_cols <- grep("^Non-Esports Games Watched \\(Part [12] of 2\\)_", names(df), value = TRUE)

# Step 2: Extract the game title from each column
game_titles <- sub("^Non-Esports Games Watched \\(Part [12] of 2\\)_", "", all_non_esports_cols)

# Step 3: Determine which columns to keep
keep_cols <- all_non_esports_cols[game_titles %in% keep_games]

# Step 4: Find the two 'None of these' columns
none1 <- grep("^Non-Esports Games Watched \\(Part 1 of 2\\)_None of these", names(df), value = TRUE)
none2 <- grep("^Non-Esports Games Watched \\(Part 2 of 2\\)_None of these", names(df), value = TRUE)

# Step 5: Add new logical column
df[, `Non Esports Games_None of these` := ifelse(
  get(none1) == "None of these" & get(none2) == "None of these",
  "None of these", 0
)]

# Step 6: Drop all other Non-Esports columns except those in `keep_cols`
cols_to_remove <- setdiff(all_non_esports_cols, keep_cols)
df[, (cols_to_remove) := NULL]




#######################END NON ESPORTS GAMES




####################"CHANNEL AWARENESS AND CONTENT TYPE WATCHED


library(data.table)
setDT(df)

# 1) Identify source columns
aw_cols   <- grep("^Channel Awareness_", names(df), value=TRUE)
ct_base   <- sub("^Channel Awareness_", "Content Type Watched on Each Channel_", aw_cols)
channels  <- sub("^Channel Awareness_", "", aw_cols)

# 2) Loop over channels, but do each entirely vectorized
for (i in seq_along(channels)) {
  ch      <- channels[i]
  aw      <- df[[ aw_cols[i] ]]        # logical-ish awareness vector
  c0      <- df[[ ct_base[i]    ]]     # .   → esports competitions
  c1      <- df[[ paste0(ct_base[i],".1") ]]  # .1 → non-esports
  c2      <- df[[ paste0(ct_base[i],".2") ]]  # .2 → not watch
  
  # start with empty string
  result <- rep("", nrow(df))
  
  # mark awareness
  is_aw <- !aw %in% c("0","",NA)
  result[is_aw] <- "aware"
  
  if (ch != "None of these") {
    # append each content-type label
    # for esports competitions
    idx <- !c0 %in% c("0","",NA)
    result[idx] <- paste0(
      result[idx],
      ifelse(result[idx]=="", "", ", "),
      "watch esports competitions"
    )
    
    # non-esports gaming
    idx <- !c1 %in% c("0","",NA)
    result[idx] <- paste0(
      result[idx],
      ifelse(result[idx]=="", "", ", "),
      "watch non-esports gaming content"
    )
    
    # not watch gaming
    idx <- !c2 %in% c("0","",NA)
    result[idx] <- paste0(
      result[idx],
      ifelse(result[idx]=="", "", ", "),
      "not watch gaming content"
    )
  }
  
  # assign in one go
  set(df, j = ch, value = result)
}


#############CHANNEL AWARENESS AND CONTENT TYPE WATCHED 




######################################ADVERTISING TYPE
library(data.table)
setDT(df)

# 1) grab your source cols
rec_cols  <- grep("^Recognition of Esports Advertising Types_", names(df), value=TRUE)
pref_cols <- grep("^Preference of Esports Advertising Types_",     names(df), value=TRUE)

# 2) extract the ad-type (the bit after the final “_”)
ad_types  <- sub(".*_", "", rec_cols)

# 3) sanitize into valid column names (e.g. “None of these” → “None_of_these”)
new_cols  <- gsub("[^A-Za-z0-9]+", "_", ad_types)

# 4) loop over each type
for (i in seq_along(ad_types)) {
  rec_col <- rec_cols[i]
  pref_col<- pref_cols[i]
  new_col <- new_cols[i]
  type    <- ad_types[i]
  
  # logical vectors: did they select this type?
  rec_vec  <- df[[rec_col]]  == type
  pref_vec <- df[[pref_col]] == type
  
  # initialize all to empty string
  out <- rep("", nrow(df))
  
  # both recognition & preference
  both        <- rec_vec & pref_vec
  out[both]   <- "Recognition, Preference"
  
  # only recognition
  only_rec    <- rec_vec & !pref_vec
  out[only_rec] <- "Recognition"
  
  # only preference
  only_pref   <- pref_vec & !rec_vec
  out[only_pref] <- "Preference"
  
  # assign the new column
  df[, (new_col) := out]
}

# 5) (optional) drop the 18 originals
df[, c(rec_cols, pref_cols) := NULL]

# Result: df now has 9 columns named after each ad type,
# each cell either "", "Recognition", "Preference", or "Recognition, Preference".


###################################ADVERTISING TYPE




#####################"SPONSOR CATEGORIES

# 1) Grab the two sets of source columns
exp_cols  <- grep("^Expected Sponsor Categories_", names(df), value = TRUE)
pref_cols <- grep("^Preference of Sponsor Categories_", names(df), value = TRUE)

# 2) Extract the category names (they’re identical in order)
categories <- sub("^Expected Sponsor Categories_", "", exp_cols)

# 3) Turn those into “safe” column names (e.g. spaces → underscores)
safe_names <- gsub("[^A-Za-z0-9]", "_", categories)

# 4) Loop over each category and build the new summary column
for (i in seq_along(categories)) {
  cat    <- categories[i]
  safe   <- safe_names[i]
  e_col  <- exp_cols[i]
  p_col  <- pref_cols[i]
  
  # logicals for each respondent
  e_vec  <- df[[e_col]]  == cat
  p_vec  <- df[[p_col]]  == cat
  
  # start with empty
  out <- rep("", nrow(df))
  
  # both
  both <- e_vec & p_vec
  out[both] <- "expectation, preference"
  
  # only expectation
  only_e <- e_vec & !p_vec
  out[only_e] <- "expectation"
  
  # only preference
  only_p <- p_vec & !e_vec
  out[only_p] <- "preference"
  
  # assign to df
  set(df, j = safe, value = out)
}

# 5) (Optional) Drop the original 28 columns
df[, c(exp_cols, pref_cols) := NULL]

# 6) Quick sanity check
head(df[, safe_names, with = FALSE])
###########################################SPONSOR CATEGORIES




##########################MERCH
library(data.table)

# Ensure df is a data.table
setDT(df)

# 1. Get all relevant columns
all_cols <- grep("^Merchandise Purchase History / Intention_", names(df), value = TRUE)

# 2. Get unique base labels by stripping _dup_ suffixes
base_labels <- unique(sub("_dup_\\d+$", "", all_cols))

# 3. Create safe new column names
summary_names <- sub("^Merchandise Purchase History / Intention_", "", base_labels)
summary_names <- gsub("[^A-Za-z0-9]", "_", summary_names)

# 4. Loop over base labels to build new columns
for (i in seq_along(base_labels)) {
  base <- base_labels[i]
  new_col <- summary_names[i]
  out <- rep("", nrow(df))
  
  # Column variants
  purchased_col  <- base
  interested_col <- paste0(base, "_dup_1")
  neither_col    <- paste0(base, "_dup_2")
  
  # Fill values if present
  if (purchased_col %in% names(df)) {
    idx <- df[[purchased_col]] != "0" & df[[purchased_col]] != ""
    out[idx] <- "has purchased this"
  }
  
  if (interested_col %in% names(df)) {
    idx <- df[[interested_col]] != "0" & df[[interested_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "interested in purchasing in future",
                       paste(out[idx], "interested in purchasing in future", sep = ", "))
  }
  
  if (neither_col %in% names(df)) {
    idx <- df[[neither_col]] != "0" & df[[neither_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "neither of these",
                       paste(out[idx], "neither of these", sep = ", "))
  }
  
  # Save the new summary column
  df[[new_col]] <- out
}

# Optional: check results
head(df[, ..summary_names])


#####################################END MERCH













#################################PC BRANDS
# Brand Awareness columns
awareness_cols <- grep("^PC Brand Awareness_", names(df), value = TRUE)

# Brand Ownership columns
ownership_cols <- grep("^PC Brand Ownership_", names(df), value = TRUE)

# Brands Best Suited to Gaming columns
best_suited_cols <- grep("^Brands Best Suited to Gaming_", names(df), value = TRUE)

# Print them
print(awareness_cols)
print(ownership_cols)
print(best_suited_cols)



# 1) Your three vectors (you already have these)
awareness_cols     <- print(awareness_cols)
ownership_cols     <- print(ownership_cols)
best_suited_cols   <- print(best_suited_cols)

# 2) Extract just the brand name (strip the prefix)
aw_brands   <- sub("^PC Brand Awareness_",            "", awareness_cols)
own_brands  <- sub("^PC Brand Ownership_",            "", ownership_cols)
best_brands <- sub("^Brands Best Suited to Gaming_",  "", best_suited_cols)

# 3) If some have trailing punctuation (e.g. the "." on "None of these."), remove it
aw_brands   <- gsub("\\.$", "", aw_brands)
own_brands  <- gsub("\\.$", "", own_brands)
best_brands <- gsub("\\.$", "", best_brands)

# 4) Check if they’re all the same set
cat("Aw vs Own identical? ", setequal(aw_brands, own_brands), "\n")
cat("Aw vs Best identical? ", setequal(aw_brands, best_brands), "\n")
cat("Own vs Best identical? ", setequal(own_brands, best_brands), "\n")

# 5) If any of those are FALSE, see what differs
cat("In Awareness but not Ownership:\n")
print(setdiff(aw_brands, own_brands))

cat("In Ownership but not Awareness:\n")
print(setdiff(own_brands, aw_brands))

cat("In Awareness but not Best:\n")
print(setdiff(aw_brands, best_brands))

cat("In Best but not Awareness:\n")
print(setdiff(best_brands, aw_brands))




Freq_select_cols <- grep("^Peripheral Brand Awareness_", names(df), value = TRUE)
freq_list <- lapply(Freq_select_cols, function(col) {
  tbl <- as.data.frame(table(df[[col]], useNA = "ifany"))
  names(tbl) <- c("Value", "Frequency")
  tbl$Column <- col
  tbl
})
freq_table <- rbindlist(freq_list)
# View(freq_table)


library(data.table)

# 1) Define your group → brand mapping
brand_groups <- list(
  `Major Global PC Brands`              = c("Dell","hp","Lenovo", "Medion", "Toshiba","Acer","Apple (Mac)","Apple", "Samsung","Fujitsu", "ASUS"),
  `Gaming-Focused Brands`       = c("MSI", "ROG - Republic of Gamers", "Legion", "NZXT", "Alienware", "Corsair", "Razer", "AORUS", "OMEN", "CyberPowerPC", "SCAN", "Predator", "Xidax", "Origin", "LDLC", "Galleria", "iBuyPower", "Velocity Micro", "Alternate", "XMG", "Origin PC", "Falcon Northwest", "Maingear", "Velocity", "PCSpecialist", "Schenker", "Digital Storm", "Materiel.net", "King Mod Systems", "Millenium"),
  `Component Manufacturers/ Niche Hardware Brands` = c("Shuttle", "Gigabyte", "Leadtek", "Lite-On", "Biostar", "Tones", "ZOTAC", "Albatron", "Aerocool"),
  `Self-Built or Custom`         = c("Self-built"),
  `None of these`           = c("None of these")
)

# 2) Grab the three blocks of columns
aw_cols   <- grep("^PC Brand Awareness_",            names(df), value=TRUE)
own_cols  <- grep("^PC Brand Ownership_",            names(df), value=TRUE)
best_cols <- grep("^Brands Best Suited to Gaming_",  names(df), value=TRUE)

# 3) Extract clean brand names
aw_brands   <- gsub("\\.$","", sub("^PC Brand Awareness_",           "", aw_cols))
own_brands  <- gsub("\\.$","", sub("^PC Brand Ownership_",           "", own_cols))
best_brands <- gsub("\\.$","", sub("^Brands Best Suited to Gaming_", "", best_cols))

# 4) For each group, build the summary column
setDT(df)
for (grp in names(brand_groups)) {
  # safe R column name
  safe_grp <- gsub("[^A-Za-z0-9]+","_", grp)
  members  <- brand_groups[[grp]]
  
  # find which original cols belong here
  aw_match   <- aw_cols[   aw_brands   %in% members ]
  own_match  <- own_cols[  own_brands  %in% members ]
  best_match <- best_cols[ best_brands %in% members ]
  
  # initialize output vector
  out <- rep("", nrow(df))
  
  # any awareness?
  if (length(aw_match)) {
    idx <- rowSums(df[, ..aw_match] != "0" & df[, ..aw_match] != "", na.rm=TRUE) > 0
    out[idx] <- "aware"
  }
  
  # any ownership?
  if (length(own_match)) {
    idx <- rowSums(df[, ..own_match] != "0" & df[, ..own_match] != "", na.rm=TRUE) > 0
    out[idx] <- ifelse(out[idx]=="", "owner", paste(out[idx],"owner", sep=", "))
  }
  
  # any best-suited?
  if (length(best_match)) {
    idx <- rowSums(df[, ..best_match] != "0" & df[, ..best_match] != "", na.rm=TRUE) > 0
    out[idx] <- ifelse(out[idx]=="", "best suited", paste(out[idx],"best suited", sep=", "))
  }
  
  # assign
  df[, (safe_grp) := out]
}

# 5) Inspect your new 8 columns
df[, gsub("[^A-Za-z0-9]+","_", names(brand_groups)), with=FALSE]



library(data.table)

# Make sure df is a data.table
setDT(df)

# Define the 14 target brands (note: exact text from column names)
brands <- c("Acer", "Apple (Mac)", "Samsung", "Lenovo", "Dell", "ASUS", 
            "Apple", "Self-built", "Alienware", "hp", "Toshiba", "Fujitsu", 
            "MSI", "Razer")

# Get the original columns
aw_cols   <- grep("^PC Brand Awareness_",            names(df), value=TRUE)
own_cols  <- grep("^PC Brand Ownership_",            names(df), value=TRUE)
best_cols <- grep("^Brands Best Suited to Gaming_",  names(df), value=TRUE)

# Extract the brand names (after the prefix)
aw_brands   <- sub("^PC Brand Awareness_", "", aw_cols)
own_brands  <- sub("^PC Brand Ownership_", "", own_cols)
best_brands <- sub("^Brands Best Suited to Gaming_", "", best_cols)

# Loop through each brand
for (brand in brands) {
  # Column-safe brand name
  col_name <- gsub("[^A-Za-z0-9]+", "_", brand)
  
  # Find matching columns
  aw_col   <- aw_cols[aw_brands == brand]
  own_col  <- own_cols[own_brands == brand]
  best_col <- best_cols[best_brands == brand]
  
  # Initialize empty output
  out <- rep("", nrow(df))
  
  # Aware
  if (length(aw_col) == 1) {
    idx <- df[[aw_col]] != "0" & df[[aw_col]] != ""
    out[idx] <- "aware"
  }
  
  # Owner
  if (length(own_col) == 1) {
    idx <- df[[own_col]] != "0" & df[[own_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "owner", paste(out[idx], "owner", sep = ", "))
  }
  
  # Best suited
  if (length(best_col) == 1) {
    idx <- df[[best_col]] != "0" & df[[best_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "best suited", paste(out[idx], "best suited", sep = ", "))
  }
  
  # Assign the new column
  df[[col_name]] <- out
}

# ✅ Optional: show the new columns
new_cols <- gsub("[^A-Za-z0-9]+", "_", brands)
head(df[, ..new_cols])

####################################END PC BRANDS 





##########################COMPONENT BRAND

# Base R approach
periph_cols   <- grep("^Peripheral Brand Awareness_", names(df), value = TRUE)
component_cols<- grep("^Component Brand Awareness_",  names(df), value = TRUE)

print(periph_cols)
print(component_cols)





# 1) Define your group → brand mapping
component_groups <- list(
  `CPU & Motherboards` = c("Intel", "AMD", "ASUS", "MSI", "Gigabyte", "ASRock", "EVGA"),
  `GPU Brands` = c("NVIDIA", "AMD", "MSI", "ASUS", "Gigabyte", "EVGA", "Palit", "PowerColor", 
                   "Sapphire", "ZOTAC", "XFX", "Galax", "Colorful", "INNO3D", "PNY"),
  `Memory & Storage` = c("Samsung", "ADATA", "Crucial", "Kingston", "Patriot", "GeIL", 
                         "G. Skill", "Team Group", "XPG", "Toshiba", "Seagate", 
                         "Western Digital", "Freecom", "Kioxia", "Buffalo Technology"),
  `Cooling & Power` = c("Noctua", "be quiet!", "EKWB", "Thermaltake", "Cooler Master", 
                        "NZXT", "Seasonic", "SilverStone", "Antec", "Phanteks", 
                        "Fractal Design", "Akasa"),
  `Cases` = c("NZXT", "Lian Li", "InWin", "BitFenix", "Jonsbo", "Phanteks", 
              "Fractal Design", "SilverStone", "Antec"),
  `Gaming Sub-brands` = c("AORUS", "ROG - Republic of Gamers", "XPG"),
  `Peripherals & RAM Brands` = c("Corsair", "HyperX", "Zowie"),
  `Controllers & Interfaces` = c("Adaptec", "3Ware", "Promise Technology"),
  `Other / Networking` = c("Cisco", "None of these")
)


# 2) Grab all your Component Brand Awareness columns
comp_cols   <- grep("^Component Brand Awareness_", names(df), value=TRUE)

# 3) Extract the raw brand labels
comp_brands <- sub("^Component Brand Awareness_", "", comp_cols)
comp_brands <- gsub("\\.$", "", comp_brands)

# 4) For each group, count how many of its brands each respondent is aware of
for (grp in names(component_groups)) {
  members <- component_groups[[grp]]
  cols    <- comp_cols[comp_brands %in% members]
  newcol  <- grp
  
  if (length(cols)) {
    # rowSums of non-"0"/non-blank → count how many brands in this group they ticked
    df[, (newcol) := rowSums(.SD != "0" & .SD != "", na.rm=TRUE), .SDcols = cols]
  } else {
    # if your mapping had a group with no matching columns, fill zeros
    df[, (newcol) := 0L]
  }
}

# 5) Inspect your new 10 columns
df[, names(component_groups), with=FALSE]



library(data.table)

# Zorg dat df een data.table is
setDT(df)

# Define de merken die je wilt behouden
keep_brands <- c("Seagate", "MSI", "Gigabyte", "Kingston", "AMD", 
                 "NVIDIA", "Toshiba", "Samsung", "ASUS", "Intel")

# Alle kolommen die beginnen met "Component Brand Awareness_"
all_aw_cols <- grep("^Component Brand Awareness_", names(df), value = TRUE)

# Extract de merknamen (na de '_')
brand_names <- sub("^Component Brand Awareness_", "", all_aw_cols)

# Selecteer de kolommen die eindigen op de juiste merken
keep_cols <- all_aw_cols[brand_names %in% keep_brands]

# Bepaal welke kolommen je wil verwijderen
drop_cols <- setdiff(all_aw_cols, keep_cols)

# Verwijder de overige kolommen
df[, (drop_cols) := NULL]










######################"PERIPHERAL BRANDS 

# 1) Define your 9 peripheral‐brand groups
periph_groups <- list(
  `General PC Brands` = c(
    "ASUS", "Gigabyte", "HP", "MSI", "Acer", "Dell", "Lenovo", "Samsung", "MSI"
  ),
  
  `Gaming Sub-brands` = c(
    "Alienware", "AORUS", "Legion", "OMEN", "Predator", "ROG - Republic of Gamers"
  ),
  
  `Mainstream Peripherals` = c(
    "Logitech", "Corsair", "Cooler Master", "HyperX", "Sharkoon", "Cougar"
  ),
  
  `Premium / Esports Gear` = c(
    "Logitech G", "Razer", "ROCCAT", "SteelSeries", "Glorious", "Xtrfy", "Zowie",
    "Fnatic", "Endgame Gear", "Millenium", "Final Mouse", "Mountain", "Mionix", "GFallen", "Mad Catz"
  ),
  
  `Budget Peripherals` = c(
    "Redragon", "Cosmic Byte", "Circle Gaming", "Red Gear", 
    "Multilaser", "Warrior", "iBall", "Zebronic", "Trust Gaming"
  ),
  
  `Audio Brands` = c(
    "Sennheiser", "Bose", "JBL", "EPOS", "TEAC", "Jaybird"
  ),
  
  `Streaming / Accessories` = c(
    "Elgato", "Nacon", "SCUF", "Das Keyboard", "Ducky", "Dornfinger"
  ),
  
  `Gaming Headsets` = c(
    "Astro", "Turtlebeach", "RIG"
  ),
  
  `None / Other` = c("None of these")
)


# 2) Grab all Peripheral Brand Awareness columns
aw_cols   <- grep("^Peripheral Brand Awareness_", names(df), value=TRUE)

# 3) Extract just the brand labels
aw_brands <- sub("^Peripheral Brand Awareness_", "", aw_cols)
aw_brands <- gsub("\\.$","", aw_brands)  # remove any trailing “.”

# 4) For each group, count how many of its brands each respondent is aware of
for (grp in names(periph_groups)) {
  members <- periph_groups[[grp]]
  cols    <- aw_cols[aw_brands %in% members]
  newcol  <- grp
  
  if (length(cols) > 0) {
    # Count non‐"0" / non‐empty entries in those columns
    df[, (newcol) := rowSums(.SD != "0" & .SD != "", na.rm=TRUE), .SDcols = cols]
  } else {
    # If no columns matched, fill with zeros
    df[, (newcol) := 0L]
  }
}

# 5) Inspect the 9 new summary columns
df[, names(periph_groups), with=FALSE]





library(data.table)

# Make sure df is a data.table
setDT(df)

# Define the brand names you want to keep
keep_brands <- c("Bose", "Razer", "Samsung", "Logitech G", "Logitech", 
                 "HP", "Dell", "Acer", "ASUS", "Lenovo")

# Find all Peripheral Brand Awareness_ columns
all_periph_cols <- grep("^Peripheral Brand Awareness_", names(df), value = TRUE)

# Extract the brand names from the column names
brand_names <- sub("^Peripheral Brand Awareness_", "", all_periph_cols)

# Select only columns that match the brands you want
keep_cols <- all_periph_cols[brand_names %in% keep_brands]

# Identify columns to remove
drop_cols <- setdiff(all_periph_cols, keep_cols)

# Drop them
df[, (drop_cols) := NULL]


############################ENDPERIPHERAL BRANDS















component_cols<- grep("^gwi-siz-fce.q65_",  names(df), value = TRUE)

print(component_cols)



library(data.table)
setDT(df)
library(data.table)
setDT(df)

# Mapping: q65 column ID → Brand
brand_map <- list(
  "Acer" = 1, "Alienware" = 2, "AORUS" = 3, "Astro" = 4, "ASUS" = 5, "Bose" = 6,
  "Circle Gaming" = 7, "Cooler Master" = 8, "Corsair" = 9, "Cosmic Byte" = 10,
  "Cougar" = 11, "Das Keyboard" = 12, "Dell" = 13, "Dornfinger" = 14, "Ducky" = 15,
  "Elgato" = 16, "Endgame Gear" = 17, "EPOS" = 18, "Final Mouse" = 19, "Fnatic" = 20,
  "GFallen" = 21, "Gigabyte" = 22, "Glorious" = 23, "HP" = 24, "HyperX" = 25,
  "iBall" = 26, "Jaybird" = 27, "JBL" = 28, "Legion" = 29, "Lenovo" = 30,
  "Logitech" = 31, "Logitech G" = 32, "Mad Catz" = 33, "Millenium" = 34,
  "Mionix" = 35, "Mountain" = 36, "MSI" = 37, "Multilaser" = 38, "Nacon" = 39,
  "OMEN" = 40, "Predator" = 41, "Razer" = 42, "Red Gear" = 43, "Redragon" = 44,
  "RIG" = 45, "ROCCAT" = 46, "ROG - Republic of Gamers" = 47, "Samsung" = 48,
  "SCUF" = 49, "Sennheiser" = 50, "Sharkoon" = 51, "SteelSeries" = 52, "TEAC" = 53,
  "Trust Gaming" = 54, "Turtlebeach" = 55, "Warrior" = 56, "Xtrfy" = 57,
  "Zebronic" = 58, "Zowie" = 59, "Intel" = 60, "AMD" = 61, "NVIDIA" = 62,
  "EVGA" = 63, "XPG" = 64, "NZXT" = 65, "Palit" = 66, "ADATA" = 67, "Crucial" = 68,
  "Kingston" = 69, "Patriot" = 70, "Sapphire" = 71, "be quiet!" = 72, "Noctua" = 73,
  "PNY" = 74, "Seagate" = 75, "Western Digital" = 76, "Freecom" = 77,
  "Buffalo Technology" = 78, "Adaptec" = 79, "3Ware" = 80, "Promise Technology" = 81,
  "EKWB" = 82, "Antec" = 83, "Seasonic" = 84, "ASRock" = 85, "GeIL" = 86,
  "G. Skill" = 87, "Team Group" = 88, "ZOTAC" = 89, "XFX" = 90, "PowerColor" = 91,
  "Akasa" = 92, "INNO3D" = 93, "Jonsbo" = 94, "Toshiba" = 95, "Kioxia" = 96,
  "BitFenix" = 97, "Thermaltake" = 98, "SilverStone" = 99, "Phanteks" = 100,
  "Fractal Design" = 101, "Lian Li" = 102, "InWin" = 103, "Galax" = 104,
  "Colorful" = 105, "Cisco" = 106
)

# Grouping: brand name → category
group_map <- list(
  `General PC & Peripherals` = c("Acer","ASUS","Dell","Gigabyte","HP","Lenovo","MSI","Samsung"),
  `Gaming Sub-brands` = c("Alienware","AORUS","Legion","OMEN","Predator","ROG - Republic of Gamers"),
  `Mainstream Peripherals` = c("Cooler Master","Corsair","HyperX","Logitech","Logitech G","Cougar","Ducky"),
  `Esports / Pro Peripherals` = c("Fnatic","Glorious","Mad Catz","Millenium","Mionix","Redragon",
                                  "ROCCAT","Razer","SteelSeries","Xtrfy","Zowie","Endgame Gear",
                                  "GFallen","Final Mouse","Mountain"),
  `Budget / Entry Peripherals` = c("Circle Gaming","Cosmic Byte","iBall","Multilaser","Red Gear",
                                   "Trust Gaming","Warrior","Zebronic", "Sharkoon"),
  `Audio / Headsets` = c("Astro","Bose","EPOS","JBL","Jaybird","Sennheiser","TEAC","Turtlebeach", "RIG"),
  `Controllers & Streaming` = c("Elgato","Nacon","SCUF"),
  `Storage & Memory` = c("ADATA","Crucial","Kingston","Kioxia","Patriot","PNY","Seagate",
                         "Team Group","Western Digital","XPG","Freecom","Buffalo Technology","Toshiba"),
  `GPU Manufacturers` = c("AMD","EVGA","Intel","NVIDIA","Palit","PowerColor","Sapphire",
                          "ZOTAC","XFX","INNO3D","Galax","Colorful"),
  `Cooling & Power Supplies` = c("Antec","be quiet!","EKWB","Noctua","Phanteks","Seasonic",
                                 "SilverStone","Thermaltake", "Akasa"),
  `Motherboards & Components` = c("ASRock","G. Skill","GeIL"),
  `Cases & Chassis` = c("BitFenix","Fractal Design","Jonsbo","Lian Li","InWin","NZXT"),
  `Storage Controllers / Enterprise` = c("Adaptec","3Ware","Promise Technology", "Cisco"),
  `Unknown / Other` = c("Das Keyboard","Dornfinger")
)

# Flatten to brand → group
brand_to_group <- data.table(
  brand = unlist(group_map),
  group = rep(names(group_map), lengths(group_map))
)

# Convert brand_map to ID → brand
id_to_brand <- data.table(
  id = unlist(brand_map),
  brand = rep(names(brand_map), lengths(brand_map))
)

# Join brand → group
id_to_group <- merge(id_to_brand, brand_to_group, by = "brand", all.x = TRUE)

# View missing group assignments if any
print(id_to_group[is.na(group)])  # should be empty


# Get all column names matching q65_ followed by a number
q65_cols <- grep("^q65_[0-9]+$", names(df), value = TRUE)

# Extract numeric suffix (e.g., from q65_60 get 60) to map to brand IDs
q65_ids <- as.integer(sub(".*q65_([0-9]+)$", "\\1", q65_cols))

# Standardize input: remove whitespace and convert to lowercase (so "Confident" → "confident")
df[ , (q65_cols) := lapply(.SD, function(x) {
  xx <- trimws(as.character(x))
  tolower(xx)
}), .SDcols = q65_cols ]


# Define all valid confidence levels to summarize
levels <- c("very unconfident", "unconfident", "neutral", "confident", "very confident")

# Loop over each confidence level
for (lvl in levels) {
  newcol <- lvl
  
  # Count how many times each group received this confidence level
  count_dt <- lapply(component_groups, function(idxs) {
    cols <- q65_cols[q65_ids %in% idxs]
    if (length(cols)) {
      rowSums(df[, ..cols] == lvl, na.rm = TRUE)
    } else {
      integer(nrow(df))
    }
  })
  count_dt <- as.data.table(count_dt)
  
  # Collapse results into string like "GPU_Chipsets: 1, Motherboards: 2"
  summary_vec <- apply(count_dt, 1, function(r) {
    nz <- which(r > 0)
    if (!length(nz)) return(NA_character_)
    paste0(names(r)[nz], ":", r[nz], collapse = ", ")
  })
  
  # Save result in new column (e.g., "confident", "neutral", etc.)
  df[, (newcol) := summary_vec]
}


# Show the 5 new summary columns
df[, levels, with = FALSE]








########################CONSUMPTION



# 1) Deduplicate your original columns
names(df) <- make.unique(names(df), sep=".")

# 2) Grab the now‐unique names
cons_cols  <- grep("^Consumption Habits During Gaming", names(df), value=TRUE)
food_cols  <- grep("^Food Categories During Gaming",       names(df), value=TRUE)
drink_cols <- grep("^Drink Categories During Gaming",      names(df), value=TRUE)

# 3) Context labels in the order your columns appear
contexts <- c("while playing", "while waiting", "while watching")

# 4) The 16 behaviors / categories
beh_cons  <- c("Eat_food", "Drink_soft_drinks", "Drink_alcohol", "None_of_these")
beh_food  <- c("Snacks", "Sweets_Candy", "Healthy_snacks", "Full_home_cooked_meals",
               "Takeaways_Home_delivery", "Other_foods")
beh_drink <- c("Energy_drinks", "Soda_Fizzy_drinks", "Water_Flavoured_water",
               "Juices", "Hot_drinks", "Other_drinks")

# 5) A-by-reference helper to build the summaries
make_summary <- function(df, cols_block, behaviors) {
  n_beh <- length(behaviors)
  stopifnot(length(cols_block) == n_beh * length(contexts))
  
  for (i in seq_len(n_beh)) {
    beh <- behaviors[i]
    # start with an empty character vector
    out <- character(nrow(df))
    
    # for each context
    for (j in seq_along(contexts)) {
      ctx <- contexts[j]
      col <- cols_block[(j-1)*n_beh + i]
      sel <- df[[col]] != "0" & df[[col]] != "" & !is.na(df[[col]])
      out[sel] <- paste0(out[sel],
                         ifelse(out[sel]=="", "", ", "),
                         ctx)
    }
    
    out[out==""] <- NA_character_
    # **here** assign by reference
    df[ , (beh) := out ]
  }
}

# 6) Build the 4 consumption‐habits summaries
make_summary(df, cons_cols,  beh_cons)

# 7) Build the 6 food‐category summaries
make_summary(df, food_cols,  beh_food)

# 8) Build the 6 drink‐category summaries
make_summary(df, drink_cols, beh_drink)

# 9) Verify your 16 new columns exist and hold strings like "while playing, while waiting"
new_cols <- c(beh_cons, beh_food, beh_drink)
df[ , ..new_cols ]


#################################END Consumption


###############################BETTING

# 1) Find the raw column names
bet_cols <- grep("^Categories Betted On_", names(df), value=TRUE)
reg_cols <- grep("^Categories Regularly Betted On_", names(df), value=TRUE)

# 2) Define your six categories and a safe new‐column name
categories <- c(
  "Esports",
  "Sports",
  "Current affairs",
  "Television",
  "Other (please specify)",
  "None of these"
)
new_names <- c(
  "Esports",
  "Sports",
  "Current_affairs",
  "Television",
  "Other",
  "None_of_these"
)

# 3) Loop over each category
for (i in seq_along(categories)) {
  cat_label <- categories[i]
  new_col   <- new_names[i]
  
  # find the exact columns (they exist once in bet_cols, once in reg_cols)
  bet_col <- grep(paste0("^Categories Betted On_", 
                         gsub("\\(", "\\\\(", cat_label),
                         "$"),
                  bet_cols, value=TRUE)
  
  reg_col <- grep(paste0("^Categories Regularly Betted On_", 
                         gsub("\\(", "\\\\(", cat_label),
                         "\\.?$"),
                  reg_cols, value=TRUE)
  
  # safety: if reg_col doesn’t exist (only for “Other”), treat as always FALSE
  has_reg <- length(reg_col)==1
  
  # 4) Create the summary by reference
  df[ , (new_col) := {
    # logical vectors for bet / reg
    b <- !is.na(get(bet_col)) & get(bet_col) != "0" & get(bet_col) != ""
    if (has_reg) {
      r <- !is.na(get(reg_col)) & get(reg_col) != "0" & get(reg_col) != ""
    } else {
      r <- rep(FALSE, .N)
    }
    # build the text outcome
    out <- rep(NA_character_, .N)
    out[b & r]   <- "betted on, regularly betted on"
    out[b &!r]   <- "betted on"
    out[!b & r]  <- "regularly betted on"
    out
  }]
}

# 5) Quick peek
df[ , .SD, .SDcols=new_names ]








component_cols<- grep("^Esports Title Betting Preferences_",  names(df), value = TRUE)

print(component_cols)





library(data.table)

# 1) Load the same mapping you used for “Watched Ever”
mapping <- fread("C:/Users/mira.leenders/Downloads/Final_Esports_Watched_Ever_Mapping.csv")

# 2) Grab all your “Esports Title Betting Preferences_…” columns
pref_cols <- grep("^Esports Title Betting Preferences_", names(df), value = TRUE)

# 3) Build a small helper table to extract the title after the underscore
pref_dt <- data.table(column = pref_cols)
pref_dt[, Original_Esport_Title := sub(
  "^Esports Title Betting Preferences_",
  "",
  column
)]

# 4) Join in the grouping from your mapping file
#    (mapping must have columns “Original Esport Title” and “Grouped Genre / Category”)
setnames(mapping,
         old = "Original Esport Title",
         new = "Original_Esport_Title")
setnames(mapping,
         old = "Grouped Genre / Category",
         new = "Genre")
pref_dt <- merge(pref_dt,
                 mapping[, .(Original_Esport_Title, Genre)],
                 by = "Original_Esport_Title",
                 all.x = TRUE)

# 5) For each group, count how many titles in that group the respondent indicated
for (g in unique(pref_dt$Genre)) {
  # 5a) All columns belonging to this group
  cols_for_group <- pref_dt[Genre == g, column]
  # 5b) Make a safe name (underscores instead of spaces/slashes)
  safe_name <- gsub("[ /]", "_", g)
  new_col   <- paste0("BettingPref_", safe_name)
  
  # 5c) Add the summary column: a simple row‐sum of non‐zero, non‐blank picks
  df[ , (new_col) := rowSums(
    .SD != "0" & .SD != "",
    na.rm = TRUE
  ), .SDcols = cols_for_group ]
}

# 6) Inspect your new columns
new_cols <- grep("^BettingPref_", names(df), value = TRUE)
df[ , ..new_cols ]


Freq_select_cols <- grep("^Team Awareness_", names(df), value = TRUE)
freq_list <- lapply(Freq_select_cols, function(col) {
  tbl <- as.data.frame(table(df[[col]], useNA = "ifany"))
  names(tbl) <- c("Value", "Frequency")
  tbl$Column <- col
  tbl
})
freq_table <- rbindlist(freq_list)
# View(freq_table)





library(data.table)

# Make sure df is a data.table
setDT(df)

# Titles you want to keep (case-sensitive exact matches)
keep_titles <- c("Mortal Kombat 11", "Call Of Duty Warzone", "PUBG", "Dota2",
                 "League of Legends: Wild Rift", "Call of Duty Mobile",
                 "League of Legends", "Portnite", "Call of Duty", "FIFA")

# Find all relevant columns
all_bet_cols <- grep("^Esports Title Betting Preferences_", names(df), value = TRUE)

# Extract title part (after the last underscore)
titles <- sub("^Esports Title Betting Preferences_", "", all_bet_cols)

# Get matching column names
keep_cols <- all_bet_cols[titles %in% keep_titles]

# Drop all others
drop_cols <- setdiff(all_bet_cols, keep_cols)
df[, (drop_cols) := NULL]


##############################END OF BETTING






component_cols<- grep("^Team Awareness_",  names(df), value = TRUE)

print(component_cols)

# 1) Identify your Team Awareness columns
team_cols <- grep("^Team Awareness_", names(df), value = TRUE)

# 2) For each column, tabulate its values
freq_list <- lapply(team_cols, function(col) {
  # compute frequencies, including NA
  dt <- df[, .(Frequency = .N), by = .(Value = get(col))]
  dt[, Column := col]
  setcolorder(dt, c("Column","Value","Frequency"))
  dt
})

# 3) Stack them back together
freq_table <- rbindlist(freq_list)

# 4) Inspect or export
print(freq_table)
# fwrite(freq_table, "team_awareness_frequencies.csv")





# 1) All your “Team Awareness_…” columns
team_cols <- grep("^Team Awareness_", names(df), value = TRUE)

# 2) Which ones *not* to group as “other”
keep <- c(
  "Team Awareness_Alliance",
  "Team Awareness_Cloud9",  
  "Team Awareness_Excel Esports", "Team Awareness_Fnatic",
  "Team Awareness_G2 Esports","Team Awareness_Nova Esports",
  "Team Awareness_PSG",            "Team Awareness_Red Bull",
  "Team Awareness_Team Liquid",  "Team Awareness_Team Vitality",  "Team Awareness_None of these"
)

# 3) Everything else is “other”
other_cols <- setdiff(team_cols, keep)

# 4) Count per‐row how many “other” teams they’re aware of
df[ , other_awareness := rowSums(
  .SD != "0" & .SD != "",
  na.rm = TRUE
), .SDcols = other_cols
]

# Quick sanity check
df[ , .(other_awareness)][ , summary(other_awareness) ]


# Define the ones you want to keep
keep <- c(
  "Team Awareness_Alliance",
  "Team Awareness_Cloud9",  
  "Team Awareness_Excel Esports", "Team Awareness_Fnatic",
  "Team Awareness_G2 Esports","Team Awareness_Nova Esports",
  "Team Awareness_PSG", "Team Awareness_Red Bull",
  "Team Awareness_Team Liquid", "Team Awareness_Team Vitality",
  "Team Awareness_None of these"
)

# Identify all columns starting with "Team Awareness_"
all_team_cols <- grep("^Team Awareness_", names(df), value = TRUE)

# Identify which ones to drop (all minus keepers)
cols_to_drop <- setdiff(all_team_cols, keep)

# Remove them
df[, (cols_to_drop) := NULL]












###########################################ESPORTS TOURNAMENTS
component_cols<- grep("^Specific Esports Tournaments Watched_",  names(df), value = TRUE)

print(component_cols)



Freq_select_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)
freq_list <- lapply(Freq_select_cols, function(col) {
  tbl <- as.data.frame(table(df[[col]], useNA = "ifany"))
  names(tbl) <- c("Value", "Frequency")
  tbl$Column <- col
  tbl
})
freq_table <- rbindlist(freq_list)
# View(freq_table)


library(data.table)
library(readxl)

# 1. Laad je originele dataframe
# df <- fread("pad/naar/jouw/data.csv") # of hoe jij het ook inleest
df <- as.data.table(df)  # zorg dat df een data.table is

# 2. Mappingbestand inlezen
mapping <- read_excel("C:/Users/mira.leenders/Downloads/Esports_Tournament_Grouping_FILLED_CORRECT.xlsx")
mapping <- as.data.table(mapping)

# 3. Maak een helperdata.table met toernooikolommen in df
tourn_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)
tourn_dt <- data.table(column = tourn_cols)

# 4. Voeg mapping toe op basis van de kolomnamen
setnames(mapping, old = "Esports Tournament Column Name", new = "column")
tourn_dt <- merge(tourn_dt, mapping, by = "column", all.x = TRUE)

# 5. Voor elke groep: maak een nieuwe kolom in df met het aantal bekeken toernooien uit die groep
for (g in unique(tourn_dt$`Grouped Category`)) {
  group_cols <- tourn_dt[`Grouped Category` == g, column]
  
  # Maak veilige kolomnaam (spaties en speciale tekens → underscores)
  safe_group_name <- gsub("[^A-Za-z0-9]", "_", g)
  new_col <- paste0("Watched_", safe_group_name)
  
  # Voeg kolom toe aan df
  df[[new_col]] <- rowSums(
    as.data.frame(lapply(df[, ..group_cols], function(x) x != "0" & x != "" & !is.na(x))),
    na.rm = TRUE
  )
}

# 6. Optioneel: print een voorbeeld
watched_group_cols <- grep("^Watched_", names(df), value = TRUE)
print(df[, ..watched_group_cols])










library(data.table)
library(readxl)

# 1. Zorg dat je originele dataframe beschikbaar is
# Bijvoorbeeld: df <- fread("path/to/your/data.csv")
df <- as.data.table(df)  # zorg dat df een data.table is

# 2. Lees het mappingbestand in (pas pad aan!)
mapping <- read_excel("C:/Users/mira.leenders/Downloads/Esports_Tournament_Grouping_STRUCTURE_BASED.xlsx")
mapping <- as.data.table(mapping)

# 3. Vind alle kolommen over bekeken toernooien
tourn_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)

# 4. Maak helper-tabel met kolomnamen
tourn_dt <- data.table(column = tourn_cols)

# 5. Merge mapping in op basis van kolomnamen
setnames(mapping, old = "Esports Tournament Column Name", new = "column")
tourn_dt <- merge(tourn_dt, mapping, by = "column", all.x = TRUE)

# 6. Voor elke structure-based groep: maak een nieuwe kolom met het aantal bekeken toernooien
for (g in unique(tourn_dt$`Structure-Based Grouping`)) {
  group_cols <- tourn_dt[`Structure-Based Grouping` == g, column]
  
  # Veilige kolomnaam genereren
  safe_group_name <- gsub("[^A-Za-z0-9]", "_", g)
  new_col <- paste0("Watched_", safe_group_name)
  
  # Voeg kolom toe aan df
  df[[new_col]] <- rowSums(
    as.data.frame(lapply(df[, ..group_cols], function(x) x != "0" & x != "" & !is.na(x))),
    na.rm = TRUE
  )
}


keep_tournaments <- c(
  "FIFAe World Cup 2023",
  "EA Sports Cup",
  "Call of Duty League 2023: Playoffs",
  "FNCS 2023 Global Championship",
  "2023 World Championship [Worlds 2023 ]",
  "Call of Duty League 2023: Stage 3 Major",
  "Call of Duty League 2023: Stage 4 Major",
  "Call of Duty League 2023: Stage 2 Major",
  "EA Champions Cup Spring 2023",
  "Gamers8 2023",
  "None of these"
)
library(data.table)

# Zorg dat df een data.table is
setDT(df)

# Definieer alle kolommen die starten met het juiste patroon
all_tournament_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)

# Extract het deel na de underscore
tournament_names <- sub("^Specific Esports Tournaments Watched_", "", all_tournament_cols)

# Selecteer de kolommen die overeenkomen met de opgegeven toernooien
keep_cols <- all_tournament_cols[tournament_names %in% keep_tournaments]

# Bepaal welke kolommen verwijderd moeten worden
drop_cols <- setdiff(all_tournament_cols, keep_cols)

# Verwijder de ongewenste kolommen
df[, (drop_cols) := NULL]




##############################ESPORTS TOURNAMENTS










################################REASONS FOR SUPPORT
reason_cols <- grep("^Reasons for Supporting Specific Esports Teams_", names(df), value = TRUE)
print(reason_cols)


library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# 1) Define your 12 reasons in order
reasons <- c(
  "I am only a fan of a particular player on the team",
  "I like the team because of their performances in a particular competition/game",
  "I like the attitude of the players on the team",
  "I like the influencers or brand ambassadors that are part of the team",
  "I was originally only a fan of a particular player, but now I am also a fan of the team",
  "I like that the team represents me in some way",
  "I like the team's overall branding, values, and messaging",
  "I was influenced by family or friends to become a fan of the team",
  "I like the team's community and what they do for their community",
  "I am a fan of the team's content (video, web, social media)",
  "I like the story or narrative behind the team",
  "None of these"
)

# 2) Your “top” teams
top_teams <- c(
  "Cloud9", "Excel Esports", "Fnatic",
  "G2 Esports", "PSG", "Red Bull",
  "T1", "Team Liquid", "Infinity", "FaZe Clan"
)

# 3) Grab the reason‐columns
reason_cols <- grep(
  "^Reasons for Supporting Specific Esports Teams_",
  names(df), value = TRUE
)

# 4) Build lookup of col → team → reason_index
col_info <- tibble(colname = reason_cols) %>%
  extract(
    col     = colname,
    into    = c("team", "suffix"),
    regex   = "^Reasons for Supporting Specific Esports Teams_(.*?)(?:_dup_(\\d+))?$",
    remove  = FALSE,
    convert = TRUE
  ) %>%
  mutate(
    reason_index = if_else(is.na(suffix), 1L, as.integer(suffix) + 1L)
  ) %>%
  select(colname, team, reason_index)


# 5) Pivot long, filter, tag top vs other, aggregate per hash×reason
df_reasons <- df %>%
  select(hash, all_of(col_info$colname)) %>%
  pivot_longer(
    cols      = -hash,
    names_to  = "colname",
    values_to = "val"
  ) %>%
  filter(!is.na(val), val != "", val != "0") %>%
  left_join(col_info, by = "colname") %>%
  mutate(is_top = team %in% top_teams) %>%
  group_by(hash, reason_index) %>%
  summarise(
    top_list    = list(unique(team[is_top])),
    other_count = sum(!is_top),
    .groups     = "drop"
  ) %>%
  mutate(
    reason_str = map2_chr(top_list, other_count, ~{
      teams <- .x
      if (.y > 0) teams <- c(teams, paste0("Other: ", .y))
      if (length(teams)) str_c(teams, collapse = ", ") else NA_character_
    })
  ) %>%
  select(hash, reason_index, reason_str) %>%
  pivot_wider(
    names_from   = reason_index,
    values_from  = reason_str,
    names_prefix = "R"
  )

# 6) Rename R1…R12 to your actual reason text
colnames(df_reasons)[-1] <- reasons

# 7) Join back onto original df
df <- df %>%
  left_join(df_reasons, by = "hash")

#########################################REASONS FOR SUPPORT








library(data.table)
setDT(df)

# Remove the prefix from all column names
setnames(df, old = names(df), new = sub("^gwi-siz-fce\\.", "", names(df)))

setnames(df, old = c("q2", "q3"), new = c("gender", "age"))
setnames(df, old = c("q4", "q7"), new = c("gaming freq.", "main gaming device"))
setnames(df, old = c("q8_1", "q8_2"), new = c("play time weekday", "play time weekend"))
setnames(df, old = c("q11"), new = c("play time with others"))
df <- df %>%
  rename(
    Purch_Freq_NewVideoGames = `q12_1`,
    Purch_Freq_Subscr_GamingServices = `q12_2`,
    Purch_Freq_Donations_Subscr_Streamers_ContentCreators = `q12_3`, 
    Purch_Freq_PlayableCharacters  = `q12_4`, 
    Purch_Freq_InGameCurrency  = `q12_5`, 
    Purch_Freq_PayToWin  = `q12_6`, 
    Purch_Freq_LootBoxes_Packs  = `q12_7`, 
    Purch_Freq_CosmeticSkins  = `q12_8`, 
    Purch_Freq_Expansions  = `q12_9`
  )




######################################SPENDING CATEGORY
library(data.table)

# Zorg dat df een data.table is
setDT(df)

# 1. Bepaal alle q13-prefixed kolommen (zonder prefix)
prefixes <- c("q13", paste0("q13", letters[2:11]))  # q13, q13b, ..., q13k
all_q13_cols <- paste0(rep(prefixes, each = 4), "_", rep(1:4, times = length(prefixes)))
all_q13_cols <- intersect(all_q13_cols, names(df))  # alleen kolommen die bestaan

# 2. Zet "0", "", of "NA" om naar echte NA
df[, (all_q13_cols) := lapply(.SD, function(x) {
  x <- as.character(x)
  x[x %in% c("0", "", "NA")] <- NA_character_
  return(x)
}), .SDcols = all_q13_cols]

# 3. Mapping van waarden naar spending-categorieën
mapping <- c(
  # UK (GBP)
  "Less than PS20" = "lowest spending category",
  "PS20 - PS29" = "2nd lowest spending category",
  "PS30 - PS49" = "3rd lowest spending category",
  "PS50 - PS69" = "4rth lowest spending category",
  "PS70 - PS99" = "middle spending category",
  "PS100 - PS149" = "4rth highest spending category",
  "PS150 - PS199" = "3rd highest spending category",
  "PS200 - PS299" = "2nd highest spending category",
  "Over PS300" = "highest spending category",
  # Australia (AUD)
  "Less than 40 AUD" = "lowest spending category",
  "40 - 59 AUD" = "2nd lowest spending category",
  "60 - 89 AUD" = "3rd lowest spending category",
  "90 - 129 AUD" = "4rth lowest spending category",
  "130 - 189 AUD" = "middle spending category",
  "190 - 279 AUD" = "4rth highest spending category",
  "280 - 369 AUD" = "3rd highest spending category",
  "370 - 560 AUD" = "2nd highest spending category",
  "Over 560 AUD" = "highest spending category",
  # Europe (EUR)
  "Less than 25 EUR" = "lowest spending category",
  "25 - 34 EUR" = "2nd lowest spending category",
  "35 - 54 EUR" = "3rd lowest spending category",
  "55 - 79 EUR" = "4rth lowest spending category",
  "80 - 114 EUR" = "middle spending category",
  "115 - 169 EUR" = "4rth highest spending category",
  "170 - 229 EUR" = "3rd highest spending category",
  "230 - 345 EUR" = "2nd highest spending category",
  "Over 345 EUR" = "highest spending category",
  # Brazil (BRL)
  "Less than 125 BRL" = "lowest spending category",
  "125 - 179 BRL" = "2nd lowest spending category",
  "180 - 299 BRL" = "3rd lowest spending category",
  "300 - 424 BRL" = "4rth lowest spending category",
  "425 - 609 BRL" = "middle spending category",
  "610 - 919 BRL" = "4rth highest spending category",
  "920 - 1229 BRL" = "3rd highest spending category",
  "1230 - 1845 BRL" = "2nd highest spending category",
  "Over 1845 BRL" = "highest spending category",
  # Canada (CAD)
  "Less than 35 CAD" = "lowest spending category",
  "35 - 49 CAD" = "2nd lowest spending category",
  "50 - 79 CAD" = "3rd lowest spending category",
  "80 - 114 CAD" = "4rth lowest spending category",
  "115 - 169 CAD" = "middle spending category",
  "170 - 249 CAD" = "4rth highest spending category",
  "250 - 339 CAD" = "3rd highest spending category",
  "340 - 505 CAD" = "2nd highest spending category",
  "Over 505 CAD" = "highest spending category",
  # Chile (CLP)
  "Less than 22,000 CLP" = "lowest spending category",
  "22,000 - 31,999 CLP" = "2nd lowest spending category",
  "32,000 - 53,999 CLP" = "3rd lowest spending category",
  "54,000 - 75,999 CLP" = "4rth lowest spending category",
  "76,000 - 108,999 CLP" = "middle spending category",
  "109,000 - 163,999 CLP" = "4rth highest spending category",
  "164,000 - 218,999 CLP" = "3rd highest spending category",
  "219,000 - 329,000 CLP" = "2nd highest spending category",
  "Over 329,000 CLP" = "highest spending category",
  # Colombia (COP)
  "Less than 100,000 COP" = "lowest spending category",
  "100,000 - 144,999 COP" = "2nd lowest spending category",
  "145,000 - 249,999 COP" = "3rd lowest spending category",
  "250,000 - 349,999 COP" = "4rth lowest spending category",
  "350,000 - 499,999 COP" = "middle spending category",
  "500,000 - 749,999 COP" = "4rth highest spending category",
  "750,000 - 999,999 COP" = "3rd highest spending category",
  "1,000,000 - 1,500,000 COP" = "2nd highest spending category",
  "Over 1,500,000 COP" = "highest spending category",
  # Mexico (MXN)
  "Less than 430 MXN" = "lowest spending category",
  "430 - 619 MXN" = "2nd lowest spending category",
  "620 - 1,059 MXN" = "3rd lowest spending category",
  "1,060 - 1,499 MXN" = "4rth lowest spending category",
  "1,500 - 2,099 MXN" = "middle spending category",
  "2,100 - 3,199 MXN" = "4rth highest spending category",
  "3,200 - 4,299 MXN" = "3rd highest spending category",
  "4,300 - 6,400 MXN" = "2nd highest spending category",
  "Over 6,400 MXN" = "highest spending category",
  # Peru (PEN)
  "Less than 95 PEN" = "lowest spending category",
  "95 - 139 PEN" = "2nd lowest spending category",
  "140 - 229 PEN" = "3rd lowest spending category",
  "230 - 319 PEN" = "4rth lowest spending category",
  "320 - 459 PEN" = "middle spending category",
  "460 - 699 PEN" = "4rth highest spending category",
  "700 - 929 PEN" = "3rd highest spending category",
  "930 - 1,400 PEN" = "2nd highest spending category",
  "Over 1,400 PEN" = "highest spending category",
  # USA (USD)
  "Less than 25 USD" = "lowest spending category",
  "25 - 34 USD" = "2nd lowest spending category",
  "35 - 59 USD" = "3rd lowest spending category",
  "60 - 89 USD" = "4rth lowest spending category",
  "90 - 129 USD" = "middle spending category",
  "130 - 189 USD" = "4rth highest spending category",
  "190 - 249 USD" = "3rd highest spending category",
  "250 - 380 USD" = "2nd highest spending category",
  "Over 380 USD" = "highest spending category",
  # Vietnam (VND)
  "Less than 600,000 VND" = "lowest spending category",
  "600,000 - 899,999 VND" = "2nd lowest spending category",
  "900,000 - 1,499,999 VND" = "3rd lowest spending category",
  "1,500,000 - 2,099,999 VND" = "4rth lowest spending category",
  "2,100,000 - 3,099,000 VND" = "middle spending category",
  "3,100,000 - 4,599,999 VND" = "4rth highest spending category",
  "4,600,000 - 6,199,999 VND" = "3rd highest spending category",
  "6,200,000 - 9,300,000 VND" = "2nd highest spending category",
  "Over 9,300,000 VND" = "highest spending category"
)

# 4. Categoriseer per type besteding
cols_topic1 <- intersect(paste0(prefixes, "_1"), names(df))
cols_topic2 <- intersect(paste0(prefixes, "_2"), names(df))
cols_topic3 <- intersect(paste0(prefixes, "_3"), names(df))
cols_topic4 <- intersect(paste0(prefixes, "_4"), names(df))

df[, new_games_spend_cat := mapping[
  Reduce(function(x, y) fifelse(is.na(x) | x == "" | x == "0", y, x), .SD)
], .SDcols = cols_topic1]

df[, gaming_subs_spend_cat := mapping[
  Reduce(function(x, y) fifelse(is.na(x) | x == "" | x == "0", y, x), .SD)
], .SDcols = cols_topic2]

df[, streamer_donations_spend_cat := mapping[
  Reduce(function(x, y) fifelse(is.na(x) | x == "" | x == "0", y, x), .SD)
], .SDcols = cols_topic3]

df[, in_game_purchases_spend_cat := mapping[
  Reduce(function(x, y) fifelse(is.na(x) | x == "" | x == "0", y, x), .SD)
], .SDcols = cols_topic4]

# 5. Controle op ongecodeerde waarden
alle_waarden <- unlist(df[, c(cols_topic1, cols_topic2, cols_topic3, cols_topic4), with=FALSE])
ongecodeerd <- setdiff(unique(alle_waarden[!is.na(alle_waarden)]), names(mapping))
if (length(ongecodeerd) > 0) {
  warning("Onbekende categorieën aangetroffen die niet zijn omgezet: ", paste(ongecodeerd, collapse = ", "))
}

# 6. Controle op meerdere waarden per respondent binnen 1 onderwerp
for (cols in list(cols_topic1, cols_topic2, cols_topic3, cols_topic4)) {
  mult_vals <- df[, rowSums(!is.na(.SD)), .SDcols = cols]
  if (any(mult_vals > 1, na.rm = TRUE)) {
    warning("Respondenten met meerdere waarden gevonden in kolommen: ", paste(cols, collapse = ", "))
  }
}

# 7. Hernoem voor duidelijkheid
setnames(df, c("new_games_spend_cat", "gaming_subs_spend_cat", 
               "streamer_donations_spend_cat", "in_game_purchases_spend_cat"),
         c("Nieuwe_videogames_bestedingscategorie", 
           "Abonnementen_gamediensten_bestedingscategorie", 
           "Donaties_streamers_bestedingscategorie", 
           "Ingame_aankopen_bestedingscategorie"))




# Identify columns that start with the specific pattern
cols_to_remove <- grep("^q13", names(df), value = TRUE)

# Remove those columns
df[, (cols_to_remove) := NULL]


df <- df %>%
  rename(
    Gaming_Purchase_Reason = `q14`,
    GamingImportance = `q15`
  )


df <- df %>%
  rename(
    FirstChoice_InFreeTime = `q16_1`,
    Identify_AsGamer = `q16_2`,
    MainTalkingSubject_FriendsFamily = `q16_3`, 
    MadeGamingfriends  = `q16_4`, 
    ProudToPlay = `q16_5`, 
    GameThemedClothing_Decoration  = `q16_6`
  )



df <- df %>%
  rename(GamingTournamentCompetition_Interest = `q17`)
df <- df %>%
  rename(EsportsViewFreq = `q18`)
df <- df %>%
  rename(GamingContent_Stream_ViewFreq = `q19`)
df <- df %>%
  rename(Esports_WatchedMostOften = `q21`)
df <- df %>%
  rename(Weekly_TimeSpent_WatchEsports = `q22`)
df <- df %>%
  rename(GeneralEsports_ViewHabits = `q25`)
df <- df %>%
  rename(EsportsTeamSupport = `q26`)




# Identify columns that start with the specific pattern
cols_to_remove <- grep("^WatchingBehavior", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]

# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Reasons for Supporting Specific Esports Teams_", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]




df <- df %>%
  rename(
    Improvement_Entertainment = `q30a`,
    Watching_Playing = `q30b`,
    Educational_Entertaining = `q30c`, 
    Stories_Gameplay  = `q30d`, 
    WatchAlone_Company = `q30e`, 
    OfficialBroadcast_Streamers  = `q30f`
  )



# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Channel Awareness_", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]


# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Content Type Watched on Each Channel_", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]




df <- df %>%
  rename(GamingCompetitiveness = `q38`)

df <- df %>%
  rename(RankedPlay = `q39`)
df <- df %>%
  rename(TournamentInvolvement = `q40`)


df <- df %>%
  rename(InfluenceSponsorship_TeamSupport = `q45`)
df <- df %>%
  rename(PreferredMethod_FinancialSupport = `q46`)


df <- df %>%
  rename(
    PositiveImage_Brand = `q50_1`,
    WantToKnowMore_Brand = `q50_2`,
    PreferOverCompeting_Brands = `q50_3`, 
    FollowSocialMedia_Brand = `q50_4`, 
    Recommend_Brand = `q50_5`, 
    Interact_Brand  = `q50_6`,
    ConsiderFuturePurch_Brand  = `q50_7`
  )




# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Merchandise Purchase History", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]



df <- df %>%
  rename(LimitedEditionPurch_EsportsTeam = `q52_1`)
df <- df %>%
  rename(LimitedEditionPurch_Streamer = `q52_2`)

df <- df %>%
  rename(ExperienceBuildingPC = `q53`)



# Identify columns that start with the specific pattern
cols_to_remove <- grep("^PC Brand Awareness", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]



# Identify columns that start with the specific pattern
cols_to_remove <- grep("^PC Brand Ownership", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]


# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Brands Best Suited to Gaming", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]





df <- df %>%
  rename(MostImportantFactor_GamingPC = `q58`)


df <- df %>%
  rename(
    PurchDecision_SupportEsportsTeam_Streamers = `q62_1`,
    PurchDecision_InGameBenefits = `q62_2`,
    PurchDecision_OwnedByLikedGamerPlayer = `q62_3`
  )


df <- df %>%
  rename(
    YourDecision_Internetprovider = `q67_1`,
    ImportanceGamingHabits_InternetProvider = `q67_2`
  )

# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Consumption Habits", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]


# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Food Categories During", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]


# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Drink Categories During", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]

# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Categories Betted On", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]

# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Categories Regularly Betted On", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]

# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Games Played in Past Year", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]


# Zet de gewenste kolommen met hun nieuwe namen
keep_map <- c(
  "q65_1"  = "Acer",
  "q65_2"  = "Alienware",
  "q65_5"  = "ASUS",
  "q65_6"  = "Bose",
  "q65_13" = "Dell",
  "q65_22" = "Gigabyte",
  "q65_24" = "HP",
  "q65_30" = "Lenovo",
  "q65_31" = "Logitech",
  "q65_32" = "Logitech G",
  "q65_37" = "MSI",
  "q65_42" = "Razer",
  "q65_48" = "Samsung",
  "q65_60" = "Intel",
  "q65_61" = "AMD",
  "q65_62" = "NVIDIA",
  "q65_69" = "Kingston",
  "q65_75" = "Seagate",
  "q65_95" = "Toshiba"
)

# Vind alle q65 kolommen in je dataframe
q65_all <- grep("^q65_[0-9]+$", names(df), value = TRUE)

# Bepaal welke kolommen NIET gewenst zijn en dus weg moeten
drop_cols <- setdiff(q65_all, names(keep_map))

# Verwijder de overbodige q65_ kolommen
df[, (drop_cols) := NULL]

# Hernoem de overgebleven kolommen volgens je mapping
setnames(df, old = names(keep_map), new = keep_map)


# Identify columns that start with the specific pattern
cols_to_remove <- grep("^Games Watched Non-esports Content_", names(df), value = TRUE)
# Remove those columns
df[, (cols_to_remove) := NULL]




library(openxlsx)


duplicated_names <- names(df)[duplicated(names(df))]
print(duplicated_names)


# 2. Load it
library(writexl)

# 3. Optional but recommended: clean column names again
names(df) <- trimws(names(df))
names(df) <- gsub("[^A-Za-z0-9_]", "_", names(df))
names(df) <- make.unique(names(df), sep = "_dup_")

# 4. Write the Excel file
write_xlsx(df, path = "Final_Dataset_PowerBI.xlsx")


library(data.table)

# Export to CSV
fwrite(df, file = "Final_Dataset.csv")
write.csv(df, file = "Final_Dataset.csv", row.names = FALSE)




rows <- nrow(df)
cols <- ncol(df)

library(ggplot2)

df_dims <- data.frame(
  Dimensie = c("Aantal rijen", "Aantal kolommen"),
  Aantal = c(rows, cols)
)


ggplot(df_dims, aes(x = Dimensie, y = Aantal, fill = Dimensie)) +
  geom_col(width = 0.4) +
  scale_y_log10() +
  labs(title = "Grootte van de dataset (log-schaal)", x = "", y = "Aantal (log10)") +
  theme_minimal() +
  theme(legend.position = "none")



library(ggpubr)

plot_rows <- ggplot(df_dims[df_dims$Dimensie == "Aantal rijen", ], aes(x = Dimensie, y = Aantal)) +
  geom_col(fill = "steelblue") +
  ylim(0, max(df_dims$Aantal)) + geom_text(aes(label = Aantal), vjust = -0.5, size = 3) + 
  labs(title = "Aantal rijen", x = NULL, y = NULL) +
  theme_minimal()

plot_cols <- ggplot(df_dims[df_dims$Dimensie == "Aantal kolommen", ], aes(x = Dimensie, y = Aantal)) +
  geom_col(fill = "tomato") +
  ylim(0, 5000) + geom_text(aes(label = Aantal), vjust = -0.5, size = 3) + 
  labs(title = "Aantal kolommen", x = NULL, y = NULL) +
  theme_minimal()


ggarrange(plot_cols, plot_rows, ncol = 2)





# ---- Step 2: Load your main dataset ----
df2 <- fread(
  "C:/Users/mira.leenders/OneDrive - Obasi/Documenten/R studio/DATASET THESIS.csv",
  stringsAsFactors = FALSE
)


rows2 <- nrow(df2)
cols2 <- ncol(df2)

library(ggplot2)

df_dims2 <- data.frame(
  Dimensie2 = c("Aantal rijen", "Aantal kolommen"),
  Aantal2 = c(rows2, cols2)
)


plot_rows2 <- ggplot(df_dims2[df_dims2$Dimensie2 == "Aantal rijen", ], aes(x = Dimensie2, y = Aantal2)) +
  geom_col(fill = "steelblue") +
  ylim(0, max(df_dims2$Aantal2)) +  geom_text(aes(label = Aantal2), vjust = -0.5, size = 3) + 
  labs(title = "Aantal rijen", x = NULL, y = NULL) +
  theme_minimal()

plot_cols2 <- ggplot(df_dims2[df_dims2$Dimensie2 == "Aantal kolommen", ], aes(x = Dimensie2, y = Aantal2)) +
  geom_col(fill = "tomato") +
  ylim(0, 5000) +  geom_text(aes(label = Aantal2), vjust = -0.5, size = 3) + 
  labs(title = "Aantal kolommen", x = NULL, y = NULL) +
  theme_minimal()


ggarrange(plot_cols2, plot_rows2, ncol = 2)





library(data.table)
library(ggplot2)

# Veronderstel dat je data.frame of data.table heet 'df'
# Stap 1: bepaal per kolom het type
type_table <- data.table(
  Kolom = names(df),
  Type = sapply(df, function(x) {
    if (is.logical(x)) return("Binair (logical)")
    else if (is.factor(x)) return("Categorisch (factor)")
    else if (is.character(x)) return("Tekst (character)")
    else if (is.numeric(x)) return("Numeriek")
    else return("Anders")
  })
)

# Stap 2: tel het aantal kolommen per type
type_counts <- type_table[, .N, by = Type]

# Stap 3: visualiseer met ggplot
ggplot(type_counts, aes(x = reorder(Type, -N), y = N, fill = Type)) +
  geom_col() +
  geom_text(aes(label = N), vjust = -0.3, size = 4) +
  labs(title = "Aantal kolommen per datatype",
       x = "Datatype",
       y = "Aantal kolommen") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(type_counts$N) + 10))


library(data.table)
library(ggplot2)

# Veronderstel dat je data.frame of data.table heet 'df'
# Stap 1: bepaal per kolom het type
type_table <- data.table(
  Kolom = names(df),
  Type = sapply(df, function(x) {
    if (is.logical(x)) return("Binair (logical)")
    else if (is.factor(x)) return("Categorisch (factor)")
    else if (is.character(x)) return("Tekst (character)")
    else if (is.numeric(x)) return("Numeriek")
    else return("Anders")
  })
)

# Stap 2: tel het aantal kolommen per type
type_counts <- type_table[, .N, by = Type]

# Stap 3: visualiseer met ggplot
ggplot(type_counts, aes(x = reorder(Type, -N), y = N, fill = Type)) +
  geom_col() +
  geom_text(aes(label = N), vjust = -0.3, size = 4) +
  labs(title = "Aantal kolommen per datatype",
       x = "Datatype",
       y = "Aantal kolommen") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(type_counts$N) + 10))



##############cHANGE DATA TYPE
library(data.table)
setDT(df)

# 1. Define the prefixes you want to convert
prefixes <- c(
  "Esports Awareness_", "Devices Used Ever_", "Games Played in Last 12 Months",
  "Games Played Regularly_", "Esports Watched Ever_", "Esports Team Supported_",
  "Esports Team / Influencer Engagement Methods_", "Gaming Content Type Preferences_",
  "Non-Esports Games Watched", "Tournament Organising Platforms_",
  "Esports Experience Interest_", "Prefered Brand Contributions_", "Primary Use of PC_",
  "Triggers for Upgrade / Replacement_", "Peripheral Ownership_", "Peripheral Decision Factors_",
  "Peripheral Brand Awareness_", "Component Brand Awareness_", "Peripheral Purchase Triggers_",
  "YourDecision_(yes/no)",
  "Reasons for Not Betting on Esports_", "Esports Title Betting Preferences_",
  "Team Awareness_", "Specific Esports Tournaments Watched_",
  "Reasons for Supporting General Esports Teams_", "Games_None_of_these_Both"
)

# 2. Create regex pattern and find matching columns
pattern <- paste0("^(", paste0(prefixes, collapse = "|"), ")")
cols_to_convert <- grep(pattern, names(df), value = TRUE)

# 3. Convert: "0" or "" → 0, everything else → 1
df[, (cols_to_convert) := lapply(.SD, function(x) {
  x <- as.character(x)
  fifelse(x == "0" | x == "", 0L, 1L)
}), .SDcols = cols_to_convert]

# 4. Optional: Check result
summary(df[, ..cols_to_convert])



#########################VARIABILITEIT

library(data.table)
library(ggplot2)

# Zet naar data.table indien nodig
setDT(df)

# Kolomnamen behalve 'hash'
kolommen_zonder_hash <- setdiff(names(df), "hash")

# Bereken aantal unieke waarden per kolom (zonder hash)
variability <- data.table(
  Kolom = kolommen_zonder_hash,
  AantalUniek = sapply(df[, ..kolommen_zonder_hash], function(x) length(unique(x)))
)

# Sorteer op aflopende variabiliteit
variability <- variability[order(-AantalUniek)]

# Bekijk top 20 met meeste variatie
head(variability, 20)

# Visualisatie
ggplot(variability[1:20], aes(x = reorder(Kolom, -AantalUniek), y = AantalUniek)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Aantal unieke waarden per kolom (top 20, zonder 'hash')",
       x = "Kolomnaam", y = "Aantal unieke waarden") +
  theme_minimal()

library(data.table)
library(ggplot2)

# Zet naar data.table indien nodig
setDT(df)

# Bereken aantal unieke waarden (zonder 'hash')
variability <- data.table(
  Kolom = setdiff(names(df), "hash"),
  AantalUniek = sapply(df[, !("hash"), with=FALSE], function(x) length(unique(x)))
)

# Voeg categorisering toe
variability[, Variabiliteit := fifelse(AantalUniek <= 2, "Binair (≤2)",
                                       fifelse(AantalUniek <= 5, "Laag (3–5)",
                                               fifelse(AantalUniek <= 10, "Gemiddeld (6–10)", "Hoog (>10)")))]

# Plot: aantal kolommen per categorie
ggplot(variability[, .N, by=Variabiliteit], aes(x=reorder(Variabiliteit, -N), y=N, fill=Variabiliteit)) +
  geom_col() +
  geom_text(aes(label=N), vjust=-0.5) +
  labs(title = "Aantal kolommen per niveau van variabiliteit",
       x = "Categorie variabiliteit", y = "Aantal kolommen") +
  theme_minimal() +
  theme(legend.position = "none")



library(data.table)
setDT(df)

# Bereken het aantal NA's per kolom
na_counts <- sapply(df, function(x) sum(is.na(x)))

# Selecteer kolommen met ≤ 16.000 NA's
cols_to_keep <- names(na_counts)[na_counts <= 16000]

# Houd enkel die kolommen over in de dataset
df <- df[, ..cols_to_keep]




#################aantal NA's
library(data.table)
library(ggplot2)

# Zorg dat df een data.table is
setDT(df)

# Sluit de hash-kolom uit indien gewenst
kolommen_zonder_hash <- setdiff(names(df), "hash")

# Bereken aantal NA's per kolom
na_telling <- data.table(
  Kolom = kolommen_zonder_hash,
  Aantal_NA = sapply(df[, ..kolommen_zonder_hash], function(x) sum(is.na(x)))
)

# Sorteer op aflopende volgorde
na_telling <- na_telling[order(-Aantal_NA)]

# Visualisatie met data labels
ggplot(na_telling[1:30], aes(x = reorder(Kolom, Aantal_NA), y = Aantal_NA)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = Aantal_NA), hjust = -0.1, size = 3.5) +  # data labels
  coord_flip() +
  ylim(0, max(na_telling$Aantal_NA[1:30]) * 1.1) +  # extra ruimte voor labels
  labs(title = "Top 20 kolommen met meeste NA's",
       x = "Kolom", y = "Aantal NA's") +
  theme_minimal()




# Zorg dat df een data.table is
library(data.table)
setDT(df)

# Voeg een kolom toe met het aantal niet-NA antwoorden per rij (respondent)
df[, not_NA_per_row := rowSums(!is.na(.SD))]

# Gemiddelde aantal niet-lege antwoorden per respondent
mean_not_NA <- mean(df$not_NA_per_row)
print(mean_not_NA)

library(ggplot2)
names(df) <- make.unique(names(df), sep = "_dup_")


ggplot(df, aes(x = not_NA_per_row)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Aantal niet-lege antwoorden per respondent",
       x = "Aantal niet-lege antwoorden",
       y = "Aantal respondenten") +
  theme_minimal()




# Assuming your dataset is called `df`

# Total number of values in the dataset
total_values <- prod(dim(df))

# Total number of missing values
total_missing <- sum(is.na(df))

# Proportion of missing values
missing_proportion <- total_missing / total_values

# Display the result as a percentage
missing_percentage <- round(missing_proportion * 100, 2)
cat("Overall proportion of missing values:", missing_percentage, "%\n")

