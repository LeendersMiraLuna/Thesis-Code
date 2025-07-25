#LIBRARIES
library(readxl)       
library(data.table)   
library(clustMixType) 
library(FactoMineR)   
library(factoextra)   
library(cluster)      
library(DescTools)    
library(caret)        
library(openxlsx)     
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)
library(reshape2)
library(ggplot2)
library(ggpubr)


#MAIN DATASET
df <- fread(
  "C:/Users/mira.leenders/OneDrive - Obasi/Documenten/R studio/DATASET THESIS.csv",
  stringsAsFactors = FALSE
)

############VOORBEREIDING
#mapping file 
mapping <- read.csv(
  "C:\\Users\\mira.leenders\\OneDrive - Obasi\\Documenten\\Thesis\\dataset sheet1.csv",
  sep = ";",
  stringsAsFactors = FALSE
)

#Fix the gwi-siz-fce prefix and drop unwanted columns
# Replace "_" with "-" in the prefix
setnames(
  df,
  old = grep("^gwi_siz_fce", names(df), value = TRUE),
  new = gsub("^gwi_siz_fce", "gwi-siz-fce", grep("^gwi_siz_fce", names(df), value = TRUE))
)

df <- df[, !grepl("^gwi-siz-fce\\.(q(33|32|102)|weighting|s2)", names(df)), with = FALSE]

#Rename based on mapping
mapping$new_name <- paste(mapping[[3]], mapping[[5]], sep = "_")
name_mapping <- setNames(mapping$new_name, mapping[[1]])
setnames(
  df,
  old = intersect(names(name_mapping), names(df)),
  new = name_mapping[ intersect(names(name_mapping), names(df)) ]
)




#######OPTIONAL- Frequency tables 
Freq_select_cols <- grep("^gwi_siz_fce\\.q9b?_", names(df2), value = TRUE)

freq_list <- lapply(Freq_select_cols, function(col) {
  tbl <- as.data.frame(table(df2[[col]], useNA = "ifany"))
  names(tbl) <- c("Value", "Frequency")
  tbl$Column <- col
  tbl
})
freq_table <- rbindlist(freq_list)




#####DEVICES USED EVER####
device_cols <- df %>% 
  select(starts_with("Devices Used Ever_"))
freq_tables <- map(device_cols, ~ table(.x, useNA = "ifany"))
freq_tables
freq_df <- freq_tables %>%
  imap_dfr(~ as.data.frame(.x) %>% 
             rename(Value = .x, Frequency = Freq) %>% 
             mutate(Column = .y))
print(freq_df)


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

df[, Playstation := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = playstation_cols]
df[, Xbox       := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = xbox_cols]
df[, Nintendo   := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = nintendo_cols]
df[, `PC/Mac`   := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),        .SDcols = pcmac_cols]
df[, `Mobile devices`               := rowSums(.SD != "0" & .SD != "", na.rm = TRUE), .SDcols = mobile_cols]

cols_to_delete <- c(
  playstation_cols,
  xbox_cols,
  nintendo_cols,
  pcmac_cols,
  mobile_cols
)
df[, (cols_to_delete) := NULL]
#####END DEVICES USED EVER



#####Games Played in last 12M and Regularly
setDT(df)  
#mapping table
map <- fread("C:\\Users\\mira.leenders\\Downloads\\Game_Genre_Mapping.csv")  
extract_game_name <- function(colname) {
  sub(".*_", "", colname)
}
game_cols <- grep("^Games Played (in Last 12 Months|Regularly)", names(df), value = TRUE)
col_map <- data.table(
  original_col = game_cols,
  game = sapply(game_cols, extract_game_name)
)
col_map <- merge(col_map, map, by.x = "game", by.y = "Game", all.x = TRUE)

for (qtype in c("Last 12 Months", "Regularly")) {
  cols_this_type <- col_map[grepl(qtype, original_col)]
  for (g in unique(na.omit(cols_this_type$Genre))) {
    genre_cols <- cols_this_type[Genre == g & grepl(qtype, original_col), original_col]
    new_colname <- paste0(gsub(" ", "_", g), "_", gsub(" ", "_", qtype))
    df[, (new_colname) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE), .SDcols = genre_cols]
  }
}
View(df[, ..genre_cols])
genre_cols <- grep("_(Last_12_Months|Regularly)$", names(df), value = TRUE)
df[, ..genre_cols]  

names(df) <- make.unique(names(df), sep = "_dup_")

none1 <- "Games Played in Last 12 Months (Part 1 of 2)_None of these"
none2 <- "Games Played in Last 12 Months (Part 2 of 2)_None of these"
df$Games_None_of_these_Both <- ifelse(
  df[[none1]] == "None of these" & df[[none2]] == "None of these",
  "none of these",
  0
)

#games to KEEP
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

all_game_cols <- grep("^Games Played in Last 12 Months \\(Part [12] of 2\\)_", names(df), value = TRUE)
cols_to_remove <- setdiff(all_game_cols, keep_exact)
df[, (cols_to_remove) := NULL]


#games to KEEP
keep_games <- c("Call of Duty", "Candy Crush", "FIFA", "Fortnite", "GTA V",
                "Mario Kart", "Minecraft", "Pokemon", "Roblox", "League of Legends", "None of these")
all_regular_cols <- grep("^Games Played Regularly_", names(df), value = TRUE)
game_names <- sub("^Games Played Regularly_", "", all_regular_cols)
keep_cols <- all_regular_cols[game_names %in% keep_games]
cols_to_remove <- setdiff(all_regular_cols, keep_cols)
df[, (cols_to_remove) := NULL]
#############END- GAMES PLAYED LAST 12 M and RGEULRALRY



#############Esports Watched Ever
#mapping file
mapping <- fread("C:/Users/mira.leenders/Downloads/Final_Esports_Watched_Ever_Mapping.csv")
watched_cols <- grep("^Esports Watched Ever_", names(df), value = TRUE)
watched_dt <- data.table(column = watched_cols)
watched_dt[, `Original Esport Title` := 
             str_replace(column, "^Esports Watched Ever_", "")
]
watched_dt <- merge(
  watched_dt,
  mapping,
  by = "Original Esport Title",
  all.x = TRUE
)

for (g in unique(watched_dt$`Grouped Genre / Category`)) {
  cols_for_genre <- watched_dt[`Grouped Genre / Category` == g, column]
  safe_genre    <- gsub("[ /]", "_", g)
  new_colname   <- paste0("Watched_Ever_", safe_genre)
  df[, (new_colname) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),
     .SDcols = cols_for_genre
  ]
}

genre_cols <- grep("^Watched_Ever_", names(df), value = TRUE)
df[, ..genre_cols]

#titles to KEEP
keep_esports <- c("FIFA", "Fortnite", "Call of Duty", "League of Legends", 
                  "Call Of Duty Warzone", "Mortal Kombat 11", "PUBG", 
                  "Call of Duty Mobile", "League of Legends: Wild Rift", 
                  "Counter-Strike 2 / CS:GO")
all_esports_cols <- grep("^Esports Watched Ever_", names(df), value = TRUE)
esports_names <- sub("^Esports Watched Ever_", "", all_esports_cols)
keep_cols <- all_esports_cols[esports_names %in% keep_esports]
cols_to_remove <- setdiff(all_esports_cols, keep_cols)
df[, (cols_to_remove) := NULL]
########END-ESPORTSWATCHEDEVER



########WATCHING BEHAVIOR
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

low_answers <- c(
  "I won't tune in for any particular tournament, but might watch the occasional game out of interest",
  "I will only watch the finals of the biggest tournaments"
)

medium_answers <- c("I only watch the biggest tournaments")
high_answers <- c("I watch most tournaments I can", "I'll tune into every tournament I can")
df$Watching_Low <- ""
df$Watching_Medium <- ""
df$Watching_High <- ""
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
df$Watching_Low <- trimws(gsub("^;\\s*", "", df$Watching_Low))
df$Watching_Medium <- trimws(gsub("^;\\s*", "", df$Watching_Medium))
df$Watching_High <- trimws(gsub("^;\\s*", "", df$Watching_High))
########END - WATCHING BEHAVIOR



########TEAMSSUPPORTED
all_supported <- grep("^Esports Team Supported_", names(df), value = TRUE)
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
keep_present <- intersect(keep_teams, all_supported)
df[ , total_supported := rowSums(
  .SD != "0" & .SD != "", 
  na.rm = TRUE
), .SDcols = all_supported
]
if (length(keep_present) > 0) {
  df[ , keep_count := rowSums(
    .SD != "0" & .SD != "", 
    na.rm = TRUE
  ), .SDcols = keep_present
  ]
} else {
  df[ , keep_count := 0L ]
}
df[ , Other := total_supported - keep_count ]
df[ , c("total_supported", "keep_count") := NULL ]
all_team_cols <- grep("^Esports Team Supported_", names(df), value = TRUE)
cols_to_remove <- setdiff(all_team_cols, keep_teams)
df[, (cols_to_remove) := NULL]
####### END - ESPORTS TEAM SUPPORTED



#######NON ESPORTS GAMES 
watched_cols <- grep(
  "^Non-Esports Games Watched \\(Part [12] of 2\\)_",
  names(df),
  value = TRUE
)
print(watched_cols)
# 2)mapping 
map_ne <- fread("C:\\Users\\mira.leenders\\Downloads\\Full_Non_Esports_Game_Grouping_With_Reasons.csv")
watched_cols <- grep(
  "^Non-Esports Games Watched \\(Part [12] of 2\\)_",
  names(df),
  value = TRUE
)
col_map <- data.table(original_col = watched_cols)
col_map[, game := sub(".*_", "", original_col)]
col_map <- merge(
  col_map,
  map_ne[, .(Game, Category)],
  by.x = "game",
  by.y = "Game",
  all.x = TRUE
)

for(cat in unique(na.omit(col_map$Category))) {
  cols_for_cat <- col_map[Category == cat, original_col]
  # make a clean name for the new column
  safe_cat <- gsub("[^A-Za-z0-9]", "_", cat)
  new_col  <- paste0(safe_cat, "_Watched")
  
  df[, (new_col) := rowSums(.SD != "0" & .SD != "", na.rm = TRUE),
     .SDcols = cols_for_cat]
}
genre_cols <- grep("_Watched$", names(df), value = TRUE)
head(df[, ..genre_cols])
# to KEEP
keep_games <- c("FIFA", "Minecraft", "Call of Duty", "Fortnite", "Candy Crush",
                "Mario Kart", "GTA V", "Pokemon", "Resident Evil", "Mortal Kombat")
all_non_esports_cols <- grep("^Non-Esports Games Watched \\(Part [12] of 2\\)_", names(df), value = TRUE)
game_titles <- sub("^Non-Esports Games Watched \\(Part [12] of 2\\)_", "", all_non_esports_cols)
keep_cols <- all_non_esports_cols[game_titles %in% keep_games]

none1 <- grep("^Non-Esports Games Watched \\(Part 1 of 2\\)_None of these", names(df), value = TRUE)
none2 <- grep("^Non-Esports Games Watched \\(Part 2 of 2\\)_None of these", names(df), value = TRUE)
df[, `Non Esports Games_None of these` := ifelse(
  get(none1) == "None of these" & get(none2) == "None of these",
  "None of these", 0
)]
cols_to_remove <- setdiff(all_non_esports_cols, keep_cols)
df[, (cols_to_remove) := NULL]
#########END - NON ESPORTS GAMES




##########CHANNEL AWARENESS AND CONTENT TYPE WATCHED
aw_cols   <- grep("^Channel Awareness_", names(df), value=TRUE)
ct_base   <- sub("^Channel Awareness_", "Content Type Watched on Each Channel_", aw_cols)
channels  <- sub("^Channel Awareness_", "", aw_cols)
for (i in seq_along(channels)) {
  ch      <- channels[i]
  aw      <- df[[ aw_cols[i] ]]        
  c0      <- df[[ ct_base[i]    ]]     
  c1      <- df[[ paste0(ct_base[i],".1") ]]  
  c2      <- df[[ paste0(ct_base[i],".2") ]]  
  result <- rep("", nrow(df))
  is_aw <- !aw %in% c("0","",NA)
  result[is_aw] <- "aware"
  if (ch != "None of these") {
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
  
  set(df, j = ch, value = result)
}
#####END - CHANNEL AWARENESS AND CONTENT TYPE WATCHED 



######ADVERTISING TYPE
rec_cols  <- grep("^Recognition of Esports Advertising Types_", names(df), value=TRUE)
pref_cols <- grep("^Preference of Esports Advertising Types_",     names(df), value=TRUE)
ad_types  <- sub(".*_", "", rec_cols)
new_cols  <- gsub("[^A-Za-z0-9]+", "_", ad_types)
for (i in seq_along(ad_types)) {
  rec_col <- rec_cols[i]
  pref_col<- pref_cols[i]
  new_col <- new_cols[i]
  type    <- ad_types[i]
  rec_vec  <- df[[rec_col]]  == type
  pref_vec <- df[[pref_col]] == type
  out <- rep("", nrow(df))
  both        <- rec_vec & pref_vec
  out[both]   <- "Recognition, Preference"
  only_rec    <- rec_vec & !pref_vec
  out[only_rec] <- "Recognition"
  only_pref   <- pref_vec & !rec_vec
  out[only_pref] <- "Preference"
  df[, (new_col) := out]
}

df[, c(rec_cols, pref_cols) := NULL]
######END - ADVERTISING TYPE



######SPONSOR CATEGORIES
exp_cols  <- grep("^Expected Sponsor Categories_", names(df), value = TRUE)
pref_cols <- grep("^Preference of Sponsor Categories_", names(df), value = TRUE)
categories <- sub("^Expected Sponsor Categories_", "", exp_cols)
safe_names <- gsub("[^A-Za-z0-9]", "_", categories)

for (i in seq_along(categories)) {
  cat    <- categories[i]
  safe   <- safe_names[i]
  e_col  <- exp_cols[i]
  p_col  <- pref_cols[i]
  e_vec  <- df[[e_col]]  == cat
  p_vec  <- df[[p_col]]  == cat
  out <- rep("", nrow(df))
  both <- e_vec & p_vec
  out[both] <- "expectation, preference"
  only_e <- e_vec & !p_vec
  out[only_e] <- "expectation"
  only_p <- p_vec & !e_vec
  out[only_p] <- "preference"
  set(df, j = safe, value = out)
}

df[, c(exp_cols, pref_cols) := NULL]
head(df[, safe_names, with = FALSE])
######END - SPONSOR CATEGORIES



##########################MERCH
all_cols <- grep("^Merchandise Purchase History / Intention_", names(df), value = TRUE)
base_labels <- unique(sub("_dup_\\d+$", "", all_cols))
summary_names <- sub("^Merchandise Purchase History / Intention_", "", base_labels)
summary_names <- gsub("[^A-Za-z0-9]", "_", summary_names)

for (i in seq_along(base_labels)) {
  base <- base_labels[i]
  new_col <- summary_names[i]
  out <- rep("", nrow(df))
  purchased_col  <- base
  interested_col <- paste0(base, "_dup_1")
  neither_col    <- paste0(base, "_dup_2")
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
  df[[new_col]] <- out
}

head(df[, ..summary_names])
#######END - MERCH



#######PC BRANDS
awareness_cols <- grep("^PC Brand Awareness_", names(df), value = TRUE)

ownership_cols <- grep("^PC Brand Ownership_", names(df), value = TRUE)

best_suited_cols <- grep("^Brands Best Suited to Gaming_", names(df), value = TRUE)

print(awareness_cols)
print(ownership_cols)
print(best_suited_cols)

awareness_cols     <- print(awareness_cols)
ownership_cols     <- print(ownership_cols)
best_suited_cols   <- print(best_suited_cols)

aw_brands   <- sub("^PC Brand Awareness_",            "", awareness_cols)
own_brands  <- sub("^PC Brand Ownership_",            "", ownership_cols)
best_brands <- sub("^Brands Best Suited to Gaming_",  "", best_suited_cols)

aw_brands   <- gsub("\\.$", "", aw_brands)
own_brands  <- gsub("\\.$", "", own_brands)
best_brands <- gsub("\\.$", "", best_brands)

cat("Aw vs Own identical? ", setequal(aw_brands, own_brands), "\n")
cat("Aw vs Best identical? ", setequal(aw_brands, best_brands), "\n")
cat("Own vs Best identical? ", setequal(own_brands, best_brands), "\n")

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

brand_groups <- list(
  `Major Global PC Brands`              = c("Dell","hp","Lenovo", "Medion", "Toshiba","Acer","Apple (Mac)","Apple", "Samsung","Fujitsu", "ASUS"),
  `Gaming-Focused Brands`       = c("MSI", "ROG - Republic of Gamers", "Legion", "NZXT", "Alienware", "Corsair", "Razer", "AORUS", "OMEN", "CyberPowerPC", "SCAN", "Predator", "Xidax", "Origin", "LDLC", "Galleria", "iBuyPower", "Velocity Micro", "Alternate", "XMG", "Origin PC", "Falcon Northwest", "Maingear", "Velocity", "PCSpecialist", "Schenker", "Digital Storm", "Materiel.net", "King Mod Systems", "Millenium"),
  `Component Manufacturers/ Niche Hardware Brands` = c("Shuttle", "Gigabyte", "Leadtek", "Lite-On", "Biostar", "Tones", "ZOTAC", "Albatron", "Aerocool"),
  `Self-Built or Custom`         = c("Self-built"),
  `None of these`           = c("None of these")
)

aw_cols   <- grep("^PC Brand Awareness_",            names(df), value=TRUE)
own_cols  <- grep("^PC Brand Ownership_",            names(df), value=TRUE)
best_cols <- grep("^Brands Best Suited to Gaming_",  names(df), value=TRUE)

aw_brands   <- gsub("\\.$","", sub("^PC Brand Awareness_",           "", aw_cols))
own_brands  <- gsub("\\.$","", sub("^PC Brand Ownership_",           "", own_cols))
best_brands <- gsub("\\.$","", sub("^Brands Best Suited to Gaming_", "", best_cols))

for (grp in names(brand_groups)) {
  
  safe_grp <- gsub("[^A-Za-z0-9]+","_", grp)
  members  <- brand_groups[[grp]]
  
  aw_match   <- aw_cols[   aw_brands   %in% members ]
  own_match  <- own_cols[  own_brands  %in% members ]
  best_match <- best_cols[ best_brands %in% members ]

  out <- rep("", nrow(df))

  if (length(aw_match)) {
    idx <- rowSums(df[, ..aw_match] != "0" & df[, ..aw_match] != "", na.rm=TRUE) > 0
    out[idx] <- "aware"
  }

  if (length(own_match)) {
    idx <- rowSums(df[, ..own_match] != "0" & df[, ..own_match] != "", na.rm=TRUE) > 0
    out[idx] <- ifelse(out[idx]=="", "owner", paste(out[idx],"owner", sep=", "))
  }
  
  if (length(best_match)) {
    idx <- rowSums(df[, ..best_match] != "0" & df[, ..best_match] != "", na.rm=TRUE) > 0
    out[idx] <- ifelse(out[idx]=="", "best suited", paste(out[idx],"best suited", sep=", "))
  }
  
  df[, (safe_grp) := out]
}

df[, gsub("[^A-Za-z0-9]+","_", names(brand_groups)), with=FALSE]

# 14 target brands
brands <- c("Acer", "Apple (Mac)", "Samsung", "Lenovo", "Dell", "ASUS", 
            "Apple", "Self-built", "Alienware", "hp", "Toshiba", "Fujitsu", 
            "MSI", "Razer")

aw_cols   <- grep("^PC Brand Awareness_",            names(df), value=TRUE)
own_cols  <- grep("^PC Brand Ownership_",            names(df), value=TRUE)
best_cols <- grep("^Brands Best Suited to Gaming_",  names(df), value=TRUE)

aw_brands   <- sub("^PC Brand Awareness_", "", aw_cols)
own_brands  <- sub("^PC Brand Ownership_", "", own_cols)
best_brands <- sub("^Brands Best Suited to Gaming_", "", best_cols)

for (brand in brands) {
  # Column-safe brand name
  col_name <- gsub("[^A-Za-z0-9]+", "_", brand)
  
  aw_col   <- aw_cols[aw_brands == brand]
  own_col  <- own_cols[own_brands == brand]
  best_col <- best_cols[best_brands == brand]
  
  out <- rep("", nrow(df))
  
  if (length(aw_col) == 1) {
    idx <- df[[aw_col]] != "0" & df[[aw_col]] != ""
    out[idx] <- "aware"
  }

  if (length(own_col) == 1) {
    idx <- df[[own_col]] != "0" & df[[own_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "owner", paste(out[idx], "owner", sep = ", "))
  }
  
  if (length(best_col) == 1) {
    idx <- df[[best_col]] != "0" & df[[best_col]] != ""
    out[idx] <- ifelse(out[idx] == "", "best suited", paste(out[idx], "best suited", sep = ", "))
  }
  
  df[[col_name]] <- out
}

new_cols <- gsub("[^A-Za-z0-9]+", "_", brands)
head(df[, ..new_cols])
#######END PC BRANDS 



#######COMPONENT BRAND
periph_cols   <- grep("^Peripheral Brand Awareness_", names(df), value = TRUE)
component_cols<- grep("^Component Brand Awareness_",  names(df), value = TRUE)
print(periph_cols)
print(component_cols)

#brand mapping
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
comp_cols   <- grep("^Component Brand Awareness_", names(df), value=TRUE)
comp_brands <- sub("^Component Brand Awareness_", "", comp_cols)
comp_brands <- gsub("\\.$", "", comp_brands)

for (grp in names(component_groups)) {
  members <- component_groups[[grp]]
  cols    <- comp_cols[comp_brands %in% members]
  newcol  <- grp
  
  if (length(cols)) {
    df[, (newcol) := rowSums(.SD != "0" & .SD != "", na.rm=TRUE), .SDcols = cols]
  } else {

    df[, (newcol) := 0L]
  }
}

df[, names(component_groups), with=FALSE]

keep_brands <- c("Seagate", "MSI", "Gigabyte", "Kingston", "AMD", 
                 "NVIDIA", "Toshiba", "Samsung", "ASUS", "Intel")
all_aw_cols <- grep("^Component Brand Awareness_", names(df), value = TRUE)
brand_names <- sub("^Component Brand Awareness_", "", all_aw_cols)
keep_cols <- all_aw_cols[brand_names %in% keep_brands]
drop_cols <- setdiff(all_aw_cols, keep_cols)
df[, (drop_cols) := NULL]
#######END COMPONENT BRANDS 



#######PERIPHERAL BRANDS
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

aw_cols   <- grep("^Peripheral Brand Awareness_", names(df), value=TRUE)
aw_brands <- sub("^Peripheral Brand Awareness_", "", aw_cols)
aw_brands <- gsub("\\.$","", aw_brands)  # remove any trailing “.”
for (grp in names(periph_groups)) {
  members <- periph_groups[[grp]]
  cols    <- aw_cols[aw_brands %in% members]
  newcol  <- grp
  
  if (length(cols) > 0) {

    df[, (newcol) := rowSums(.SD != "0" & .SD != "", na.rm=TRUE), .SDcols = cols]
  } else {
  
    df[, (newcol) := 0L]
  }
}

df[, names(periph_groups), with=FALSE]

keep_brands <- c("Bose", "Razer", "Samsung", "Logitech G", "Logitech", 
                 "HP", "Dell", "Acer", "ASUS", "Lenovo")
all_periph_cols <- grep("^Peripheral Brand Awareness_", names(df), value = TRUE)
brand_names <- sub("^Peripheral Brand Awareness_", "", all_periph_cols)
keep_cols <- all_periph_cols[brand_names %in% keep_brands]
drop_cols <- setdiff(all_periph_cols, keep_cols)
df[, (drop_cols) := NULL]
####END - PERIPHERAL BRANDS



####CONFIDENCE IN BRANDS
component_cols<- grep("^gwi-siz-fce.q65_",  names(df), value = TRUE)
print(component_cols)
# Mapping
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

# Grouping
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

brand_to_group <- data.table(
  brand = unlist(group_map),
  group = rep(names(group_map), lengths(group_map))
)

id_to_brand <- data.table(
  id = unlist(brand_map),
  brand = rep(names(brand_map), lengths(brand_map))
)

id_to_group <- merge(id_to_brand, brand_to_group, by = "brand", all.x = TRUE)
print(id_to_group[is.na(group)])

q65_cols <- grep("^gwi-siz-fce\\.q65_[0-9]+$", names(df), value = TRUE)
cat("Number of matched q65 columns: ", length(q65_cols), "\n")
print(q65_cols[1:5])  # just to check
q65_ids <- as.integer(sub(".*q65_([0-9]+)$", "\\1", q65_cols))
df[ , (q65_cols) := lapply(.SD, function(x) {
  xx <- trimws(as.character(x))
  tolower(xx)
}), .SDcols = q65_cols ]
levels <- c("very unconfident", "unconfident", "neutral", "confident", "very confident")
component_groups <- split(id_to_group$id, id_to_group$group)

for (lvl in levels) {
  newcol <- lvl
  count_dt <- lapply(component_groups, function(idxs) {
    cols <- q65_cols[q65_ids %in% idxs]
    if (length(cols)) {
      rowSums(df[, ..cols] == lvl, na.rm = TRUE)
    } else {
      integer(nrow(df))
    }
  })
  count_dt <- as.data.table(count_dt)

  summary_vec <- apply(count_dt, 1, function(r) {
    nz <- which(r > 0)
    if (!length(nz)) return(NA_character_)
    paste0(names(r)[nz], ":", r[nz], collapse = ", ")
  })
  df[, (newcol) := summary_vec]
}

df[, levels, with = FALSE]
confidence_cols <- c("very confident", "confident", "neutral", "unconfident", "very unconfident")

for (col in confidence_cols) {
  cat("\nUnique values in column:", col, "\n")
  print(unique(df[[col]]))
}
#####END - CONFIDENCE IN BRANDS



######CONSUMPTION
names(df) <- make.unique(names(df), sep=".")

cons_cols  <- grep("^Consumption Habits During Gaming", names(df), value=TRUE)
food_cols  <- grep("^Food Categories During Gaming",       names(df), value=TRUE)
drink_cols <- grep("^Drink Categories During Gaming",      names(df), value=TRUE)

contexts <- c("while playing", "while waiting", "while watching")

beh_cons  <- c("Eat_food", "Drink_soft_drinks", "Drink_alcohol", "None_of_these")
beh_food  <- c("Snacks", "Sweets_Candy", "Healthy_snacks", "Full_home_cooked_meals",
               "Takeaways_Home_delivery", "Other_foods")
beh_drink <- c("Energy_drinks", "Soda_Fizzy_drinks", "Water_Flavoured_water",
               "Juices", "Hot_drinks", "Other_drinks")

make_summary <- function(df, cols_block, behaviors) {
  n_beh <- length(behaviors)
  stopifnot(length(cols_block) == n_beh * length(contexts))
  
  for (i in seq_len(n_beh)) {
    beh <- behaviors[i]
    out <- character(nrow(df))
    
    for (j in seq_along(contexts)) {
      ctx <- contexts[j]
      col <- cols_block[(j-1)*n_beh + i]
      sel <- df[[col]] != "0" & df[[col]] != "" & !is.na(df[[col]])
      out[sel] <- paste0(out[sel],
                         ifelse(out[sel]=="", "", ", "),
                         ctx)
    }
    
    out[out==""] <- NA_character_
    df[ , (beh) := out ]
  }
}

make_summary(df, cons_cols,  beh_cons)
make_summary(df, food_cols,  beh_food)
make_summary(df, drink_cols, beh_drink)
new_cols <- c(beh_cons, beh_food, beh_drink)
df[ , ..new_cols ]
######END Consumption



######BETTING
bet_cols <- grep("^Categories Betted On_", names(df), value=TRUE)
reg_cols <- grep("^Categories Regularly Betted On_", names(df), value=TRUE)
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

for (i in seq_along(categories)) {
  cat_label <- categories[i]
  new_col   <- new_names[i]
  bet_col <- grep(paste0("^Categories Betted On_", 
                         gsub("\\(", "\\\\(", cat_label),
                         "$"),
                  bet_cols, value=TRUE)
  
  reg_col <- grep(paste0("^Categories Regularly Betted On_", 
                         gsub("\\(", "\\\\(", cat_label),
                         "\\.?$"),
                  reg_cols, value=TRUE)
 
  has_reg <- length(reg_col)==1
 
  df[ , (new_col) := {
    b <- !is.na(get(bet_col)) & get(bet_col) != "0" & get(bet_col) != ""
    if (has_reg) {
      r <- !is.na(get(reg_col)) & get(reg_col) != "0" & get(reg_col) != ""
    } else {
      r <- rep(FALSE, .N)
    }
    out <- rep(NA_character_, .N)
    out[b & r]   <- "betted on, regularly betted on"
    out[b &!r]   <- "betted on"
    out[!b & r]  <- "regularly betted on"
    out
  }]
}
df[ , .SD, .SDcols=new_names ]
component_cols<- grep("^Esports Title Betting Preferences_",  names(df), value = TRUE)
print(component_cols)

mapping <- fread("C:/Users/mira.leenders/Downloads/Final_Esports_Watched_Ever_Mapping.csv")
pref_cols <- grep("^Esports Title Betting Preferences_", names(df), value = TRUE)
pref_dt <- data.table(column = pref_cols)
pref_dt[, Original_Esport_Title := sub(
  "^Esports Title Betting Preferences_",
  "",
  column
)]

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
for (g in unique(pref_dt$Genre)) {

  cols_for_group <- pref_dt[Genre == g, column]
  safe_name <- gsub("[ /]", "_", g)
  new_col   <- paste0("BettingPref_", safe_name)

  df[ , (new_col) := rowSums(
    .SD != "0" & .SD != "",
    na.rm = TRUE
  ), .SDcols = cols_for_group ]
}

new_cols <- grep("^BettingPref_", names(df), value = TRUE)
df[ , ..new_cols ]

keep_titles <- c("Mortal Kombat 11", "Call Of Duty Warzone", "PUBG", "Dota2",
                 "League of Legends: Wild Rift", "Call of Duty Mobile",
                 "League of Legends", "Portnite", "Call of Duty", "FIFA")

all_bet_cols <- grep("^Esports Title Betting Preferences_", names(df), value = TRUE)
titles <- sub("^Esports Title Betting Preferences_", "", all_bet_cols)
keep_cols <- all_bet_cols[titles %in% keep_titles]
drop_cols <- setdiff(all_bet_cols, keep_cols)
df[, (drop_cols) := NULL]
######END OF BETTING



#####TEAM AWARENESS
component_cols<- grep("^Team Awareness_",  names(df), value = TRUE)
print(component_cols)
team_cols <- grep("^Team Awareness_", names(df), value = TRUE)

freq_list <- lapply(team_cols, function(col) {
  dt <- df[, .(Frequency = .N), by = .(Value = get(col))]
  dt[, Column := col]
  setcolorder(dt, c("Column","Value","Frequency"))
  dt
})

freq_table <- rbindlist(freq_list)
print(freq_table)

team_cols <- grep("^Team Awareness_", names(df), value = TRUE)
keep <- c(
  "Team Awareness_Alliance",
  "Team Awareness_Cloud9",  
  "Team Awareness_Excel Esports", "Team Awareness_Fnatic",
  "Team Awareness_G2 Esports","Team Awareness_Nova Esports",
  "Team Awareness_PSG",            "Team Awareness_Red Bull",
  "Team Awareness_Team Liquid",  "Team Awareness_Team Vitality",  "Team Awareness_None of these"
)
other_cols <- setdiff(team_cols, keep)
df[ , other_awareness := rowSums(
  .SD != "0" & .SD != "",
  na.rm = TRUE
), .SDcols = other_cols
]

df[ , .(other_awareness)][ , summary(other_awareness) ]
keep <- c(
  "Team Awareness_Alliance",
  "Team Awareness_Cloud9",  
  "Team Awareness_Excel Esports", "Team Awareness_Fnatic",
  "Team Awareness_G2 Esports","Team Awareness_Nova Esports",
  "Team Awareness_PSG", "Team Awareness_Red Bull",
  "Team Awareness_Team Liquid", "Team Awareness_Team Vitality",
  "Team Awareness_None of these"
)

all_team_cols <- grep("^Team Awareness_", names(df), value = TRUE)
cols_to_drop <- setdiff(all_team_cols, keep)
df[, (cols_to_drop) := NULL]
#####END - TEAM AWARENESS



######ESPORTS TOURNAMENTS
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

mapping <- read_excel("C:/Users/mira.leenders/Downloads/Esports_Tournament_Grouping_FILLED_CORRECT.xlsx")
mapping <- as.data.table(mapping)
tourn_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)
tourn_dt <- data.table(column = tourn_cols)
setnames(mapping, old = "Esports Tournament Column Name", new = "column")
tourn_dt <- merge(tourn_dt, mapping, by = "column", all.x = TRUE)

for (g in unique(tourn_dt$`Grouped Category`)) {
  group_cols <- tourn_dt[`Grouped Category` == g, column]

  safe_group_name <- gsub("[^A-Za-z0-9]", "_", g)
  new_col <- paste0("Watched_", safe_group_name)

  df[[new_col]] <- rowSums(
    as.data.frame(lapply(df[, ..group_cols], function(x) x != "0" & x != "" & !is.na(x))),
    na.rm = TRUE
  )
}

watched_group_cols <- grep("^Watched_", names(df), value = TRUE)
print(df[, ..watched_group_cols])


mapping <- read_excel("C:/Users/mira.leenders/Downloads/Esports_Tournament_Grouping_STRUCTURE_BASED.xlsx")
mapping <- as.data.table(mapping)

tourn_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)

tourn_dt <- data.table(column = tourn_cols)
setnames(mapping, old = "Esports Tournament Column Name", new = "column")
tourn_dt <- merge(tourn_dt, mapping, by = "column", all.x = TRUE)

for (g in unique(tourn_dt$`Structure-Based Grouping`)) {
  group_cols <- tourn_dt[`Structure-Based Grouping` == g, column]

  safe_group_name <- gsub("[^A-Za-z0-9]", "_", g)
  new_col <- paste0("Watched_", safe_group_name)

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

all_tournament_cols <- grep("^Specific Esports Tournaments Watched_", names(df), value = TRUE)
tournament_names <- sub("^Specific Esports Tournaments Watched_", "", all_tournament_cols)
keep_cols <- all_tournament_cols[tournament_names %in% keep_tournaments]
drop_cols <- setdiff(all_tournament_cols, keep_cols)
df[, (drop_cols) := NULL]
######END - ESPORTS TOURNAMENTS



######REASONS FOR SUPPORT
reason_cols <- grep("^Reasons for Supporting Specific Esports Teams_", names(df), value = TRUE)
print(reason_cols)

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

top_teams <- c(
  "Cloud9", "Excel Esports", "Fnatic",
  "G2 Esports", "PSG", "Red Bull",
  "T1", "Team Liquid", "Infinity", "FaZe Clan"
)

reason_cols <- grep(
  "^Reasons for Supporting Specific Esports Teams_",
  names(df), value = TRUE
)

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

colnames(df_reasons)[-1] <- reasons

df <- df %>%
  left_join(df_reasons, by = "hash")
#####END - REASONS FOR SUPPORT


#***RENAMING
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
#***END - RENAMING



#######SPENDING CATEGORY
prefixes <- c("q13", paste0("q13", letters[2:11]))  # q13, q13b, ..., q13k
all_q13_cols <- paste0(rep(prefixes, each = 4), "_", rep(1:4, times = length(prefixes)))
all_q13_cols <- intersect(all_q13_cols, names(df))  

df[, (all_q13_cols) := lapply(.SD, function(x) {
  x <- as.character(x)
  x[x %in% c("0", "", "NA")] <- NA_character_
  return(x)
}), .SDcols = all_q13_cols]

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

alle_waarden <- unlist(df[, c(cols_topic1, cols_topic2, cols_topic3, cols_topic4), with=FALSE])
ongecodeerd <- setdiff(unique(alle_waarden[!is.na(alle_waarden)]), names(mapping))
if (length(ongecodeerd) > 0) {
  warning("Onbekende categorieën aangetroffen die niet zijn omgezet: ", paste(ongecodeerd, collapse = ", "))
}

for (cols in list(cols_topic1, cols_topic2, cols_topic3, cols_topic4)) {
  mult_vals <- df[, rowSums(!is.na(.SD)), .SDcols = cols]
  if (any(mult_vals > 1, na.rm = TRUE)) {
    warning("Respondenten met meerdere waarden gevonden in kolommen: ", paste(cols, collapse = ", "))
  }
}

cols_to_remove <- grep("^q13", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
######END - Spending category

#***RENAMING
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

df <- df %>%
  rename(
    Improvement_Entertainment = `q30a`,
    Watching_Playing = `q30b`,
    Educational_Entertaining = `q30c`, 
    Stories_Gameplay  = `q30d`, 
    WatchAlone_Company = `q30e`, 
    OfficialBroadcast_Streamers  = `q30f`
  )

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

df <- df %>%
  rename(LimitedEditionPurch_EsportsTeam = `q52_1`)
df <- df %>%
  rename(LimitedEditionPurch_Streamer = `q52_2`)
df <- df %>%
  rename(ExperienceBuildingPC = `q53`)


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


setnames(df,
         old = c("Other", "None_of_these", "None of these.x", "None of these.y"),
         new = c("Betting_Other", "Betting_None of these", 
                 "ChannelAwareness_None of these", "ReasonsForSupport_None of these"))

setnames(df,
         old = c("q998_1", 
                 "q998_2", 
                 "q998_3", 
                 "q998_4", 
                 "q105"),
         new = c("GamingSpend_NewVideoGames", 
                 "GamingSpend_SubscriptionsGamingServices", 
                 "GamingSpend_Donations/Subscriptions.Streamers/ContentCreators", 
                 "GamingSpend_Content/in-game purchases", 
                 "AgeInterval"))



#***REMOVING
cols_to_remove <- grep("^WatchingBehavior", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Reasons for Supporting Specific Esports Teams_", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Channel Awareness_", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Content Type Watched on Each Channel_", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Merchandise Purchase History", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^PC Brand Awareness", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^PC Brand Ownership", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Brands Best Suited to Gaming", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Consumption Habits", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Food Categories During", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Drink Categories During", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Categories Betted On", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Categories Regularly Betted On", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]
cols_to_remove <- grep("^Games Played in Past Year", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]

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
q65_all <- grep("^q65_[0-9]+$", names(df), value = TRUE)
drop_cols <- setdiff(q65_all, names(keep_map))
df[, (drop_cols) := NULL]
setnames(df, old = names(keep_map), new = keep_map)
cols_to_remove <- grep("^Games Watched Non-esports Content_", names(df), value = TRUE)
df[, (cols_to_remove) := NULL]


###NAAR BINAIR OMZETTEN
cols_to_convert <- names(df)[sapply(df, function(x) {
  vals <- unique(na.omit(x))
  length(vals) == 2 && any(vals == "0") && any(vals != "0")
})]

df[, (cols_to_convert) := lapply(.SD, function(x) {
  ifelse(x == "0", 0L, 1L)
}), .SDcols = cols_to_convert]

###LEEG NAAR 0
cols_with_empty <- names(df)[sapply(df, function(x) any(x == "", na.rm = TRUE))]
df[, (cols_with_empty) := lapply(.SD, function(x) {
  x[x == ""] <- 0
  x
}), .SDcols = cols_with_empty]
df[is.na(df)] <- 0

###HAAL NR WEG VAN AANTAL BRANDS
confidence_cols <- c("very_unconfident", "unconfident", "neutral", "confident", "very_confident")
df[, (confidence_cols) := lapply(.SD, function(x) {
  gsub(":\\s*[0-9]+", "", x)  # removes ": 1", ":2", etc.
}), .SDcols = confidence_cols]


###HAAL NUMMER WEG NA OTHER
team_reason_cols <- c(
  "I_like_the_team_because_of_their_performances_in_a_particular_competition_game",
  "I_like_the_influencers_or_brand_ambassadors_that_are_part_of_the_team",
  "I_am_only_a_fan_of_a_particular_player_on_the_team",
  "I_like_the_team_s_overall_branding__values__and_messaging",
  "I_like_that_the_team_represents_me_in_some_way",
  "I_like_the_story_or_narrative_behind_the_team",
  "I_like_the_attitude_of_the_players_on_the_team",
  "I_am_a_fan_of_the_team_s_content__video__web__social_media_",
  "I_was_influenced_by_family_or_friends_to_become_a_fan_of_the_team",
  "I_was_originally_only_a_fan_of_a_particular_player__but_now_I_am_also_a_fan_of_the_team",
  "I_like_the_team_s_community_and_what_they_do_for_their_community"
)

df[, (team_reason_cols) := lapply(.SD, function(x) {
  gsub("(Other):\\s*[0-9]+", "\\1", x)  # keeps "Other", removes ": 1"
}), .SDcols = team_reason_cols]


####MAAK ENKELE KOLOMMEN NUMERISCH
df[, gender := fifelse(gender == "Male", 0L,
                       fifelse(gender == "Female", 1L, NA_integer_))]

df[, RankedPlay := fifelse(RankedPlay == "No", 0L,
                           fifelse(RankedPlay == "Yes", 1L, NA_integer_))]

describe_cols <- names(df)[sapply(df, function(x) {
  vals <- unique(na.omit(x))
  length(vals) == 2 &&
    all(vals %in% c("Describes me", "Does not describe me"))
})]

df[, (describe_cols) := lapply(.SD, function(x) {
  fifelse(x == "Describes me", 1L,
          fifelse(x == "Does not describe me", 0L, NA_integer_))
}), .SDcols = describe_cols]

df[, ExperienceBuildingPC := fifelse(ExperienceBuildingPC == "Yes", 1L,
                                     fifelse(ExperienceBuildingPC == "No", 0L,
                                             fifelse(ExperienceBuildingPC == "0", NA_integer_, NA_integer_)))]

df[, YourDecision_Internetprovider := fifelse(YourDecision_Internetprovider == "Yes", 1L,
                                              fifelse(YourDecision_Internetprovider == "No", 0L, NA_integer_))]

df[, ImportanceGamingHabits_InternetProvider := fifelse(ImportanceGamingHabits_InternetProvider == "Yes", 1L,
                                                        fifelse(ImportanceGamingHabits_InternetProvider == "No", 0L, NA_integer_))]

gaming_spend_cols <- grep("^GamingSpend_", names(df), value = TRUE)

df[, (gaming_spend_cols) := lapply(.SD, function(x) {
  fifelse(x == "0", 0L,
          fifelse(x == "Low", 1L,
                  fifelse(x == "Medium", 2L,
                          fifelse(x == "High", 3L, NA_integer_))))
}), .SDcols = gaming_spend_cols]

str(df[, c("RankedPlay", "ExperienceBuildingPC",
           "YourDecision_Internetprovider", "ImportanceGamingHabits_InternetProvider", "GamingSpend_NewVideoGames"), with = FALSE])




####AANTAL 0's in dataset
zero_counts <- sapply(df, function(col) sum(col == 0, na.rm = TRUE))
top_30 <- sort(zero_counts, decreasing = TRUE)[1:30]
top_30_dt <- data.table(
  Column = names(top_30),
  Zero_Counts = as.integer(top_30)
)
print(top_30_dt)

zero_counts_dt <- data.table(
  Column = names(zero_counts),
  Zero_Counts = as.integer(zero_counts)
)
print(zero_counts_dt)

zero_na_counts <- sapply(df, function(col) sum(col == 0 | is.na(col), na.rm = TRUE))
zero_na_dt <- data.table(
  Column = names(zero_na_counts),
  Zero_OR_NA_Count = as.integer(zero_na_counts)
)
write_xlsx(zero_na_dt, path = "zero_or_na_counts.xlsx")




###################CORR MATRIX
numeric_like_cols <- names(df)[sapply(df, function(col) {
  all(grepl("^\\s*[0-9]+\\s*$", na.omit(col)))  
})]

df[, (numeric_like_cols) := lapply(.SD, function(x) as.numeric(trimws(x))), .SDcols = numeric_like_cols]
numeric_count <- sum(sapply(df, is.numeric))
categorical_count <- ncol(df) - numeric_count
cat("Columns converted to numeric:", length(numeric_like_cols), "\n")
cat("Total numeric columns now:", numeric_count, "\n")
cat("Total categorical (non-numeric) columns:", categorical_count, "\n")

numeric_df <- df[, which(sapply(df, is.numeric)), with = FALSE]
numeric_df <- numeric_df[, which(sapply(numeric_df, function(x) !all(is.na(x)) && length(unique(x)) > 1)), with = FALSE]
cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
print(cor_matrix[1:5, 1:5])  
cor_df <- as.data.frame(cor_matrix)
cor_df <- cbind(Column = rownames(cor_df), cor_df) 
write_xlsx(cor_df, path = "correlation_matrix.xlsx")

cor_long <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")
setDT(cor_long)
cor_long[, Color := cut(Correlation,
                        breaks = c(-1, -0.7, -0.5, -0.3, 0.3, 0.5, 0.7, 1),
                        labels = c("Red", "Orange", "Yellow", "White", "Yellow", "Orange", "Red"),
                        include.lowest = TRUE)]
write_xlsx(cor_long, "correlation_matrix_colourcoded.xlsx")


####UNIQUE VALUES PER COLUMN
unique_counts <- sapply(df, function(x) length(unique(na.omit(x))))
unique_counts_dt <- data.table(
  Column = names(unique_counts),
  Unique_Value_Count = as.integer(unique_counts)
)
write_xlsx(unique_counts_dt, "unique_values.xlsx")


####NEAR ZERVO VARIANCE COLUMNS
nzv <- nearZeroVar(df, saveMetrics = TRUE)
nzv_dt <- as.data.table(nzv, keep.rownames = "Column")
write_xlsx(nzv_dt, path = "near_zero_variance_metrics.xlsx")


####EXPORT DATASET
duplicated_names <- names(df)[duplicated(names(df))]
print(duplicated_names)
names(df) <- trimws(names(df))
names(df) <- gsub("[^A-Za-z0-9_]", "_", names(df))
names(df) <- make.unique(names(df), sep = "_dup_")
write_xlsx(df, path = "Dataset_Cleaned.xlsx")
fwrite(df, file = "Final_Dataset.csv")
write.csv(df, file = "Final_Dataset.csv", row.names = FALSE)




#####DESCRIPTIVE VISUALISATIONS CLEANED VS RAW
#########CLEANED
rows <- nrow(df)
cols <- ncol(df)
df_dims <- data.frame(
  Dimensie = c("Aantal rijen", "Aantal kolommen"),
  Aantal = c(rows, cols)
)

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


#######RAW
df2 <- fread(
  "C:/Users/mira.leenders/OneDrive - Obasi/Documenten/R studio/DATASET THESIS.csv",
  stringsAsFactors = FALSE
)

rows2 <- nrow(df2)
cols2 <- ncol(df2)

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

####CLEANED
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

type_counts <- type_table[, .N, by = Type]
ggplot(type_counts, aes(x = reorder(Type, -N), y = N, fill = Type)) +
  geom_col() +
  geom_text(aes(label = N), vjust = -0.3, size = 4) +
  labs(title = "Aantal kolommen per datatype",
       x = "Datatype",
       y = "Aantal kolommen") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, max(type_counts$N) + 10))


##############CHANGE DATA TYPE
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

pattern <- paste0("^(", paste0(prefixes, collapse = "|"), ")")
cols_to_convert <- grep(pattern, names(df), value = TRUE)

df[, (cols_to_convert) := lapply(.SD, function(x) {
  x <- as.character(x)
  fifelse(x == "0" | x == "", 0L, 1L)
}), .SDcols = cols_to_convert]

summary(df[, ..cols_to_convert])


#########################VARIABILITEIT
kolommen_zonder_hash <- setdiff(names(df), "hash")

variability <- data.table(
  Kolom = kolommen_zonder_hash,
  AantalUniek = sapply(df[, ..kolommen_zonder_hash], function(x) length(unique(x)))
)

variability <- variability[order(-AantalUniek)]

head(variability, 20)

ggplot(variability[1:20], aes(x = reorder(Kolom, -AantalUniek), y = AantalUniek)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Aantal unieke waarden per kolom (top 20, zonder 'hash')",
       x = "Kolomnaam", y = "Aantal unieke waarden") +
  theme_minimal()

variability <- data.table(
  Kolom = setdiff(names(df), "hash"),
  AantalUniek = sapply(df[, !("hash"), with=FALSE], function(x) length(unique(x)))
)

variability[, Variabiliteit := fifelse(AantalUniek <= 2, "Binair (≤2)",
                                       fifelse(AantalUniek <= 5, "Laag (3–5)",
                                               fifelse(AantalUniek <= 10, "Gemiddeld (6–10)", "Hoog (>10)")))]

ggplot(variability[, .N, by=Variabiliteit], aes(x=reorder(Variabiliteit, -N), y=N, fill=Variabiliteit)) +
  geom_col() +
  geom_text(aes(label=N), vjust=-0.5) +
  labs(title = "Aantal kolommen per niveau van variabiliteit",
       x = "Categorie variabiliteit", y = "Aantal kolommen") +
  theme_minimal() +
  theme(legend.position = "none")




