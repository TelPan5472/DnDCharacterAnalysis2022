# setting wd
setwd("~/CAREER/Google Certification")

# from github: downloading the data
# devtools::install_github('oganm/dnddata')
library(dnddata)

# checking data
??tables
head(dnd_chars_unique_list)
head(dnd_chars_all)

# for the sake of simplicity, I'll focus on single class characters
dndf <- dnddata::dnd_chars_singleclass
# head(dndf)

# let's start with simple summaries/cleanup

# number of characters per class
unique(dndf$class)
unique(dndf$justClass)

# filtering out classes that are definitely homebrew
dndf_officialclasses <- dndf[dndf$justClass != "Battle Clown"
                                & dndf$justClass != "commoner"
                                & dndf$justClass != "Crafting Commoner", ]

# filtering out Mystic as well since it's not 5e official
dndf_officialclasses<- dndf_officialclasses[dndf_officialclasses$justClass != "Mystic", ]

# adding new columns called justClassClean, subclassClean
dndf_officialclasses$justClassClean <- dndf_officialclasses$justClass
dndf_officialclasses$subclassClean <- dndf_officialclasses$subclass

# let's replace "Gunslinger" with "Fighter"; subclass Gunslinger
dndf_officialclasses <- within(dndf_officialclasses, subclassClean[justClassClean == 'Gunslinger'] <- 'Gunslinger')
dndf_officialclasses <- within(dndf_officialclasses, justClassClean[justClassClean == 'Gunslinger'] <- 'Fighter')

# let's replace Revised Ranger with 
dndf_officialclasses <- within(dndf_officialclasses, justClassClean[justClassClean == 'Revised Ranger'] <- 'Ranger')

unique(dndf_officialclasses$justClassClean)

# let's check lineages
unique(dndf_officialclasses$race)
unique(dndf_officialclasses$processedRace)
# we'll used processedRace for our analyses related to lineage

# Yuan-Ti was misspelled so this line corrects the spelling
dndf_officialclasses <- within(dndf_officialclasses, processedRace[processedRace == 'Yaun-Ti'] <- 'Yuan-Ti')

# also Eladrin are technically elves, so changing that processedRace to be Elf
dndf_officialclasses <- within(dndf_officialclasses, processedRace[processedRace == 'Eladrin'] <- 'Elf')

# since processedRace looks to use official races and flags custom as separate
library(psych)

describe(dndf_officialclasses$Str)
describe(dndf_officialclasses$Dex)
describe(dndf_officialclasses$Con)
describe(dndf_officialclasses$Cha)
describe(dndf_officialclasses$Int)
describe(dndf_officialclasses$Wis)

# singled out the str 103 character for fun
# swole_frend <- dndf_officialclasses[dndf_officialclasses$Str==103, ]
# write.csv(swole_frend,"~/CAREER/Google Certification/swole_frend.csv", row.names = FALSE)

# need to filter out scores of 0 or greater than 30
# it's unlikely that characters have above 20, but some special items/class boons let you have score >20
dndf_clean <- dndf_officialclasses[dndf_officialclasses$Str >= 1 & dndf_officialclasses$Str <= 30, ]
dndf_clean <- dndf_clean[dndf_clean$Dex >= 1 & dndf_clean$Dex <= 30, ]
dndf_clean <- dndf_clean[dndf_clean$Con >= 1 & dndf_clean$Con <= 30, ]
dndf_clean <- dndf_clean[dndf_clean$Int >= 1 & dndf_clean$Int <= 30, ]
dndf_clean <- dndf_clean[dndf_clean$Wis >= 1 & dndf_clean$Wis <= 30, ]
dndf_clean <- dndf_clean[dndf_clean$Cha >= 1 & dndf_clean$Cha <= 30, ]

describe(dndf_clean$Str)
describe(dndf_clean$Dex)
describe(dndf_clean$Con)
describe(dndf_clean$Cha)
describe(dndf_clean$Int)
describe(dndf_clean$Wis)

str(dndf_clean)

# adding mods for scores
library(dplyr)
dndf_clean <- dndf_clean %>% mutate(StrMod =
                     case_when(Str <= 1 ~ -5, 
                               Str <= 3 ~ -4,
                               Str <= 5 ~ -3,
                               Str <= 7 ~ -2,
                               Str <= 9 ~ -1,
                               Str <= 11 ~ 0,
                               Str <= 13 ~ 1,
                               Str <= 15 ~ 2,
                               Str <= 17 ~ 3,
                               Str <= 19 ~ 4,
                               Str <= 21 ~ 5,
                               Str <= 23 ~ 6,
                               Str <= 25 ~ 7,
                               Str <= 27 ~ 8,
                               Str <= 29 ~ 9,
                               Str >= 30 ~ 10)
)
dndf_clean <- dndf_clean %>% mutate(DexMod =
                                      case_when(Dex <= 1 ~ -5, 
                                                Dex <= 3 ~ -4,
                                                Dex <= 5 ~ -3,
                                                Dex <= 7 ~ -2,
                                                Dex <= 9 ~ -1,
                                                Dex <= 11 ~ 0,
                                                Dex <= 13 ~ 1,
                                                Dex <= 15 ~ 2,
                                                Dex <= 17 ~ 3,
                                                Dex <= 19 ~ 4,
                                                Dex <= 21 ~ 5,
                                                Dex <= 23 ~ 6,
                                                Dex <= 25 ~ 7,
                                                Dex <= 27 ~ 8,
                                                Dex <= 29 ~ 9,
                                                Dex >= 30 ~ 10)
)
dndf_clean <- dndf_clean %>% mutate(ConMod =
                                      case_when(Con <= 1 ~ -5, 
                                                Con <= 3 ~ -4,
                                                Con <= 5 ~ -3,
                                                Con <= 7 ~ -2,
                                                Con <= 9 ~ -1,
                                                Con <= 11 ~ 0,
                                                Con <= 13 ~ 1,
                                                Con <= 15 ~ 2,
                                                Con <= 17 ~ 3,
                                                Con <= 19 ~ 4,
                                                Con <= 21 ~ 5,
                                                Con <= 23 ~ 6,
                                                Con <= 25 ~ 7,
                                                Con <= 27 ~ 8,
                                                Con <= 29 ~ 9,
                                                Con >= 30 ~ 10)
)
dndf_clean <- dndf_clean %>% mutate(IntMod =
                                      case_when(Int <= 1 ~ -5, 
                                                Int <= 3 ~ -4,
                                                Int <= 5 ~ -3,
                                                Int <= 7 ~ -2,
                                                Int <= 9 ~ -1,
                                                Int <= 11 ~ 0,
                                                Int <= 13 ~ 1,
                                                Int <= 15 ~ 2,
                                                Int <= 17 ~ 3,
                                                Int <= 19 ~ 4,
                                                Int <= 21 ~ 5,
                                                Int <= 23 ~ 6,
                                                Int <= 25 ~ 7,
                                                Int <= 27 ~ 8,
                                                Int <= 29 ~ 9,
                                                Int >= 30 ~ 10)
)
dndf_clean <- dndf_clean %>% mutate(WisMod =
                                      case_when(Wis <= 1 ~ -5, 
                                                Wis <= 3 ~ -4,
                                                Wis <= 5 ~ -3,
                                                Wis <= 7 ~ -2,
                                                Wis <= 9 ~ -1,
                                                Wis <= 11 ~ 0,
                                                Wis <= 13 ~ 1,
                                                Wis <= 15 ~ 2,
                                                Wis <= 17 ~ 3,
                                                Wis <= 19 ~ 4,
                                                Wis <= 21 ~ 5,
                                                Wis <= 23 ~ 6,
                                                Wis <= 25 ~ 7,
                                                Wis <= 27 ~ 8,
                                                Wis <= 29 ~ 9,
                                                Wis >= 30 ~ 10)
)
dndf_clean <- dndf_clean %>% mutate(ChaMod =
                                      case_when(Cha <= 1 ~ -5, 
                                                Cha <= 3 ~ -4,
                                                Cha <= 5 ~ -3,
                                                Cha <= 7 ~ -2,
                                                Cha <= 9 ~ -1,
                                                Cha <= 11 ~ 0,
                                                Cha <= 13 ~ 1,
                                                Cha <= 15 ~ 2,
                                                Cha <= 17 ~ 3,
                                                Cha <= 19 ~ 4,
                                                Cha <= 21 ~ 5,
                                                Cha <= 23 ~ 6,
                                                Cha <= 25 ~ 7,
                                                Cha <= 27 ~ 8,
                                                Cha <= 29 ~ 9,
                                                Cha >= 30 ~ 10)
)

str(dndf_clean)

# adding columns for each skill proficiency as a binary
dndf_clean$Acrobatics <- ifelse(grepl(
  "Acrobatics", dndf_clean$skills), 
                                1, 0)
dndf_clean$AnimalHandling <- ifelse(grepl(
  "Animal Handling", dndf_clean$skills), 
  1, 0)
dndf_clean$Arcana <- ifelse(grepl(
  "Arcana", dndf_clean$skills), 
  1, 0)
dndf_clean$Athletics <- ifelse(grepl(
  "Athletics", dndf_clean$skills), 
  1, 0)
dndf_clean$Deception <- ifelse(grepl(
  "Deception", dndf_clean$skills), 
  1, 0)
dndf_clean$History <- ifelse(grepl(
  "History", dndf_clean$skills), 
  1, 0)
dndf_clean$Insight <- ifelse(grepl(
  "Insight", dndf_clean$skills), 
  1, 0)
dndf_clean$Intimidation <- ifelse(grepl(
  "Intimidation", dndf_clean$skills), 
  1, 0)
dndf_clean$Investigation <- ifelse(grepl(
  "Investigation", dndf_clean$skills), 
  1, 0)
dndf_clean$Medicine <- ifelse(grepl(
  "Medicine", dndf_clean$skills), 
  1, 0)
dndf_clean$Nature <- ifelse(grepl(
  "Nature", dndf_clean$skills), 
  1, 0)
dndf_clean$Perception <- ifelse(grepl(
  "Perception", dndf_clean$skills), 
  1, 0)
dndf_clean$Performance <- ifelse(grepl(
  "Performance", dndf_clean$skills), 
  1, 0)
dndf_clean$Persuasion <- ifelse(grepl(
  "Persuasion", dndf_clean$skills), 
  1, 0)
dndf_clean$Religion <- ifelse(grepl(
  "Religion", dndf_clean$skills), 
  1, 0)
dndf_clean$SleightOfHand <- ifelse(grepl(
  "Sleight", dndf_clean$skills), 
  1, 0)
dndf_clean$Stealth <- ifelse(grepl(
  "Stealth", dndf_clean$skills), 
  1, 0)
dndf_clean$Survival <- ifelse(grepl(
  "Survival", dndf_clean$skills), 
  1, 0)

str(dndf_clean)

# adding columns for # of proficiencies with certain abilities
dndf_clean$StrSkills <- dndf_clean$Athletics
dndf_clean$DexSkills <- dndf_clean$Acrobatics + dndf_clean$SleightOfHand + dndf_clean$Stealth
dndf_clean$IntSkills <- dndf_clean$Arcana + dndf_clean$History + dndf_clean$Investigation + dndf_clean$Nature  + dndf_clean$Religion
dndf_clean$WisSkills <- dndf_clean$AnimalHandling + dndf_clean$Insight + dndf_clean$Medicine + dndf_clean$Perception  + dndf_clean$Survival
dndf_clean$ChaSkills <- dndf_clean$Deception + dndf_clean$Intimidation + dndf_clean$Performance + dndf_clean$Persuasion
dndf_clean$ConSkills <- 0
dndf_clean$TotProfSkills <- dndf_clean$ConSkills+dndf_clean$ChaSkills+dndf_clean$WisSkills+dndf_clean$IntSkills+dndf_clean$StrSkills+dndf_clean$DexSkills

# let's filter out blank lineage though
dndf_clean <- dndf_clean[dndf_clean$processedRace != "", ]

# let's check level
unique(dndf_clean$level) # looks good

# let's check background; it's a lot...will have to clean later
unique(dndf_clean$background)

# choice and background have extra complications...will figure out later
# also feats would require an additional join for what kind of feats, so for simplicity we'll exclude those

# save results to csv file and then analyse in Power BI!
write.csv(dndf_clean,"~/CAREER/Google Certification/dndf_clean_UPDATED.csv", row.names = FALSE)

