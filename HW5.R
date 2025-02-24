################################################################################
# HW 5
# Yuliia Heleveria
# MATH 240 - Spring'25
################################################################################

################################################################################
# Load libraries
################################################################################
library("tidyverse")
library("jsonlite")
library("stringr")

################################################################################
# Step 1 - Working with Au Revoir (Adios)
################################################################################

#Part1 - creating an object
current.filename <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

#Part2 - split the string
#remove trailing .json
removed.json.filename <- str_sub(current.filename, start = 1L, end = -6L)
split.curr.filename <- str_split(removed.json.filename, pattern = '-', simplify =T) #split string
#extract the artist
curr.artist <- split.curr.filename[1]
#extract the album
curr.album <- split.curr.filename[2]
#extract the track
curr.track <- split.curr.filename[3]

#Part3 - load JSON file
open.file <- paste("EssentiaOutput", current.filename, sep = '/')
file.load <- fromJSON(open.file)

#Part4 - extract song characteristic
#extract overall loudness
file.overall.loudnes <- file.load$lowlevel$loudness_ebu128$integrated
#extract spectral energy
file.spectral.enegry <- file.load$lowlevel$spectral_energy$mean
#extract dissonance
file.dissonance <- file.load$lowlevel$dissonance$mean
#extract pitch salience
file.pitch.salience <- file.load$lowlevel$pitch_salience$mean
#extract tempo in beats per minute
file.bpm <- file.load$rhythm$bpm
#extract beats loudness
file.beats.loudness <- file.load$rhythm$beats_loudness$mean
#extract danceability
file.danceability <- file.load$rhythm$danceability
#extract tuning frequency
file.tuning.frequency <- file.load$tonal$tuning_frequency

################################################################################
# Step 2 - Load and clean data from Essentia models for each .JSON file
################################################################################

#load all files from EssentiaOutput
all.files <- list.files("EssentiaOutput")
#subset all .json files
json.count <- str_count(all.files, pattern = ".json")
json.files <- subset(all.files, json.count>0)
#create the data frame
times.to.repeat = length(json.files)
song.data <- data.frame(artist = rep(NA, times.to.repeat),
                        album = rep(NA, times.to.repeat),
                        track = rep(NA, times.to.repeat),
                        overall.Loudness = rep(NA, times.to.repeat),
                        spectral.Energy = rep(NA, times.to.repeat),
                        dissonance = rep(NA, times.to.repeat),
                        pitch.salience = rep(NA, times.to.repeat),
                        tempo.in.beats.per.minute = rep(NA, times.to.repeat),
                        beat.Loudness = rep(NA, times.to.repeat),
                        danceability = rep(NA, times.to.repeat),
                        tuning.frequency = rep(NA, times.to.repeat),
                        stringsAsFactors = FALSE)
#complete Step1 for all .json files
for (i in 1:times.to.repeat){
  #current file
  curr.file = json.files[i]
  #remove trailing .json
  removed.json.file <- str_sub(curr.file, start = 1L, end = -6L)
  split.curr.file <- str_split(removed.json.file, pattern = '-', simplify =T) #split string
  #extract the artist
  current.artist <- split.curr.file[1]
  song.data$artist[i] <- current.artist
  #extract the album
  current.album <- split.curr.file[2]
  song.data$album[i] <- current.album
  #extract the track
  current.track <- split.curr.file[3]
  song.data$track[i] <- current.track
  open.curr.file <- paste("EssentiaOutput", curr.file, sep = '/')
  curr.file.load <- fromJSON(open.curr.file)
  #extract song characteristic
  #extract overall loudness
  overall.loudnes <- curr.file.load$lowlevel$loudness_ebu128$integrated
  song.data$overall.Loudness[i] <- overall.loudnes
  #extract spectral energy
  spectral.enegry <- curr.file.load$lowlevel$spectral_energy$mean
  song.data$spectral.Energy[i] <- spectral.enegry
  #extract dissonance
  dissonance <- curr.file.load$lowlevel$dissonance$mean
  song.data$dissonance[i] <- dissonance
  #extract pitch salience
  pitch.salience <- curr.file.load$lowlevel$pitch_salience$mean
  song.data$pitch.salience[i] <- pitch.salience
  #extract tempo in beats per minute
  bpm <- curr.file.load$rhythm$bpm
  song.data$tempo.in.beats.per.minute[i] <- bpm
  #extract beats loudness
  beats.loudness <- curr.file.load$rhythm$beats_loudness$mean
  song.data$beat.Loudness[i] <- beats.loudness
  #extract danceability
  danceability <- curr.file.load$rhythm$danceability
  song.data$danceability[i] <-danceability
  #extract tuning frequency
  tuning.frequency <- curr.file.load$tonal$tuning_frequency
  song.data$tuning.frequency[i] <- tuning.frequency
}

################################################################################
# Step 3 - Load and clean EssentiaModelOutput.csv
################################################################################
#Part1 - load csv file
essentia.model.output <- read_csv("EssentiaOutput/EssentiaModelOutput.csv")

essentia.model.output <- essentia.model.output %>%
  #Part2 - add valance and arousal columns
  #create column for valence by taking a mean of three columns
  mutate(valence = rowMeans(select(., deam_valence, emo_valence, muse_valence))) %>%
  #create column for arousal by taking a mean of three columns
  mutate(arousal = rowMeans(select(., deam_arousal, emo_arousal, muse_arousal))) %>%
  #Part3 - new mood columns with features
  #create column for aggressive by taking a mean of two columns
  mutate(aggressive = rowMeans(select(., eff_aggressive, nn_aggressive))) %>%
  #create column for happy by taking a mean of two columns
  mutate(happy = rowMeans(select(., eff_happy, nn_happy))) %>%
  #create column for party by taking a mean of two columns
  mutate(party = rowMeans(select(., eff_party, nn_party))) %>%
  #create column for relaxed by taking a mean of two columns
  mutate(relaxed = rowMeans(select(., eff_relax, nn_relax))) %>%
  #create column for sad by taking a mean of two columns
  mutate(sad = rowMeans(select(., eff_sad, nn_sad)))
  



#Part4 - acoustic and electric averaging
output.csv$acoustic = rep(NA, times.to.repeat)
output.csv$electric = rep(NA, times.to.repeat)
#calculate the value for each row
for (i in 1:times.to.repeat){
  #compute acoustic
  average.acoustic <- output.csv$eff_acoustic[i] + output.csv$nn_acoustic[i]
  average.acoustic <- average.acoustic/2
  output.csv$acoustic[i] <- average.acoustic #input acoustic
  #compute electric 
  average.electric <- output.csv$eff_electronic[i] + output.csv$nn_electronic[i]
  average.electric <- average.electric/2
  output.csv$electric[i] <- average.electric #input electric
}

#Part5 - compute instrumental
output.csv$instrumental = rep(NA, times.to.repeat)
#calculate the value for each row
for (i in 1:times.to.repeat){
  average.instrumental <- output.csv$eff_instrumental[i] + output.csv$nn_instrumental[i]
  average.instrumental <- average.instrumental/2
  output.csv$instrumental[i] <- average.instrumental #input instrumental
}

#Part6 - rename a column
col.to.name <- colnames(output.csv)[colnames(output.csv) == "eff_timbre_bright"] #get the column to rename
colnames(output.csv)[colnames(output.csv) == col.to.name] <- "timbreBright" #rename the column

#Part7 - delete columns
output.csv <- output.csv[ , c("artist", "album", "track", "valence", "arousal", "aggressive",
                              "happy", "party", "relaxed", "sad", "acoustic", "electric",
                              "instrumental", "timbreBright")]

################################################################################
# Step 4 - Load LIWC data and compile full dataset
################################################################################
#Part 1 - load csv file
lyrics.analysis <- read.csv("LIWCOutput/LIWCOutput.csv")

#Part 2 - merge the data
common.columns <- c("artist", "album", "track")
merged.two <- merge(song.data, output.csv, #merge first two data frames
                    by = c("artist", "album", "track"), all.x = T, all.y = T)
merges.three <- merge(merged.two, lyrics.analysis, #merge all data frames
                      by = c("artist", "album", "track"), all.x = T, all.y = T)

#Part 3 - rename function column
column.to.name <- colnames(merges.three)[colnames(merges.three) == "function."] #get the column to rename
colnames(merges.three)[colnames(merges.three) == column.to.name] <- "funct"

