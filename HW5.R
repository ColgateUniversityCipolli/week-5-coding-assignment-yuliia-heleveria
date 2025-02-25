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
#create a tibble to store object properties
au.revoir.features <- tibble() 

#Part1 - creating an object
au.revoir.filename <- "The Front Bottoms-Talon Of The Hawk-Au Revoir (Adios).json"

#Part2 - split the string
split.au.revoir.filename <- au.revoir.filename |>
  str_sub(start = 1L, end = -6L)|> #remove trailing .json
  str_split(pattern = '-', simplify =T) #split string

#extract artist, album, and song - input them into tibble
au.revoir.features <- au.revoir.features|>
  mutate(artist = split.au.revoir.filename[1]) |> #extract the artist
  mutate(albumn = split.au.revoir.filename[2]) |> #extract the album
  mutate(track = split.au.revoir.filename[3]) |> #extract the track
  #create a new row will all values
  add_row(artist = split.au.revoir.filename[1], albumn = split.au.revoir.filename[2],
          track = split.au.revoir.filename[3])


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
  mutate(sad = rowMeans(select(., eff_sad, nn_sad))) %>%
  #Part4 - acoustic and electric averaging
  #create column for acoustic by taking a mean of two columns
  mutate(acoustic = rowMeans(select(., eff_acoustic, nn_acoustic))) %>%
  #create column for electric sound by taking a mean of two columns
  mutate(electric = rowMeans(select(., eff_electronic, nn_electronic))) %>%
  #Part5 - compute instrumental
  #create column for instrumental by taking a mean of two columns
  mutate(instrumental = rowMeans(select(., eff_instrumental, nn_instrumental))) %>%
  #Part6 - rename eff_timbre_bright column
  rename(timbreBright = eff_timbre_bright) %>%
  #Part7 - retained created features and columns for artists, album, and track
  select(artist, album, track, valence, arousal, aggressive,
           happy, party, relaxed, sad, acoustic, electric,
           instrumental, timbreBright)

################################################################################
# Step 4 - Load LIWC data and compile full data set
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

