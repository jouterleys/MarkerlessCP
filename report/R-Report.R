library(devtools)
#install_github("jouterleys/v3dR")
library(v3dR)
library(tidyverse)
#install.packages("stringr")  # Install & load stringr
library(stringr)
#install.packages("rmarkdown")
# Build a function to do something on each df
loopV3DR <- function(full_filepath){
  
  # Select something for the condition.
  # In this case the first string before the first underscore
  side <- strsplit(basename(full_filepath), "_")[[1]][1]
  # Select something for the subId
  # In this case the folder name up two directories from the txt file.
  subId <- basename(dirname(dirname(full_filepath)))
  
  # Read current txt file into a dataframe and add the columns subID and side.
  df <- v3dR(full_filepath) %>%  mutate(subId = subId, side = side)
  
  return(df)
  
}

# Define path to the subject folders
resultsPath <- "C:\\Users\\Blast\\OneDrive - Queen's University\\MarkerlessCP\\subjects"

# Create list of subjects based on subject folders
subList <- list.dirs(resultsPath, recursive=FALSE)

# Build dataframe
# Creates list of txt files from within the Segment lengths folder
# Then runs the loopV3DR function on each item in the list
df <- list.files(file.path(subList,'results'), pattern = "*.txt", full.names = TRUE) %>% 
  map_df(~loopV3DR(.)) 

df$signal_names <- substring(df$signal_names, 2)

missing = which(complete.cases(df) == FALSE) # Reproduce result of Example 1 by adding == FALSE
missing <-df[missing,]


df %>%
  subset(select = -c(c3d_name, instance))%>%
  #select(signal_names,signal_types,signal_folder,signal_components,item) %>%
  #group_by(signal_names,signal_types,signal_folder,signal_components,item) %>%
  #filter(!grepl("ANGLE",signal_names))%>%
  filter(!grepl("MOMENT",signal_names))%>%
  filter(side == 'right')%>%
  filter(subId != 'markerlessAfo')%>%
  #filter(subId != 'markerbasedIK')%>%
  filter(subId != 'markerlessNoMarkers')%>%
  #filter(subId != 'markerlessMarkers')%>%
  ggplot(aes(x = item, y = value, group = subId, color = subId)) +
  geom_hline(yintercept=0,color = "black", size=0.25)+
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data="mean_sdl", fun.args = list(mult = 1), mapping = aes(color = subId, fill = subId), geom = "ribbon",alpha = 0.25,colour = NA)+
  facet_wrap(signal_components ~ signal_names, scales = "free") +
  theme_minimal()+
  theme(axis.line = element_line(size=1, colour = "black"),legend.position = "bottom")+
  scale_x_continuous(expand = c(0, 0))

#ggsave(file.path(dirname(full_filepath),paste(basename(full_filepath),'.tiff',sep = "")),
#       device = "tiff",
#       width = 8, height = 8,dpi=300)