MarkerlessCP
================
Jereme Outerleys
Last compiled on 2021-08-26

## Summary

A participant with Cerebral Palsy (CP) came into the HMRL on July 28,
2021. Overground walking gait was collected using markerless and
markerbased systems. These pilot data were helpful in assesing the
performance of a markerless system (Theia Markerless, Kingston, Ontario)
in a population where clinical gait analyses are typically performed,
with markerbased systems. In general, clinically important features of
kinematic and kinetic waveforms showed clear agreement with markerbased
data. This was particularly evident in sagittal plane kinematics and
kinetics but also the knee adduction moment. In addition, the markerless
system showed logical biomechanical difference in AFO vs. non-AFO gait
trials, where reduced plantarflexion was evident in the AFO condition.

## Collection Details

Markerless data were collected with 8 Sony RX0II cameras with a frame
rate of 120 Hz (actual 119.88 Hz). Markerbased data were collected using
11 Qualisys Oqus cameras with a frame rate of 120 Hz. The markerset used
was sufficient to build a 6DOF lower body model.

The following conditions were collected

1.  **markerlessNoMarkers**: the participant wore sport shorts, a
    t-shirt and their own sneakers.
2.  **markerlessAFO**: the participant wearing their AFOs (Ankle foot
    orthoses). Note: the AFOs are attached to a different pair of
    sneakers.
3.  **markerbased6DOF**: markerbased data collected concurrently with
    the markerlessMarkers condition. The markerset used was sufficient
    to build a 6DOF lower body model.
4.  **markerlessMarkers**: the participant with markers attached. The
    participant wore their own shoes (not AFOs) and a sports bra.
5.  **markerbasedIK**: the markerbased data with an IK model to best
    match the markerless. 3DOF hip, 3DOF knee, and 6DOF ankle.

Approximately 6 gait trials were collected per condition. Stomps were
performed prior to each gait trial to synchronize motion data with force
data. In the case of the **markerlessNoMarkers** and the
**markerbased6DOF**, all gait events (heelstrike and toeoff) were best
matched (i.e. contain all the same strides but may be shifted slightly
due to force syncing). An 8 Hz GCVSPL filter was used for both the
markerless and markerbased kinematic data. All analog data were
processed with a critically damped low pass filter with cutoff frequency
of 30 Hz. *I only included stance phase data for now.*

## Data Processing

Data were processed in Visual3D and exported using the
*Export\_Data\_To\_Ascii\_File* function. Data were subsequently
aggregated in R using the v3dR package. I will include all R libraries
and functions required to reproduce this report.

### Attach R packages

``` r
#library(devtools)
#install_github("jouterleys/v3dR")
library(v3dR)
library(tidyverse)
library(stringr)
library(here)
```

### Build loopV3DR

``` r
# Build a function to do something on each dataframe
loopV3DR <- function(full_filepath){
  
  # Add some contextual information to the dataframe
  # from each loop on import.
  
  # side information is contained in the txt filename.
  side <- strsplit(basename(full_filepath), "_")[[1]][1]
  
  # condition information is contained in the folder path
  # In this case the folder name up two directories from the txt file is the subID
  condition <- basename(dirname(dirname(full_filepath)))
  
  # Read current txt file into a dataframe and add the columns condition and side.
  df <- v3dR(full_filepath) %>%  mutate(condition = condition, side = side)
  
  return(df)
  
}
```

### Define paths and build data frame

``` r
# Define path to the subject folders
resultsPath <- here('subjects')

# Create list of subjects based on subject folders
subList <- list.dirs(resultsPath, recursive=FALSE)

# Build dataframe containing all subject data
# Creates list of all txt files from within the results folder
# Then runs the loopV3DR function on each item in the list
df <- list.files(file.path(subList,'results'), pattern = "*.txt", full.names = TRUE) %>% 
  map_df(~loopV3DR(.)) 

# The signal_names are named in visual3D as LANKLE_ANGLE, RANKLE_ANGLE, etc.
# Since we are using long data format and assign the side during import
# we will remove the first letter of the signal_name
df$signal_names <- as.factor(substring(df$signal_names, 2))

# Get factors in a specific plotting order
df$signal_names <- fct_relevel(df$signal_names, "ANKLE_ANGLE", "KNEE_ANGLE", "HIP_ANGLE","ANKLE_MOMENT", "KNEE_MOMENT", "HIP_MOMENT")

# All data were exported normalized to stance phase.
# Incomplete data is due to tracking errors, I want to 
# know this (it's hard to see sometimes in V3D and good to double check)
missing = which(complete.cases(df) == FALSE)
missing <- df[missing,]
```

## Kinematics

Data are separated by side for easier visualization. I will only include
the **markerbased6DOF** and **markerlessMarkers** for the main plot
comparisons since these data were collected concurrently.

### Right Side

``` r
df %>%
  subset(select = -c(c3d_name, instance)) %>%
  filter(!grepl("MOMENT", signal_names)) %>%
  filter(side == 'right') %>%
  filter(condition != 'markerlessAfo') %>%
  filter(condition != 'markerbasedIK') %>%
  filter(condition != 'markerlessNoMarkers')%>%
  ggplot(aes(x = item, y = value, group = condition, color = condition)) +
  geom_hline(yintercept = 0, color = "black", size = 0.25) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),
               mapping = aes(color = condition, fill = condition),
               geom = "ribbon", alpha = 0.25, colour = NA) +
  facet_wrap(signal_components ~ signal_names, scales = "free",
             labeller = labeller(signal_components = as_labeller(component_names))) +
 theme_minimal() +
 theme(axis.line = element_line(size = 1, colour = "black"), legend.position = "top") +
 scale_x_continuous(expand = c(0, 0)) +
  xlab('Percent Stance Phase (%)') +
  ylab('Joint Angle (Degrees)')
```

<div class="figure" style="text-align: center">

<img src="README_files/figure-gfm/kinematics_right-1.png" alt="Right lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion, and hip flexion are positive (+). Ankle inversion, knee adduction and hip adduction is positive (+). Toe-in, knee internal rotation, and hip internal rotation are positive (+)."  />
<p class="caption">
Right lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion,
and hip flexion are positive (+). Ankle inversion, knee adduction and
hip adduction is positive (+). Toe-in, knee internal rotation, and hip
internal rotation are positive (+).
</p>

</div>

### Left Side

<div class="figure" style="text-align: center">

<img src="README_files/figure-gfm/kinematics_left-1.png" alt="Left lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion, and hip flexion are positive (+). Ankle inversion, knee adduction and hip adduction is positive (+). Toe-in, knee internal rotation, and hip internal rotation are positive (+)."  />
<p class="caption">
Left lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion, and
hip flexion are positive (+). Ankle inversion, knee adduction and hip
adduction is positive (+). Toe-in, knee internal rotation, and hip
internal rotation are positive (+).
</p>

</div>

## Kinetics

### Right Side

<img src="README_files/figure-gfm/kinetics_left-1.png" style="display: block; margin: auto;" />

### Left Side

<img src="README_files/figure-gfm/kinetics_right-1.png" style="display: block; margin: auto;" />

#### Click Image for Video

[![overlayVideo](https://github.com/jouterleys/MarkerlessCP/blob/main/imgs/VideoThumb.PNG?raw=true)](https://player.vimeo.com/video/583583235?badge=0&autopause=0&player_id=0&app_id=58479)

## IK

Since data from the markerbased were originally modeled using full 6DOF,
I was interested in trying an IK model in Visual3D to best match the
Theia IK. 3DOF Hip, 3DOF knee, and 6DOF ankle (“Free Feet”).

### Right Side

<img src="README_files/figure-gfm/kinematics_IK_right-1.png" style="display: block; margin: auto;" />

### Left Side

<img src="README_files/figure-gfm/kinematics_IK_left-1.png" style="display: block; margin: auto;" />

## All Conditions

## Kinematics

### Right Side

<img src="README_files/figure-gfm/kinematics_all_right-1.png" style="display: block; margin: auto;" />

### Left Side

<img src="README_files/figure-gfm/kinematics_all_left-1.png" style="display: block; margin: auto;" />

## Kinetics

### Right Side

<img src="README_files/figure-gfm/kinetics_all_right-1.png" style="display: block; margin: auto;" />

### Left Side

<img src="README_files/figure-gfm/kinetics_all_left-1.png" style="display: block; margin: auto;" />

## AFOs

The participant sometimes wears AFOs when walking. The following
compares the pariticpant in the MarkerlessNoMarkers condition and the
MarkerlessAFO condition, as these are best matched in terms of clothing
with the only difference being the AFOs.

<div class="figure" style="text-align: center">

<img src="imgs/img_afo.PNG" alt="Right: the participant without AOF (markerlessNoMarkers). Left: the participant with AFOs (markerlessAfo). " width="100%" />
<p class="caption">
Right: the participant without AOF (markerlessNoMarkers). Left: the
participant with AFOs (markerlessAfo).
</p>

</div>

Only right side data presented as there weren’t sufficient clean
platform contacts for left side.

## Kinematics

### Right Side

<div class="figure" style="text-align: center">

<img src="README_files/figure-gfm/kinematics_afo_right-1.png" alt="Right lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion, and hip flexion are positive (+). Ankle inversion, knee adduction and hip adduction is positive (+). Toe-in, knee internal rotation, and hip internal rotation are positive (+)."  />
<p class="caption">
Right lower-limb joint kinematics. Note: Dorsi-flexion, knee flexion,
and hip flexion are positive (+). Ankle inversion, knee adduction and
hip adduction is positive (+). Toe-in, knee internal rotation, and hip
internal rotation are positive (+).
</p>

</div>

<img src="README_files/figure-gfm/kinetics_afo_right-1.png" style="display: block; margin: auto;" />
