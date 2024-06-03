# Parasites Project

This repository contains a project focused on parasite analysis in feces, divided into two main parts:

### Part 1: Parasitos_heces
The "Parasitos_heces" folder includes all the necessary structure for folder generation, and inside the `resources` folder, all the images that participants needed to annotate. This section of the project is programmed in Java.

### Part 2: Parasite_Analysis
The "Parasite_Analysis" folder contains all the statistical analysis conducted in R. Within this folder, there is a subfolder named "datasets_R", which contains the two data tables initially used for performing all calculations, available for anyone interested in replicating or testing the analysis.

### Project Structure

- `Parasitos_heces/`
  - `resources/`
    - *Images for annotation*
  - `src/`
    - *Java source code for folder generation and other functionalities*
  - `experimento_prueba1_06-03-24/`
    - *Sample folder generated to show how annotation folders look*

- `Parasite_Analysis/`
  - `datasets_R/`
    - *Data tables for analysis in R*
  - `calculate_iou.R`
    - *Function to calculate IoU*
  - `calculate_measures.R`
    - *Function to calculate precision, recall, and F1 score*
  - `consensus_function.R`
    - *Function to calculate consensus annotations using two different strategies*
  - `extract_info.R`
    - *Function to extract annotation data*
  - `main.R`
    - *Main script to run the analysis*
  - `process_fenerated_folders.R`
    - *Function to extract all data from `process_xml`*
  - `process_xml.R`
    - *Function to read all data from XML files*

### Requirements

- **Java**: Required to execute the folder and image generation part.
- **R**: Required to perform the statistical analysis.

### Usage

To use this project, follow these steps:

1. **Java**: Navigate to the "Parasitos_heces" folder and run the Java code in the `src` directory to generate the folders and load the images.
2. **R**: Navigate to the "Parasite_Analysis" folder and use the R scripts to conduct the statistical analysis. The necessary data tables are located in "datasets_R".
