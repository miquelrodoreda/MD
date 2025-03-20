# MD: Data Mining Group Project

## Overview
This repository contains the collaborative work for the Data Mining (Mineria de Dades) course project. Our objective is to analyze a dataset of restaurants to uncover patterns and insights related to various restaurant attributes.

## Project Structure
The project is organized into the following directories:

### Data and Analysis
- **dataset/**: Contains the original and processed datasets used for analysis
- **scripts/**: Contains all R scripts used for analysis and data processing

### Analysis Outputs
- **univariant_before/**: Outputs for univariate analysis before data preprocessing
- **univariant_after/**: Outputs for univariate analysis after data preprocessing
- **bivariant_before/**: Outputs for bivariate analysis before data preprocessing
- **bivariant_after/**: Outputs for bivariate analysis after data preprocessing
- **clustering/**: Contains scripts and results related to clustering analysis
- **pca/**: Contains scripts and results related to Principal Component Analysis (PCA)

## Analysis Scripts
All R scripts are located in the `scripts/` directory:

### Data Processing
- **preprocessing.R**: Handles data cleaning and preprocessing tasks
- **data_cleaning.R**: Contains functions for cleaning and preparing the dataset
- **renameColumns.R**: Handles column renaming and standardization
- **metadata.R**: Manages metadata information for the project

### Analysis
- **univariantAnalysis.R**: Performs univariate analysis on the dataset
- **bivariantAnalysis.R**: Conducts bivariate analysis to explore relationships between variables
- **bivariantAnalysisPDF.R**: Generates a PDF containing all bivariate analysis plots
- **clustering.R**: Implements clustering algorithms to identify groups within the data
- **pca.R**: Performs Principal Component Analysis to reduce data dimensionality and identify key components

## Getting Started

### Prerequisites
1. **R Installation**
   - Download and install R from [CRAN](https://cran.r-project.org/)

2. **Required R Packages**
   ```R
   install.packages(c("psych", "RColorBrewer", "ggplot2", "dplyr"))
   ```

### Running the Analysis
1. Clone the repository:
   ```bash
   git clone https://github.com/miquelrodoreda/MD.git
   ```

2. Navigate to the `scripts/` directory
3. Execute the desired script in your R environment to perform specific analyses

## Project Information
- **Contributors**: Data Mining course group members
- **License**: MIT License (see [LICENSE](LICENSE) file for details)

---

*Note: This README provides an overview of the project structure and guidance on how to get started. For detailed explanations of each analysis and findings, please refer to the respective scripts and output files in the repository.* 