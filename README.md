---
title: "README"
author: "Yingjie(Gary) Zhou, Tianye Cui, Yifan Zhao"
date: "12/15/2021"
output: html_document
    
---

# Foodborne Pathogen Detection 
## PHP 2550 Practical Data Analysis: Final Project

This project aims to track and predict the source of foodborne pathogen, Listeria, using the NCBI Pathogen Detection System.
Data source: https://www.ncbi.nlm.nih.gov/pathogens 

We propose using machine learning methods to tackle this problem. This repository contains the code, data, EDA, and results for the report: "Foodborne Pathogen Detection of Listeria Isolation Source and Seasonal/Regional Patterns using Machine Learning Models". 

The Listeria monocytogenes dataset was downloaded on Oct 23, 2022. 

- Both raw and intermediate data sets are documented under the [data](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/data) folder.

- Code and preprocessing for the EDA and Level 1 Isolation Source Analysis can be found in under the [Code/DataClean](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/code/DataClean) folder.

- Preprocessing for the sensitivity analysis for the Level 2 Food Categorization Groups are located in the [Code/DataClean/Level_2](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/code/DataClean/Level_2) folder.

- Code for models and analysis are under the [Code/Models](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/code/Models) folder. The neural net code is in a Jupyter Notebook under [result](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/result).

- All results for both the primary analysis and sensitivity analysis are found in the [result](https://github.com/yingjiegaryzhou/Food_Pathogen_Detection/tree/main/result) folder. Supplementary Material is found under "supplementary".


