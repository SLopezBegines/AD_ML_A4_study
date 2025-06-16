# AD_ML_A4_study
Longitudinal and cross-sectional analysis of A4 study
A4 Dataset
Anti-Amyloid Treatment in Asymptomatic Alzheimer's Disease
https://doi.org/10.1001/jamaneurol.2020.0387

https://discover.a4studydata.org/catalogue/studies/1901af97-b7cf-46ef-ab5c-b43212902b72

We will use A4 dataset with to perspectives:
- First, use tabular data to obtain a probability of AD in patients
- Second, use PET images to get the probability (or score) of Tau or B-amiloid deposits.
- Third, and most importatn, mix both CNN in a single one to obtain the mixed probability bases in tabular and images data.

First aim will be done in R, with EDA, remove outliers, impute missing values, and select variables. For ML based in tabular data, I will use R (tidymodels/H2O) and/or Python.
Second and third aim will be done in Pyton.
