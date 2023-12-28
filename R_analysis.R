install.packages("haven")
install.packages("dplyr")
install.packages("plyr")
install.packages("corrplot")

library(haven)
identity_data <- read_sas("C:\\Users\\jared\\Downloads\\c35_2020\\Data_Données\\gss35pumfm.sas7bdat")

library(dplyr)
# add my binary married variable
identity_data <- identity_data %>%
  mutate(
    Ymarried = ifelse(MARSTAT %in% c(96, 97, 98, 99) | AGEGR10 %in% c(96, 97, 98, 99), NA, ifelse(MARSTAT == 1 & AGEGR10 != 1, 1, 0))
  )

# add my binary black race variable 
identity_data <- identity_data %>%
  mutate(
    Yblack = ifelse(VISMIN_C %in% c(96, 97, 98, 99), NA, ifelse(VISMIN_C == 3, 1, 0))
  )

# add my life satisfaction variable
identity_data <- identity_data %>%
  mutate(
    Ylifesat = ifelse(LSM_01 %in% c(96, 97, 98, 99), NA, LSM_01)
  )

# add my freedom from mental disability variable
identity_data <- identity_data %>%
  mutate(
    Ymental_fr = ifelse(DMENT_FL %in% c(6, 7, 8, 9), NA, DMENT_FL)
  )

# add my freedom from mental health variable
identity_data <- identity_data %>%
  mutate(
    Ymental_he = ifelse(SRM_115 %in% c(6, 7, 8, 9), NA, 6 - SRM_115)
  )

# add my gender variable (female is 2, male is 1)
identity_data <- identity_data %>% mutate(Yfemale = GENDER2P)

# add my visible minority variable
identity_data <- identity_data %>%
  mutate(
    Yvisible_m = ifelse(VISMINFL %in% c(6, 7, 8, 9), NA, 3 - VISMINFL)
  )

# add my freedom from indigenous variable
identity_data <- identity_data %>%
  mutate(
    Yindigenous = ifelse(ABM_01A %in% c(6, 7, 8, 9), NA, ABM_01A)
  )

# add my religious participation variable
identity_data <- identity_data %>%
  mutate(
    Yreligious = ifelse(REL_02 %in% c(6, 7, 8, 9), NA, 6 - REL_02)
  )

# add my generations since immigration variable
identity_data <- identity_data %>%
  mutate(
    # generations since immigration
    Ygen_immig = ifelse(GENSTAT %in% c(6, 7, 8, 9), NA, ifelse(GENSTAT == 3, 2.5, ifelse(GENSTAT == 4, 3, GENSTAT))),
    Yimmig_recency = 4 - Ygen_immig # measure how new of an immigrant
  )

# add my trusting variable
identity_data <- identity_data %>%
  mutate(Ytrusting = ifelse(PCT_10 %in% c(6, 7, 8, 9), NA, 3 - PCT_10))


# testing
library(plyr)
count(identity_data, 'MARSTAT')
count(identity_data, 'Ymarried')
count(identity_data, 'VISMIN_C')
count(identity_data, 'Yblack')
count(identity_data, 'Ylifesat')
count(identity_data, 'Ymental_fr')
count(identity_data, 'Ymental_he')
count(identity_data, 'Yfemale')
count(identity_data, 'Yvisible_m')
count(identity_data, 'Yindigenous')
count(identity_data, 'Yreligious')
count(identity_data, 'Yimmig_recency')
count(identity_data, 'Ytrusting')

# Compute the correlation matrix using pairwise deletion of missing values
selected_data <- identity_data[, c("Ylifesat", "Ymental_fr", "Ymental_he", "Ymarried", 
                                   "Yblack", "Yfemale", "Yvisible_m", "Yindigenous", 
                                   "Yreligious", "Yimmig_recency", "Ytrusting")]
correlation_matrix <- cor(selected_data, use = "pairwise.complete.obs")
subset_matrix <- correlation_matrix[4:nrow(correlation_matrix), 1:3] # Selecting the bottom 3 rows and the left 3 columns

# Plotting the subset matrix
print(subset_matrix)
library(corrplot)
corrplot(subset_matrix, method = "ellipse", tl.col = "black", tl.srt = 45, addCoef.col = "black", is.corr = FALSE)
corrplot(subset_matrix, method = "ellipse", col="lightblue", tl.col = "black", tl.srt = 45, addCoef.col = "black")

