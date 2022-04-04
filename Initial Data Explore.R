library(haven)

path = file.path("C:/Users/Owner/Desktop/PROJ518/ONS Annual Population Survey, October 2020 - September 2021/spss", "spss25", "apsp_o20s21_eul_pwta20.sav")
dataset = read_sav(path)
# Read in ONS data

head(dataset)
