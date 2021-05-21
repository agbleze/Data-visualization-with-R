library(readr)
#loading data gives an error requiring specifying the separator as ,
df = read.table("StudyArea.csv", header = TRUE)
# gIVES an error resulting from missing values which are not filled by default
df = read.table("StudyArea.csv", sep=",", header = TRUE)
#Throws an error that should be resolved by setting quote to empty string
df = read.table("StudyArea.csv", header = TRUE, fill=TRUE, sep=",")
# Sucessfully loads the data with read.table
df = read.table("StudyArea.csv", header=TRUE, fill=TRUE, quote="", sep=",")
View(df)

# Loading csv file with read.csv
df = read.csv("StudyArea.csv")
View(df)

#Open .txt file with read.table
df2 = read.table("all_genes_pombase.txt", header=TRUE, quote = "", sep = "\t")
View(df2)

#loading data with read_csv from readr package
dfReadr = read_csv("StudyArea.csv", col_types = cols(.default = "c"), col_names = TRUE)
View(dfReadr)
#Gives an error of character type
dfReadr = read_csv("StudyArea.csv", col_names = TRUE)
dfReadr = read_csv("StudyArea.csv", col_types = list(UNIT = col_character()), col_names = TRUE)
#Error resolve by setting the columename to col_character()
dfReader = read_csv("StudyArea.csv", col_types = list(OUTDATED = col_character(), UNIT = col_character()), col_names = TRUE)
head(dfReadr)
