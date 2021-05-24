## creating bar charts ###
readData = read_csv("StudyArea.csv", col_types = list(OUTDATED = col_character(), UNIT = col_character()))
View(readData)
subsetData = select(readData, ORGANIZATI, STATE, YEAR_, TOTALACRES, CAUSE)
filterData = filter(subsetData, STATE == 'California')
groupData = group_by(filterData, YEAR_)
ggplot(data = groupData) + geom_bar(mapping = aes(x = YEAR_, fill = 'red'))
##use geom_col to creat chart
ggplot(data = groupData) + geom_col(mapping = aes(x = YEAR_, y = TOTALACRES, fill = "red"))

###REATE Violin chart
filt5000 = filter(subsetData, TOTALACRES >= 5000)
grpData = group_by(filt5000, ORGANIZATI)
ggplot(data = grpData, mapping = aes(x=ORGANIZATI, y = log(TOTALACRES))) + geom_violin()
###Add mean to violin plot
ggplot(data = grpData, mapping= aes(x = ORGANIZATI, y = log(TOTALACRES))) + geom_violin() +geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun=mean, geom = "point", size = 2, color = "green") 
## Add boxplot to add mean and IQR
ggplot(data = grpData, mapping= aes(x = ORGANIZATI, y = log(TOTALACRES))) + geom_violin() +geom_jitter(height = 0, width = 0.1) +
   geom_boxplot(width = 0.1)

###create density plot
ggplot(data = grpData, aes(log(TOTALACRES))) + geom_density()

####create 2D plots with contours
ggplot(data = grpData, aes(x = YEAR_, y = log(TOTALACRES))) + geom_point() + geom_density_2d(color = "red")

###create 2D density surface###
ggplot(data = grpData, aes(x = YEAR_, y = log(TOTALACRES))) + geom_density_2d() +
  stat_density_2d(geom = "raster", aes(fill = ..density..), contour = FALSE)
