#ggplot2: A powerful and flexible library for creating high-quality visualizations.
library(ggplot2)

#Load the CSV file into R using the read.csv() function. For example, if your file is called "data.csv" and is in your current working directory, you can load it like this:
data <- read.csv("data.csv")

#Extract the wavelength and absorbance columns from the data frame. If the wavelength column is named "Wavelength" and the absorbance column is named "Absorbance", you can extract them like this:
wavelength <- data$Wavelength
absorbance <- data$Absorbance

#Create a plot using the plot() function. Pass the wavelength vector as the x-axis and the absorbance vector as the y-axis. You can also customize the plot by adding a title, axis labels, and changing the colors and line types. Here's an example:
ggplot(data, aes(x = wavelength, y = absorbance)) +
  geom_point()
