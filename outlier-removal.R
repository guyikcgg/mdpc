
library(outliers)

outliers = outlier(data.numeric.simplified)

ggplot(data) + geom_bar(aes(AA_freq_central_A))


rm.outlier(data.numeric.simplified, fill = T)




# A row will be considered an outlier if it presents an
# anomaly on any variable or if it presents an anomalous
# combination of its values.



# Normalize data
data.numeric.simplified.scaled = scale(data.numeric.simplified)



alpha.value = 0.05
alpha.value.penalizado = 1 - ( 1 - alpha.value) ^ (1 / nrow(mydata.numeric))

mvoutlier.plot = uni.plot(data.numeric.simplified.scaled, symb=FALSE, alpha = alpha.value.penalizado)
