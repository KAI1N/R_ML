data(heights)
heights
nrow(heights)
head(heights)
heights[777,2]
heights$sex[777]
max(heights$height)
which(heights$height==min(heights$height), arr.ind=TRUE)#which.min(heights$height)
mean(heights$height)
median(heights$height)
mean(heights$sex == "Male")
sum(heights$height>78)
sum(heights$height>78&heights$sex=='Female')
