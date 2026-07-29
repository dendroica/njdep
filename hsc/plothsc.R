source("C:/Users/jgorzo/OneDrive - New Jersey Office of Information Technology/Documents/code/NJOTDeltaDist_JessG-update.R")
#hsc("female", "spring")
#hsc("male", "fall")

#save(IndexFFall, IndexMFall, IndexMSpring, IndexFSpring, indexFFall, indexMFall, indexMSpring, indexFSpring, file="HSC_NJOTindex.RData")
library(ggplot2)
indexFFall$sex <- "Female"
indexFFall$season <- "Fall"
indexMFall$sex <- "Male"
indexMFall$season <- "Fall"
indexMSpring$season <- "Spring"
indexMSpring$sex <- "Male"
indexFSpring$sex <- "Female"
indexFSpring$season <- "Spring"
index <- rbind(indexFFall, indexMFall, indexMSpring, indexFSpring)

ggplot(index, aes(Year, Yrmean)) + geom_point() + geom_line() +
  geom_ribbon(aes(ymin = LCI, ymax = UCI), fill = "grey80", alpha = 0.5) +
  theme_bw() +
  labs(y = "Index") +
  facet_grid(sex ~ factor(season, levels = c("Spring", "Fall")))
