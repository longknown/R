library(ggplot2)
# clear all the existing variables
rm(list=ls())

# First read in the SNP information
snp_data <- read.csv(file.choose(), header=T)
attach(snp_data)

# Read in the miRNA classification
class_data <- read.csv(file.choose(), header=T)
attach(class_data)

# Add another column defined as conservation
conservation <- is.element(mature.miRNA, conserved)
# Replace TRUE & FALSE into Conserved & Non-conserved
conservation[conservation==TRUE] <- 'Conserved'
conservation[conservation==FALSE] <- 'Non-conserved'

more_data <- cbind(snp_data, conservation)
detach(snp_data)

# create a data.frame to store total cultivar SNP distribution
total <- data.frame(x=seq(1,24), y=as.vector(table(more_data$relavant.position)))

# Use ggplot() to plot multiple layers in the same image
g <- ggplot() + geom_bar(aes(factor(relavant.position), fill=conservation), position='dodge', more_data)
g <- g + geom_line(aes(x,y,color="Total"), total) + scale_color_manual(values = c("Total"="red"))
g <- g + geom_point(aes(x,y), total)
g <- g + labs(list(title="SNP distribution", x="position", y="SNP count"))
print(g)
ggsave(file="SNP.png", width = 12, height = 9)
