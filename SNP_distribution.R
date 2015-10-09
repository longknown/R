library(ggplot2)
# clear all the existing variables
rm(list=ls())

# position population
pos_data <- read.csv(file.choose(), header = T)

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

# generate conserved & non-conserved data.frame
con_data <- more_data[conservation=='Conserved',]
non_data <- more_data[conservation=='Non-conserved',]

# conserved and non-conserved
sum_con <- data.frame(table(con_data$relavant.position))
colnames(sum_con) <- c('pos', 'occurence')
sum_con$conservation <- 'Conserved'

sum_non <- data.frame(table(non_data$relavant.position))
colnames(sum_non) <- c('pos', 'occurence')
sum_non$conservation <- 'Non-conserved'

sum_all <- data.frame(table(more_data$relavant.position))
colnames(sum_all) <- c('pos', 'occurence')
sum_all <- merge(sum_all, pos_data, by='pos')
sum_all$occurence <- sum_all$occurence / sum_all$number

sum_total <- rbind(sum_con, sum_non)
sum_total <- merge(sum_total, pos_data, by='pos')
attach(sum_total)
sum_total$occurence = occurence / number

# Use ggplot() to plot multiple layers in the same image
g <- ggplot() + geom_bar(aes(x=factor(pos), y=occurence, fill=conservation), position='dodge', sum_total, stat = 'identity')
g <- g + geom_line(aes(x=pos, y=occurence,color="Total", group=1), sum_all) + scale_color_manual(values = c("Total"="red"))
g <- g + geom_point(aes(pos, occurence), sum_all)
g <- g + labs(list(title="SNP distribution", x="mature miRNA site", y="SNP density"))
g <- g + scale_y_continuous(breaks=seq(0,0.08,.005))
print(g)
ggsave(file="SNP.png", width = 12, height = 9)
