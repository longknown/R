library(ggplot2)
rm(list=ls())

density_data <- read.csv(file.choose(), header=T)
density_data <- density_data[rowSums(is.na(density_data))==0,]  # remove empty rows
attach(density_data)
class_data <- read.csv(file.choose(), header = T)
attach(class_data)

# define their conservantion and bind it to density_data data.frame
conservation <- is.element(miRNA.name, Conserved)
conservation[conservation==TRUE] <- 'Conserved'
conservation[conservation==FALSE] <- 'Non-conserved'

density_data <- cbind(density_data, conservation)

# whole cultivars data.frame
whole_data <- data.frame(table(SNP_density))

# plot
g <- ggplot(density_data, aes(SNP_density)) + geom_histogram(aes(fill=conservation), position='identity', alpha=.5, binwidth=.01)
g <- g + geom_freqpoly(aes(color="All miRNAs"), binwidth=.01) + scale_color_manual(values = c("All miRNAs"="blue"))
g <- g + labs(list(title='miRNA SNP density distribution', x='SNP density', y='miRNA count'))
g <- g + scale_x_continuous(breaks=seq(0, 0.45, 0.01)) + coord_flip() + scale_y_continuous(breaks=seq(0,30,5))
print(g)
ggsave(file="SNP_density.png", width = 12, height = 9)

g1 <- ggplot(density_data, aes(SNP_density)) + geom_histogram(aes(y=..density..,fill=conservation), position='identity', alpha=.5, binwidth=.01)
g1 <- g1 + geom_freqpoly(aes(y=..density..,color="All miRNAs"), binwidth=.01) + scale_color_manual(values = c("All miRNAs"="blue"))
g1 <- g1 + coord_flip() + scale_x_continuous(breaks=seq(0, 0.45, 0.01)) + labs(list(title='miRNA SNP density distribution', x='SNP density', y='miRNA percentage (%)'))
g1 <- g1 + scale_y_continuous(breaks=seq(0,15,1))

print(g1)
ggsave(file="SNP_density_percent.png", width = 12, height = 9)

