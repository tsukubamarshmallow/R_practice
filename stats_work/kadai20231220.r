# set current directory
setwd("Z:/stats_work")
# import data
load("./week10-data.dat")
# calculate cor-matrix
cormatrix <- cor(FiveSubjPoints)
# calculate eigenvalue of cor-matrix
eigenvalue_of_cormatrix <- eigen(cormatrix)
# clasify eigencalues guessing whether each eigenvalue is over 1.0 or not
colors <- ifelse(eigenvalue_of_cormatrix$values > 1, "red", "blue")
# plot
png("Q10-2.png", width = 400, height = 400)
barplot(eigenvalue_of_cormatrix$values, col = colors, main = "Eigenvalues Plot", names.arg = seq_along(eigenvalue_of_cormatrix$values))
abline(h = 1, lty = 2, col = "black")
dev.off()
#analusys of non_rotaion factanal
non_roraion <- factanal(FiveSubjPoints,rotation = "none",factors = 2, cutoff = 0)
#analusys of rotaion factanal
non_roraion <- factanal(FiveSubjPoints,rotation = "varimax",factors = 2, cutoff = 0)