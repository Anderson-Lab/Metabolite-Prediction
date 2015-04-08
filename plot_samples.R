positive = read.csv('Tyrosine/positive_train2.csv')
negative = read.csv('Tyrosine/negative_train2.csv')

plot.metabolite <- function(dir,metabolite,part,data) {
  for (i in 2:(ncol(data)-1)) {
    data.i = data[,c(1,i)]
    colnames(data.i)[2] = 'y'
    pdf(file = paste(dir,'/',metabolite,'-',sprintf("%03d",i-1),'-',part,'.pdf',sep=''), width= 3.5, height = 3.5,
        useDingbats=F) #I have had trouble when uploading figures with digbats before, so I don't use them
    p <- ggplot(data.i, aes(x=ppm,y=y)) + geom_line() + ggtitle(metabolite) + theme_classic() + scale_x_reverse()# + ylim(ylimits)
    print(p) #print our plot
    dev.off()
  }
}

metabolite = 'Tyrosine'
orig.regions = c(7.25,7.15,6.95,6.85,4,3.85,3.25,3.15,3.1,3)
regions = matrix(orig.regions,ncol=2,byrow=T)
for (i in 1:nrow(regions)) {
  inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
  plot.metabolite(paste(metabolite,'/images/positive2/',sep=""),metabolite,i,positive[inxs,])
}

for (i in 1:nrow(regions)) {
  inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
  plot.metabolite(paste(metabolite,'/images/negative2/',sep=""),metabolite,i,negative[inxs,])
}
