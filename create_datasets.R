library(ggplot2)

df = read.table('collection_All_,_PVT_Scores_9481.txt',skip=14)
ppm = df[,1]
X = df[,2:ncol(df)]

all.maxs = T

plot.metabolite <- function(dir,metabolite,part,X,inxs) {
  X.m = X[inxs,]
  data = cbind(ppm[inxs],X.m)
  colnames(data)[1] = 'ppm'
  for (i in 2:ncol(data)) {
    data.i = data[,c(1,i)]
    colnames(data.i)[2] = 'y'
    pdf(file = paste(dir,'/',metabolite,'-',sprintf("%03d",i-1),'-',part,'.pdf',sep=''), width= 3.5, height = 3.5,
        useDingbats=F) #I have had trouble when uploading figures with digbats before, so I don't use them
    p <- ggplot(data.i, aes(x=ppm,y=y)) + geom_line() + ggtitle(metabolite) + theme_classic() + scale_x_reverse()# + ylim(ylimits)
    print(p) #print our plot
    dev.off()
  }
}

# metabolite = 'Taurine'
# regions = matrix(c(3.5,3.2),ncol=2)
# inxs = c()
# for (i in 1:nrow(regions)) {
#   inxs = c(inxs,which(regions[i,1] >= ppm & ppm >= regions[i,2]))
# }
# plot.metabolite(metabolite,X,inxs)
# 
# metabolite = 'alpha-ketoglutarate'
# regions = matrix(c(3.1,2.9,2.5,2.35),ncol=2,byrow=T)
# for (i in 1:nrow(regions)) {
#   inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
#   plot.metabolite(metabolite,paste(metabolite,'-',i,'-',sep=""),X,inxs,c(0,0.5))
# }

metabolite = 'Tyrosine'
regions = matrix(c(7.25,7.15,6.95,6.85,4,3.85,3.25,3.15,3.1,3),ncol=2,byrow=T)
#for (i in 1:nrow(regions)) {
#  inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
#  plot.metabolite(metabolite,metabolite,i,X,inxs)
#}
inxs = c()
for (i in 1:nrow(regions)) {
  region.inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
  if (all.maxs == F) {
    maxs = apply(X[region.inxs,],2,max)
  } else {
    maxs = apply(X,2,max)    
  }
  for (j in 1:ncol(X)) {
    X[region.inxs,j] = X[region.inxs,j]/maxs[j]
  }
  inxs = c(inxs,region.inxs)
}
X.m = X[inxs,]
data = cbind(ppm[inxs],X.m)
colnames(data)[1] = 'ppm'
colnames(data)[2:ncol(data)] = 1:(ncol(data)-1)
if (all.maxs == F) {
  write.csv(data,file='Tyrosine/positive_train.csv',quote=F,row.names=F)
} else {
  write.csv(data,file='Tyrosine/positive_train_global_max.csv',quote=F,row.names=F)  
}
inxs = c()
regions = matrix(c(7.25,7.15,6.95,6.85,4,3.85,3.25,3.15,3.1,3)+1,ncol=2,byrow=T)
for (i in 1:nrow(regions)) {
  region.inxs = which(regions[i,1] >= ppm & ppm >= regions[i,2])
  maxs = apply(X[region.inxs,],2,max)
  for (j in 1:ncol(X)) {
    X[region.inxs,j] = X[region.inxs,j]/maxs[j]
  }
  inxs = c(inxs,region.inxs)
}
X.m = X[inxs,]
data = cbind(ppm[inxs]+1,X.m)
colnames(data)[1] = 'ppm'
colnames(data)[2:ncol(data)] = 1:(ncol(data)-1)
if (all.maxs == F) {
  write.csv(data,file='Tyrosine/negative_train.csv',quote=F,row.names=F)
} else {
  write.csv(data,file='Tyrosine/negative_train_global_max.csv',quote=F,row.names=F)  
}