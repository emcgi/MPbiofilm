library(meta)

data<-read.csv("metaForest.csv",sep = ",")
numVars<-1:length(levels(data$Variable))
numTre<-1:length(levels(data$Treatment))

for (i in numVars){
  for (j in numTre){
  new<-subset(data, Variable==levels(data$Variable)[i])
  m.raw <- metacont(Ne,
                    Me,
                    Se,
                    Nc,
                    Mc,
                    Sc,
                    data=new,
                    byvar = new$Treatment,
                    studlab=paste(Variable,Polymer),
                    comb.fixed = TRUE,
                    comb.random = FALSE,
                    prediction=TRUE,
                    sm="SMD")
  png(file=paste(levels(data$Variable)[i],'forestplot.png',sep = '_'), width=800)
  forest(m.raw)
  dev.off()
  }
}
