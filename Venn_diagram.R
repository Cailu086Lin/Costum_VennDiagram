#adapted from https://scriptsandstatistics.wordpress.com/2018/04/26/how-to-plot-venn-diagrams-using-r-ggplot2-and-ggforce/
# Load library
library(VennDiagram)

# Generate 2 sets of 200 words
mvGWAS <- c("LRP1B",	"MLLT3",	"NPSR1-AS1",	"RNU6-356P",	"RP11-106M7.4",	"GATA3-AS1",	"ZNF729", NA, NA, NA, NA,NA, NA,NA, NA,NA, NA)
uvGWAS<-c("AC091705.1",	"CLEC11A",	"GLI3",	"GRIN2B",	"MCTP1",	"PRKRIRP9",	"RAPGEF2",	"RIMS2",	"RP11-354K4.1",	"RP11-575F12.1",	"RP11-736K20.6",	"RP5-1112F19.2",	"RPL21P112",	"SYT9",	"VWC2L",	"GATA3-AS1",	"ZNF729")

source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
library(limma)
library(tidyverse)
library(ggforce)
set.seed((123))
mydata <- data.frame(mvGWAS = mvGWAS,
                     uvGWAS = uvGWAS) %>%
  mutate_all(., as.logical)

##
vdc <- vennCounts(mydata)
class(vdc) <- 'matrix'
df.vdc <- as.data.frame(vdc)[-1,] %>%
  mutate(x = c(0, 0.5, 1),
         y = c(1.7, 1.5, 1.7),
         counts2=c(15,2,5))
gene<-as.data.frame(vdc)[-1,] %>%
  mutate(x = c(-0.15, 0.5, 1.1),
         y = c(1, 1, 1),
         label=c("AC091705.1\nCLEC11A\nGLI3, GRIN2B\nMCTP1, PRKRIRP9\n	RAPGEF2, RIMS2\nRP11-354K4.1\nRP11-575F12.1\nRP11-736K20.6\nRP5-1112F19.2\nRPL21P112\nSYT9, VWC2L", "GATA3-AS1\n \n ZNF729", "LRP1B\n \nMLLT3\n\n	NPSR1-AS1\n\nRNU6-356P\n\nRP11-106M7.4"))


name<- as.data.frame(vdc)[-1,] %>%
  mutate(x = c(0, 0.5, 1),
         y = c(1.9,1.9, 1.9),
         nam=c("uvGWAS","", "mvGWAS"))


p<- ggplot(df.venn, aes(x0 = x, y0 = y, r = 0.8, fill = labels)) +
  geom_circle(alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void()+theme(legend.position="none") +
  annotate("text", x = df.vdc$x, y = df.vdc$y, label = df.vdc$counts2, size = 5,  fontface =2)+annotate("text", x = gene$x, y = gene$y, label = gene$label, size = 3,  fontface =3, alpha=0.8)+annotate("text", x = name$x, y = name$y, label = name$nam, size = 6,  fontface =2)


postscript(colormodel="cmyk")
ggsave(filename ='Figure4.tif', p,  width = 12.5, height = 8, units ="cm",dpi = 900, device='tiff', limitsize = TRUE,  compression = "lzw")
dev.off()


