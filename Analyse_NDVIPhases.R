W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
source("Variables.R")
library(tidyverse)
library(DBI)


conn = dbConnect(RSQLite::SQLite(), paste(OUT.SQLITE, "sqlite", sep="."))

phaseRange = tbl(conn, "Filtered_NDVI_Phase_Range") %>% 
  collect()

coherent =phaseRange %>%
  mutate(NDVI=NDVI/10000) %>% 
  group_by(Crop, Pre_P) %>%
  ungroup() %>% 
  mutate(transition = paste(Pre_P,Next_P, sep="-"))

graphs = lapply(unique(coherent$Crop), function(crop){
  dat = filter(coherent, Crop==crop)
  mdat = dat %>% group_by(Crop, transition) %>%
    summarise(NDVI = mean(NDVI))
  ggplot(dat)+
    geom_density(mapping = aes(x=NDVI, color = transition))+
    geom_vline(data=mdat, aes(xintercept = NDVI, color = transition, linetype="mean"),
               size = .8)+
    scale_linetype_manual(name="",values = c(mean ="longdash"))+
    facet_wrap(~Crop)
})
ggsave("arranged.pdf",
       gridExtra::grid.arrange(grobs=graphs,ncol = 1),
       width = 500, height = 1000, units = "mm")


pdf("density.pdf")
for(crop in unique(coherent$Crop)){
  filter(coherent, Crop==crop) %>% 
    ggplot( aes(x=NDVI, color = transition))+
    geom_density() %>% 
    print()
}
dev.off()

gridExtra::grid.arrange(grobs=graphs,ncol = 1)


zr = ggplot(coherent, aes(x=NDVI, color = transition))+
  facet_wrap(~Crop)+
  geom_density()


rmIncoherence = filter(phaseRange, Crop==253)
grap = ggplot(rmIncoherence, aes(x=Relative_NDays, y=NDVI))+
  geom_point(alpha=0.01)+
  geom_smooth()+
  facet_grid(Crop~Pre_P+Next_P)
grap


PR = phaseRange %>% filter(Crop==204&Pre_P==18&Next_P==21)
grap = ggplot(PR, aes(x=Relative_NDays, y=NDVI))+
  geom_hex(bins=100)+
  geom_smooth()
grap
