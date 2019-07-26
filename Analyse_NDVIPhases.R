W.DIR = "L:/Lucas/phenology/ParaPhase"
setwd(W.DIR)
library(tidyverse)
library(DBI)


conn = dbConnect(RSQLite::SQLite(), "ParaPhase.sqlite")

phaseRange = tbl(conn, "Filtered_NDVI_Phase_Range") %>% 
  collect()

graphs = lapply(unique(phaseRange$CropName), function(crop){
  dat = filter(phaseRange, CropName==crop)
  mdat = dat %>% group_by(CropName, Transition) %>%
    summarise(NDVI = mean(NDVI))
  ggplot(dat)+
    geom_density(mapping = aes(x=NDVI, y=..scaled.., color = Transition))+
    geom_vline(data=mdat, aes(xintercept = NDVI, color = Transition, linetype="mean"),
               size = .8)+
    scale_linetype_manual(name="",values = c(mean ="longdash"))+
    facet_wrap(~CropName)+
    coord_cartesian(xlim = c(-0.25,1), ylim = c(0,1))
})
ggsave("Density_NDVI_Transition.pdf",
       gridExtra::grid.arrange(grobs=graphs,ncol = 1),
       width = 500, height = 1000, units = "mm")


grap =  phaseRange %>% #round to create classes for the boxplots
  mutate(Relative_NDays = round(Relative_NDays, 2)) %>% 
  ggplot(aes(x=Relative_NDays, y=NDVI, group=Relative_NDays))+
  geom_boxplot(outlier.shape=NA)+
  geom_smooth( aes(group=1), alpha=0.3)+
  facet_wrap(~CropName+Transition, ncol=1)+
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))
ggsave("Boxplot_NDVI_Transition.pdf", grap, limitsize = FALSE,
       width = 500, height = 5000, units = "mm")

dbDisconnect(conn)
