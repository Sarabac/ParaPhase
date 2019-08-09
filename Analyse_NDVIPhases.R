W.DIR = "/home/luxis/Dropbox/Kuhn/phenology/ParaPhase"
setwd(W.DIR)
library(tidyverse)
library(DBI)

Zone_ID = 1

conn = dbConnect(RSQLite::SQLite(), "ParaPhase.sqlite")


phaseRange_total = tbl(conn, "NDVI_Phase_Range_Relative") %>% 
  filter(Zone_ID==!!Zone_ID) %>% collect()

FieldNR = phaseRange_total %>% filter(weight == 1) %>% 
  select(Field_ID, Crop, Culture_Year) %>% 
  distinct() %>% 
  group_by(Culture_Year, Crop) %>% 
  mutate(Field_NR=row_number()) %>% 
  ungroup() %>% select(Field_ID, Field_NR)

phaseRange = phaseRange_total %>% filter(weight == 1) %>% 
  inner_join(FieldNR, by=c("Field_ID"))

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

### Use lines and Years

grap =  phaseRange %>% 
  ggplot(aes(x= Relative_NDays, y=NDVI, group=Culture_ID,
             color=as.factor(Culture_Year)))+
  geom_line(alpha=0.2)+
  facet_wrap(~CropName+Transition, ncol=1, scales = "free")+
  coord_cartesian(ylim = c(0,1))
ggsave("Boxplot_NDVI_Transition_line.pdf", grap, limitsize = FALSE,
       width = 500, height = 5000, units = "mm")

graphs = lapply(unique(phaseRange$CropName), function(crop){
  dat = filter(phaseRange, CropName==crop)
  mdat = dat %>% group_by(CropName, Transition,Culture_Year) %>%
    summarise(NDVI = mean(NDVI))
  ggplot(dat)+
    geom_density(mapping = aes(x=NDVI, y=..scaled.., color = Transition))+
    geom_vline(data=mdat, aes(xintercept = NDVI, color = Transition, linetype="mean"),
               size = .8)+
    scale_linetype_manual(name="",values = c(mean ="longdash"))+
    facet_grid(Culture_Year~CropName)+
    coord_cartesian(xlim = c(-0.25,1), ylim = c(0,1))
})
ggsave("Density_NDVI_Transition_Year.pdf",
       gridExtra::grid.arrange(grobs=graphs,ncol = 1),
       width = 500, height = 1000, units = "mm")


### Sugar Beet ###
grap =  phaseRange %>% mutate(Culture_Year = as.factor(Culture_Year)) %>% 
  filter(Crop==253) %>% 
  ggplot(aes(x= NDays, y=NDVI, group=Culture_ID, color=as.factor(Field_NR)))+
  geom_line(alpha=0.3)+
  facet_wrap(~CropName+Transition+Culture_Year, ncol = 1, scales = "free")+
  coord_cartesian(ylim = c(0,1))+ 
  theme(legend.position="top")
ggsave("Boxplot_NDVI_Transition_line_Sugar_Beet.pdf", grap, limitsize = FALSE,
       width = 500, height = 5000, units = "mm")




dbDisconnect(conn)
