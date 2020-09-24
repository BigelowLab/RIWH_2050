library(readr) 
library(ggplot2)
library(plyr)
library(dplyr)
library(viridis)
library(maps)

DIR <- '~/Desktop/Thesis/Bigelow/right_whale'
setwd(dir = DIR)
#Create filepath to presence/absense data
fp_pa <- file.path(DIR, 'pa_data')

cbp <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Formatted presence/absence & covariate data
full_data <- read_csv(file.path(fp_pa,'RIWH_MAR_2020_PRES_CLIMATOLOGY.csv'))

#Take log of bathymetry
#Convert presence/absence to binary
full_data <- full_data %>%
              filter(YEAR < 2010) %>%
             #Add binary presence/absence column (p = 1, a = 0)
             mutate(pa = if_else(is.na(SPECCODE), 0, 1))

presences <- data.frame(rowsum(full_data$pa, group = full_data$MONTH)) %>%
  mutate(month = seq(1:12),
         pa = 'Present')

names(presences) <- c('count', "month", "pa")

full_data <- full_data %>%
  mutate(pa = if_else(is.na(SPECCODE), 1, 0))

absences <- data.frame(rowsum(full_data$pa, group = full_data$MONTH)) %>%
  mutate(month = seq(1:12),
         pa = 'Absent')

names(absences) <- c('count', "month", "pa")

df <- rbind(absences, presences)
names(df) <- c('count', 'month', 'RightWhale')

df_a <- df %>% dplyr::filter(RightWhale == "Absent")

#Presence/absence bar chart
ggplot(presences, aes(fill=pa, y=count, x=month)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Count") +
  xlab("Month") + 
  ggtitle("E. glacialis Presences & Absences") +
  scale_x_continuous(breaks = seq(1:12)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#Line chart

ggplot(absences, aes(x = month, y = count, color = pa)) +
  geom_path() +
  geom_path(data = presences, aes(x = month, y = count*20, color = pa)) +
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Presences")) +
  labs(color = "") + 
  ylab("Absences") +
  xlab("") + 
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  scale_fill_manual(labels = c("Present", "Absent"), values = cbp[3:4]) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.justification=c(1.15,0), legend.position=c(1,0.72))

ggplot(df[df$RightWhale == "Present",], aes(x = month, y = count)) +
  geom_path() +
  ylab("Count") +
  xlab("Month") + 
  ggtitle("E. glacialis Presences") +
  scale_x_continuous(breaks = seq(1,12,2), labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov")) +
  scale_fill_manual(labels = c("Present"), values = cbp[3]) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

effort <- full_data %>% 
  mutate(code = paste0(latgrd, longrd)) %>%
  add_count(code)

worldmap <- map_data("world")

ggplot() + 
  #Add probability data
  geom_tile(data = effort, aes(longrd, latgrd, fill = n)) +
  #Add probabilitiy color gradient and label
  scale_fill_gradientn(colours = inferno(500), name = 'Effort') +
  labs(x = "Longitude", 
       y = "Latitude",
       title = "Sightings effort") +
  #Add sightings data
  geom_point(data = full_data[full_data$pa == 0,], aes(longrd, latgrd), pch = 21, cex = 0.001, color = 'white', fill = "white") +
  #Add world map data
  geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray", colour = "gray43") +
  coord_quickmap(xlim = c(round(min(effort$longrd)), round(max(effort$longrd))), 
                 ylim = c(round(min(effort$latgrd)), round(max(effort$latgrd))),
                 expand = TRUE) +
  #Remove grid lines
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# Normalize to maximum of 12 visits per site
effort <- full_data %>% 
  distinct(MONTH, latgrd, longrd) %>%
  mutate(code = paste0(latgrd, longrd),
         month_lab = as.character(MONTH)) %>%
  mutate(Season = ifelse(MONTH == 12 | MONTH == 1 | MONTH == 2, "Winter", 
                         ifelse(MONTH == 3 | MONTH == 4 | MONTH == 5, "Spring", 
                                ifelse(MONTH == 6 | MONTH == 7 | MONTH == 8, "Summer", "Fall")))) %>%
  add_count(code)

effort <- effort %>%
  mutate(season_f = factor(Season, levels=c('Winter','Spring','Summer','Fall')))

sightings <- full_data %>%
  mutate(Season = ifelse(MONTH == 12 | MONTH == 1 | MONTH == 2, "Winter", 
                         ifelse(MONTH == 3 | MONTH == 4 | MONTH == 5, "Spring", 
                                ifelse(MONTH == 6 | MONTH == 7 | MONTH == 8, "Summer", "Fall"))))

sightings <- sightings %>%
  mutate(season_f = factor(Season, levels=c('Winter','Spring','Summer','Fall')))

ggplot() + 
  #Add probability data
  geom_tile(data = effort, aes(longrd, latgrd, fill = n)) +
  #Add probabilitiy color gradient and label
  scale_fill_gradientn(colours = inferno(500, begin = 0.25, end = 1), name = 'Effort') +
  #Add sightings data
  geom_point(data = sightings[sightings$pa == 0,], aes(longrd, latgrd), pch = 21, cex = 1, color = "white", fill = "black", stroke = 0.5) +
  #Add world map data
  geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray", colour = "gray") +
  coord_quickmap(xlim = c(round(min(effort$longrd)), round(max(effort$longrd))), 
                 ylim = c(round(min(effort$latgrd)), round(max(effort$latgrd))+0.6),
                 expand = TRUE) +
  #Remove grid lines
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  #ggsave(filename = file.path(DIR, paste('effort_normalized_with_sightings.jpeg', sep = '')))
  #
  facet_grid(row = vars(season_f)) + 
  facet_wrap(~ season_f, scales = 'free', nrow = 2)

ggplot() + 
  #Add probability data
  geom_tile(data = as.data.frame(bat, xy = T, na.rm = T), aes(x, y, fill = layer)) +
  geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray", colour = "gray") +
  coord_quickmap(xlim = c(round(min(effort$longrd)), round(min(effort$longrd)), 
                 ylim = c(round(min(effort$latgrd)), round(max(effort$latgrd))+0.6),
                 expand = TRUE)) +
  #Remove grid lines
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())
  #ggsave(filename = file.path(DIR, paste('effort_normalized_with_sightings.jpeg', sep = '')))
  #
library(marmap) ; library(ggplot2)
dat <- getNOAA.bathy(round(min(effort$longrd))-0.6,round(max(effort$longrd))+0.6,round(min(effort$latgrd))-0.6,round(max(effort$latgrd))+0.8,res=1, keep=TRUE)
plot(dat, image=TRUE, deep=-6000, shallow=0, step=10)
# Plot bathy object using custom ggplot2 functions
autoplot(dat, geom=c("r", "c")) + scale_fill_etopo() + 
  xlab("") + ylab("") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())

ggplot(dat, aes(x=x, y=y)) + coord_quickmap() +
  # background
  geom_raster(aes(fill=z)) +
  scale_fill_etopo() +
  # countours
  geom_contour(aes(z=z),
               breaks=c(0, -100, -200, -500, -1000, -2000, -4000),
               colour="black", size=0.2
  ) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank())

  
# Monthly effort maps
month_labs <- c("January", "February", "March", "April", "May", "June", 
                "July", "August", "September", "October", "November", "December")

for (i in 1:12) {
  
  effort <- full_data %>% 
    filter(MONTH == i) %>%
    mutate(code = paste0(latgrd, longrd)) %>%
    add_count(code)
  
  month_data <- full_data %>% filter(MONTH == i)
  
  ggplot() + 
    #Add probability data
    geom_tile(data = effort, aes(longrd, latgrd, fill = n)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), name = 'Effort') +
    labs(x = "Longitude", 
         y = "Latitude",
         title = paste(month_labs[i], " effort")) +
    #Add sightings data
    #geom_point(data = month_data[month_data$pa == 1,], aes(longrd, latgrd), pch = 21, cex = 1, color = 'black', fill = "white") +
    #Add world map data
    geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray", colour = "gray43") +
    coord_quickmap(xlim = c(round(min(effort$longrd)), round(max(effort$longrd))), 
                   ylim = c(round(min(effort$latgrd)), round(max(effort$latgrd))),
                   expand = TRUE) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(DIR, paste(i, '_effort.jpeg', sep = '')))
  
  ggplot() + 
    #Add probability data
    geom_tile(data = effort, aes(longrd, latgrd, fill = n)) +
    #Add probabilitiy color gradient and label
    scale_fill_gradientn(colours = inferno(500), name = 'Effort') +
    labs(x = "Longitude", 
         y = "Latitude",
         title = paste(month_labs[i], "effort")) +
    #Add sightings data
    geom_point(data = month_data[month_data$pa == 1,], aes(longrd, latgrd), pch = 21, cex = 1, color = 'black', fill = "white") +
    #Add world map data
    geom_polygon(data = worldmap, aes(long, lat, group = group), fill = "gray", colour = "gray43") +
    coord_quickmap(xlim = c(round(min(effort$longrd)), round(max(effort$longrd))), 
                   ylim = c(round(min(effort$latgrd)), round(max(effort$latgrd))),
                   expand = TRUE) +
    #Remove grid lines
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    #Save plot to hard drive
    ggsave(filename = file.path(DIR, paste(i, '_effort_sightings.jpeg', sep = '')))
    
  
}
  
  
  





