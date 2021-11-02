#imports
library(dplyr)
library(tidyr)
library(data.table)

#data
df <- read.csv('united-kingdom/united-kingdom.csv')

#head and tail of data
head(df)
tail(df)

#structure of data
str(df)

#variables statistics
summary(df)

#changing variable data type 
df$endtime <- as.Date(df$endtime,format="%d/%m/%Y")

#extracting month and year date values
df$year <- format(df$endtime,"%Y")

#filtering enteries from 2020
df <- filter(df,df$year == 2021)

# filtering the data for the unvaccinated respondents
df <- filter(df,vac =="No, neither")%>%filter(vac5=="No" | vac5 =="Not sure")

#irregular col name fix
df <-rename(df,RecordNo=ï..RecordNo)

# filtering out insignificant cols and spliting data into background and survey response data
df_background <- select(df,RecordNo,endtime,age,region,gender,household_size,household_children,employment_status,vac5)

#renaming variable vac5 in df_background for the sake of clarity
df_background <-rename(df_background,vac_willingness = vac5)

# replace values to ensure data integrity
df_background$household_children <- replace(df_background$household_children,
                                            which(df_background$household_children=="7"),"5 or more")

#new age group variable
df_background$agegrp <- cut(df_background$age,breaks=c(18,25,35,45,55,65,Inf),
                            labels=c("18-24","25-34","35-45","45-55","55-65","65+"))
head(df_background,2)

#structure of data frame
str(df_background)

#imports
library(modeest)
#crosstable
tbl_demographic <- df_background %>% group_by(vac_willingness) %>% 
summarise(avg_age=round(mean(age),0),region=mfv(region),gender=mfv(gender),
          employment_status=mfv(employment_status),household_size=mfv(household_size),
          household_children=mfv(household_children),total_respondents=table(vac_willingness))

tbl_demographic

#imports
library(sf)
library(geojsonio)
library(plotly)

#extracting datasets for regions and countries
eng <- geojson_read(paste0("https://opendata.arcgis.com/datasets/",
                              "8d3a9e6e7bd445e2bdcc26cdf007eac7_4.geojson"), 
                                what="sp")
countries <- geojson_read(paste0("https://opendata.arcgis.com/datasets/",
                                   "92ebeaf3caa8458ea467ec164baeefa4_0.geojson"),
                                     what="sp")

#creating UK regions map data frame
eng <- st_as_sf(eng)
countries <- st_as_sf(countries)
UK <- countries[-1,] 
names(eng)[3] <- "Region"
names(UK)[3] <- "Region"
UK$objectid <- 10:12
eng <- eng[-2]
UK <- UK[c(1, 3, 9:11)]
UK <- rbind(eng, UK)

#creating regions dataframe
data <- data.frame(table(df_background$region))

#renaming columns with appropriate names
data <-rename(data,Region=Var1)
data <-rename(data,Population=Freq)

#joining data
mapdata <- left_join(UK,data,by="Region")

#display
pl <- ggplot(mapdata, aes(fill = Population,label=Region)) + geom_sf() + 
        scale_fill_gradient(low = "blue",high="red") + ggtitle("Population of vaccine holdouts by Region") + theme_classic() +
         theme(axis.text = element_blank(), axis.ticks = element_blank(),axis.line = element_blank(),plot.margin = unit(c(0, 1, 0, 1), "cm"))

ggplotly(pl)

#setting plot size
options(repr.plot.width=8, repr.plot.height=8)

# imports
library(ggplot2)
library(treemapify)
library(grid)
library(gridExtra)

#aggregated dataframe
df_background_certain <- filter(df_background,vac_willingness=="No")
certain <- data.frame(table(df_background_certain$region))

df_background_uncertain <- filter(df_background,vac_willingness=="Not sure")
uncertain <- data.frame(table(df_background_uncertain$region))


# treemaps
pl_certain <- ggplot(data=certain, 
       aes(x=Var1, fill=Var1, area=Freq,label=paste(Var1, Freq, sep="\n"),)) + 
        geom_treemap() + geom_treemap_text(colour = "white", place = "centre",size = 15) + 
         labs(title="Certain vaccine holdouts") + xlab("") +
          theme(legend.position = "none",axis.text.x = element_blank(),
           axis.ticks = element_blank())

pl_uncertain <- ggplot(data=uncertain, 
       aes(x=Var1, fill=Var1, area=Freq,label=paste(Var1, Freq, sep="\n"),)) + 
        geom_treemap() + geom_treemap_text(colour = "white", place = "centre",size = 15) + 
         labs(title="Uncertain vaccine holdouts") + xlab("") +
          theme(legend.position = "none",axis.text.x = element_blank(),
           axis.ticks = element_blank()) 

#display
grid.arrange(pl_certain,pl_uncertain,ncol=1,nrow=2,top=textGrob(expression(bold("Population of vaccine holdouts by Region")),gp = gpar(col='black',fontsize=18)))

# creating age dataframe 
data.pl1 <- data.frame(df_background %>% select(agegrp,vac_willingness))
data.pl1 <- data.pl1[complete.cases(data.pl1),]

data.pl2 <- data.frame(df_background %>% select(agegrp))
data.pl2$status <- "Unvaccinated"
data.pl2 <- data.pl2[complete.cases(data.pl2),]

data.pl3 <- 
#barplots
pl1 <- ggplot(data.pl1,aes(x=agegrp)) + geom_bar(aes(fill=vac_willingness),position="dodge") +
       labs(x ="",fill="Vaccine Willingness") + theme_classic()
pl2 <- ggplot(data.pl2,aes(x=agegrp)) + geom_bar(aes(fill=status)) +
       labs(x ="age group",fill="Vaccination status") + theme_classic()
pl3 <- ggplot(data.pl1,aes(x=agegrp)) + geom_bar(aes(fill=vac_willingness),position="fill") +
       labs(x ="",fill="Vaccine Willingness") + theme_classic()

#display
grid.arrange(pl1,pl3,pl2,top=textGrob(expression(bold("Population of vaccine holdouts distributed by Age")), gp = gpar(col='black',fontsize=18)))

#adjusting plot size
options(repr.plot.width=12, repr.plot.height=6)

#data function
df <- function(type){
#creating gender dataframe
df.gender <- df_background %>% select(gender,vac_willingness) %>% filter(gender==type)
data.gender <- data.frame(table(df.gender$vac_willingness,df.gender$gender))
data.gender$percentage <- (data.gender$Freq)/sum(data.gender$Freq)
data.gender$ymax = cumsum(data.gender$percentage)
data.gender$ymin = c(0, head(data.gender$ymax, n=-1))
data.gender$labelPosition <- (data.gender$ymax + data.gender$ymin) / 2
data.gender$label <- paste0(data.gender$Var1, "\n",round(data.gender$percentage*100,0),"%", "\n value: ", data.gender$Freq)
return(data.gender)
}
data.male <- df("Male")
data.female <- df("Female")

#creating dataframe for all genders
df.allgenders <- df_background %>% select(gender,vac_willingness)
df.allgenders$variable <- "All Genders"
data.allgenders <- data.frame(table(df.allgenders$vac_willingness,df.allgenders$variable))
data.allgenders$percentage <- (data.allgenders$Freq)/sum(data.allgenders$Freq)
data.allgenders$ymax = cumsum(data.allgenders$percentage)
data.allgenders$ymin = c(0, head(data.allgenders$ymax, n=-1))
data.allgenders$labelPosition <- (data.allgenders$ymax + data.allgenders$ymin) / 2
data.allgenders$label <- paste0(data.allgenders$Var1,"\n",round(data.allgenders$percentage*100,0),"%", "\n value: ", data.allgenders$Freq)

#combining dataframe
data.gender <- rbind(data.male,data.female,data.allgenders)

#donut charts
pl <- ggplot(data.gender, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
        geom_rect() + geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
          scale_fill_brewer(palette=18) + coord_polar(theta="y") + xlim(c(2, 4)) + 
            theme_classic() + labs(title="Population of vaccine holdouts by Gender",x="",y="") + facet_grid(.~Var2) +
             theme(legend.position = "none", plot.title = element_text(size = 20),axis.text = element_blank(),
              axis.ticks = element_blank())
#display
pl

#adjusting plot size
options(repr.plot.width=12, repr.plot.height=8)

# imports
library(packcircles)

# plot function
plot <- function(status){
#data
d <- df_background %>% select(employment_status,vac_willingness) %>% filter(employment_status==status)
d <- data.frame(table(d$vac_willingness,group_by=d$employment_status))
d$percentage <- round(d$Freq/sum(d$Freq) * 100,0)

total <- sum(d$Freq)
packing <- circleProgressiveLayout(d$Freq, sizetype='area')
d <- cbind(d,packing)
data.gg <- circleLayoutVertices(packing, npoints=100)

#returning plot
return(ggplot() + geom_polygon(data = data.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
geom_text(data = d, aes(x, y, size=100,label= paste0(Var1,"\n",percentage,"%"))) +
  scale_size_continuous(range = c(1,10)) + theme_void() + 
  theme(legend.position="none") + ggtitle(paste0(status,"\n","Total respondents: ",total)) +
  coord_equal())
       }

#Assigning plots by employment status
plot.FTE <- plot("Full time employment")
plot.PTE <- plot("Part time employment")
plot.FTS <- plot("Full time student")
plot.R <- plot("Retired")
plot.NW <- plot("Not working")
plot.U <- plot("Unemployed")

#creating plot grid
pl <- grid.arrange(plot.FTE,plot.PTE,plot.NW,plot.FTS,plot.R,plot.U,
             top=textGrob(expression(bold("Population of vaccine holdouts by Employment status")), gp = gpar(col='black',fontsize=20)))

d <- df_background %>% select(age,agegrp,employment_status) %>% #%>% filter(employment_status=="Retired")
      group_by(employment_status) %>% summarise(average.age=round(mean(age),0),age.group=mfv(agegrp))
d <- d %>% filter(employment_status != "Other")
d