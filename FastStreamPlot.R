require(ggplot2)
require(scales)
require(grid)
require(ggthemes)

fs <- read.csv("./FSstats.csv")
str(fs)

years  <- unique(fs$Year)

for (i in years) {
  fs$GrandTotalApps <- fs$Total.Applicants[fs$Degree.Type=="Overall" & fs$Year==i]
  fs$GrandTotalHires[fs$Year==i] <- fs$Total.Successful[fs$Degree.Type=="Overall" & fs$Year==i]
}

fs <- fs[fs$Degree.Type !="Overall", ]

fs$TotalApps <- fs$Total.Applicants
fs$TotalHires <- fs$Total.Successful
fs$Degree <- fs$Degree.Type

fs$SuccessRate <- fs$TotalHires/fs$TotalApps

fs$OverallSuccessRate <- fs$GrandTotalHires/fs$GrandTotalApps

fs$RelativeSuccessRate <- fs$SuccessRate/fs$OverallSuccessRate-1

fs$ShareofApps  <- fs$Total.Applicants/fs$GrandTotalApps 
fs$ShareofHires  <- fs$Total.Successful/fs$GrandTotalHires
fs$Overrepr <- fs$ShareofHires/fs$ShareofApps-1

#qplot(x=RelativeSuccessRate, y=Overrepr, data=fs)

fs$Area[fs$Degree.Type=="Mathematics"] <- "STEM" 
fs$Area[fs$Degree.Type=="Sciences (Physical & Biology)"] <- "STEM" 
fs$Area[fs$Degree.Type=="Engineering"] <- "STEM" 
fs$Area[fs$Degree.Type=="Technology"] <- "STEM" 
fs$Area[fs$Degree.Type=="Medicine"] <- "Medicine" 
fs$Area[fs$Degree.Type=="Business"] <- "Business" 
fs$Area[fs$Degree.Type=="Other"] <- "Other" 
fs$Area[fs$Degree.Type=="Multi Discipline"] <- "Multi" 
fs$Area[fs$Degree.Type=="Economics"] <- "SocSci & Humanities" 
fs$Area[fs$Degree.Type=="Languages"] <- "SocSci & Humanities" 
fs$Area[fs$Degree.Type=="Social Sciences"] <- "SocSci & Humanities" 
fs$Area[fs$Degree.Type=="Humanities"] <- "SocSci & Humanities"

fs$Area <- as.factor(fs$Area)
fs <- droplevels.data.frame(fs)

fs$Year <- as.factor(fs$Year)

fsplot <- ggplot(fs, aes(ShareofApps, ShareofHires)) +
  geom_point(aes(size=log(fs$TotalHires), colour=fs$Area)) +
  geom_point() +
  geom_text(aes(label=fs$Degree))+
  facet_wrap(~Year)
fsplot

fsbar <- ggplot(fs, aes(Degree,RelativeSuccessRate)) +
  geom_bar(aes(fill=fs$TotalHires),stat="identity",position="dodge")+
  facet_wrap(~Year)+
  coord_flip()+
  theme_minimal(base_size=14)+
  scale_fill_continuous(low="yellow",high="red",
                        name="Number of\nsuccessful\napplicants")+
  scale_y_continuous(labels=percent)+
  theme(axis.title=element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank())+
  labs(title="Success rate by field of study\ncompared to average success rate",
       ylab=element_blank(), xlab=element_blank())
fsbar

ggsave(plot=fsbar, file="./fsbar.png",scale=1.5)
ggsave(plot=fsbar, file="./fsbar.pdf",scale=1.1)

write.csv(fs,file="./fsdata_processed.csv")