require(ggplot2)
require(scales)
require(grid)
require(ggthemes)

fs <- read.csv("./FSstats.csv")
str(fs)

years  <- unique(fs$Year)

fs <- rename(fs,c("Total.Applicants" = "Apps", "Total.Successful" = "Offers",
                  "Degree.Type" = "Degree","Overall.Success.Rate"="SuccessRate"))

fs$Apps <- fs$Total.Applicants
fs$Offers <- fs$Total.Successful
fs$Degree <- fs$Degree.Type
fs$SuccessRate <- fs$Overall.Success.Rate

fs$Degree.Type <- NULL
fs$Total.Applicants <- NULL
fs$Total.Successful <- NULL
fs$Overall.Success.Rate <- NULL

for (i in years) {
  fs$GrandApps <- fs$Apps[fs$Degree=="Overall" & fs$Year==i]
  fs$GrandOffers[fs$Year==i] <- fs$Offers[fs$Degree=="Overall" &
                                                      fs$Year==i]
}

fs <- fs[fs$Degree !="Overall", ]

fs$SuccessRate <- fs$Offers/fs$Apps

fs$OverallSuccessRate <- fs$GrandOffers/fs$GrandApps

fs$RelativeSuccessRate <- fs$SuccessRate/fs$OverallSuccessRate-1

fs$ShareofApps  <- fs$Apps/fs$GrandApps 
fs$ShareofOffers  <- fs$Offers/fs$GrandOffers

fs$Area[fs$Degree=="Mathematics"] <- "STEM" 
fs$Area[fs$Degree=="Sciences (Physical & Biology)"] <- "STEM" 
fs$Area[fs$Degree=="Engineering"] <- "STEM" 
fs$Area[fs$Degree=="Technology"] <- "STEM" 
fs$Area[fs$Degree=="Medicine"] <- "Medicine" 
fs$Area[fs$Degree=="Business"] <- "Business" 
fs$Area[fs$Degree=="Other"] <- "Other" 
fs$Area[fs$Degree=="Multi Discipline"] <- "Multi" 
fs$Area[fs$Degree=="Economics"] <- "SocSci & Humanities" 
fs$Area[fs$Degree=="Languages"] <- "SocSci & Humanities" 
fs$Area[fs$Degree=="Social Sciences"] <- "SocSci & Humanities" 
fs$Area[fs$Degree=="Humanities"] <- "SocSci & Humanities"

fs$Area <- as.factor(fs$Area)
fs <- droplevels.data.frame(fs)

fsplot <- ggplot(fs, aes(ShareofApps, ShareofOffers)) +
  geom_point(aes(size=log(fs$Offers), colour=fs$Area)) +
  geom_point() +
  geom_text(aes(label=fs$Degree))+
  facet_wrap(~Year)
fsplot

fsbar <- ggplot(fs, aes(Degree,RelativeSuccessRate)) +
  geom_bar(aes(fill=fs$Offers),stat="identity",position="dodge")+
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

ggsave(plot=fsplot, file="./fsplot.png",scale=1.1)

write.csv(fs,file="./fsdata_processed.csv")