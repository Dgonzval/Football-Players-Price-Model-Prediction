#UPc Project

setwd("C:/Users/Ordenador/Desktop/UPC Project/Bases")

library(readxl)
library(data.table)
library(dplyr)

players<-read.csv("2_players.csv")
stats<-read.csv("5_stats.csv")
values<-read.csv("6_values.csv")
teams<-read.csv("df_team_value.csv")

#Stats

#Making an aggregation of the stats per year.

bs<-stats%>%
  group_by(player_id,name,year)%>%
  summarise(minutes=sum(minutes, na.rm = TRUE),
            goals=sum(goals, na.rm = TRUE),
            assists=sum(assists, na.rm = TRUE),
            bench=sum(matches_from_bench, na.rm = TRUE))

bs$player_id<-as.character(bs$player_id)
bs$year<-as.character(bs$year)


#Players

#players<-subset(players,players$year==2020|players$year==2019|players$year==2018)

players$seleccion<-ifelse(players$nation_matches=="[]",0,1)
players%>%group_by(seleccion)%>%summarize(mean(market_value,na.rm=TRUE))

##Positions##
players$pos<-ifelse(players$position=="Defensa" | players$position=="Defensa central" |
             players$position=="Defensa central" | players$position=="Lateral derecho"|
             players$position=="Lateral izquierdo", "Defensa",
             ifelse(players$position=="Delantero" | players$position=="Delantero centro" |
             players$position=="Extremo derecho" | players$position=="Extremo izquierdo", "Delantero",
             ifelse(players$position=="Interior derecho" | players$position=="Interior izquierdo" |
             players$position=="Mediapunta" | players$position=="Medio campo"|
             players$position=="Mediocentro" |players$position=="Mediocentro ofensivo"|
             players$position=="Pivote", "Medio Centro", "Portero")))

table(players$pos)

##Equipos##

players$equipo<-ifelse(players$team=="Bayern MÃºnich"| players$team=="Manchester City"|
                         players$team=="Liverpool FC"| players$team=="Chelsea FC"|
                         players$team=="Real Madrid CF"| players$team=="ParÃ­s Saint-Germain FC"|
                         players$team=="FC Barcelona"| players$team=="Juventus de TurÃ­n"|
                         players$team=="Manchester United"|players$team=="AtlÃ©tico de Madrid","1",
                ifelse(players$team=="Sevilla FC"| players$team=="AS Roma"|
                         players$team=="Tottenham Hotspur"| players$team=="Ajax de Ãmsterdam"|
                         players$team=="Arsenal FC"| players$team=="Borussia Dortmund"|
                         players$team=="FC Oporto"| players$team=="RasenBallsport Leipzig"|
                         players$team=="Villarreal CF"|players$team=="Olympique de Lyon"|
                         players$team=="Inter de MilÃ¡n"|
                         players$team=="AC Milan"| players$team=="SL Benfica"|
                         players$team=="Sporting CP"|players$team=="Lazio", "2", "0"))

players$id<-as.character(players$id)
table(players$equipo)

#Values
#Años 19-15
values$player_id<-as.character(values$player_id)
values$year<-substr(values$date,1,4)

#values_1<-subset(x=values,values$year=="2020"|values$year=="2019"|values$year=="2018")

#Making an aggregation in the values, the avg value per year.

bv<-values%>%
  group_by(player_id,name,year)%>%
  summarise(value=mean(value, na.rm = TRUE))

bv$lvalue<-lag(bv$value)

#Teams

teams$year<-as.character(teams$year)

#Merge de Bases
players$year<-as.character(players$year)

base<-bs%>%
  left_join(select(players,id,year,age,pos,equipo,seleccion,team), by=c("player_id"="id","year"="year"))%>%
  left_join(select(bv,player_id,year,value,lvalue,name), by=c("player_id"="player_id","year"="year"))

base<-base%>%
 left_join(select(teams,name,total_market_value,year), by=c("team"="name","year"="year"))

base_mod<-base%>%select(player_id,year,seleccion,name.x,age,pos,equipo,team,minutes,goals,assists,bench,value,lvalue,total_market_value)
base_mod_1<-na.omit(base_mod)

#modelo<-lm(log(value)~minutes+pos+age+lvalue+total_market_value+seleccion,data =base )

#summary(modelo)

####Base para modelo####
base_ori<-base
base<-base_mod_1

VC <- base%>%select(age,minutes,lvalue,total_market_value,goals,assists)
dummie.0<-base%>%select(seleccion)
dummie.n<-base%>%select(pos,equipo)

library(fastDummies)

dummie.n <- dummy_cols(dummie.n, c("pos","equipo"))

dummie.n<-dummie.n[,4:10]

base = cbind(VC,dummie.0,dummie.n,value=base$value)
base<-base[,c(-1,-8)]

save(base,file = "base")
write.csv(base,file = "base.csv")

####Estadistica Descriptiva####
#base_mod_1

#Valor de Jugadores de Seleccion

library(dplyr)

selec_graph<-base_mod_1%>%group_by(seleccion)%>%summarize(Mean_Market_Value=mean(value,na.rm=TRUE))

selec_graph$seleccion<-as.character(selec_graph$seleccion)
selec_graph$seleccion <- factor(selec_graph$seleccion, levels=c("1","0"))

ggplot(data=selec_graph,aes(x =seleccion,y =Mean_Market_Value/1000000))+
  geom_bar(stat = "identity",width=0.5)+
  labs(x="National Team",y="Mean Market Value (MM EUR)")

#Posicion vs Precio

pos_graph<-base_mod_1%>%group_by(pos)%>%
  summarize(Mean_Market_Value=mean(value,na.rm=TRUE))

pos_graph$pos <- factor(pos_graph$pos, levels=c("Delantero","Medio Centro","Defensa","Portero"))


ggplot(data=pos_graph,aes(x = pos,y =Mean_Market_Value/1000000))+
  geom_bar(stat = "identity",width=0.5)+
  labs(x="Position",y="Mean Market Value (MM EUR)")

#Edad vs Precio

library(ggplot2)

ggplot(data=players,aes(x = age,y = market_value))+geom_point()+
  labs(x="Age",y="Market Value")

ggplot(data=base_mod_1,aes(x = age,y = value/1000000))+geom_point()+
  labs(x="Age",y="Market Value (MM EUR)")

#Minutos vs Precio

library(ggplot2)

ggplot(data=base_mod_1,aes(x = minutes,y = value))+geom_point()+
  labs(x="Minutes",y="Market Value")

#Minutos vs Edad

ggplot(data=base_mod_1,aes(x = age,y = minutes))+geom_point()+
  labs(x="Age",y="Mean Minutes")


#Matriz de correlaciones

base_mod_2<-base_mod_1%>%select(seleccion,age,minutes,value,lvalue,total_market_value)
base_mod_2<-base_mod_2[,-1]

correlacion<-round(cor(base_mod_2), 1)
library(corrplot)
corrplot(correlacion, method="number", type="upper")

#Equipo vs Jugador (valores)

ggplot(data=base_mod_1,aes(x = total_market_value/1000000,y = value/1000000))+geom_point()+
  labs(x="Team Market Value (MM EUR)",y="Player Value (MM EUR)")

#Base para ejemplos

#Base Dony
base$value<-as.character(base$value)
base_Ansu<-base%>%filter(value=="42666666.6666667" & age==24)

write.csv(x = base_Ansu,file = "Ansu.csv")

#Frankie
base_Ansu<-base%>%filter(minutes=="4492" & goals==7)
write.csv(x = base_Ansu,file = "Ansu.csv")

#Neto
base_Ansu<-base%>%filter(minutes==4284 & equipo_1==1)
write.csv(x = base_Ansu,file = "Ansu.csv")

#stats

base_log<-base
base_log$log_lvalue<-log(base_log$lvalue)
base_log$lmkval<-log(base_log$total_market_value)
summary(base_log)
sd(base_log$log_lvalue)
sd(base_log$lmkval)
