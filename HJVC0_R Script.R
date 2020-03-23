#Setting WD
WD <- getwd()

#igraph
library(igraph)


# Non-Parade Adjacency Table
NoParade<- as.matrix(read.csv("MKN_Time_NonParade.csv",header = TRUE))
# Generating Graph
Attraction <- NoParade[,1]
NoParade <- NoParade[, -1]
colnames(NoParade) <- rownames(NoParade) <- Attraction
NoParade[is.na(NoParade)] <- 0
NoParade<- graph.adjacency(NoParade, weighted = TRUE)


#\#\#\#\#\

# Adding Additial Thematic Attributes

#434E9F Blue
#BA0A30 Pink
# Creating BTA Colour Ramp
palf <- colorRampPalette(c("#434E9F", "#BA0A30"))
plot(x=10:1, y=1:10, pch=19, cex=3, col=palf(7))

NodeAttribute<- read.csv("MKN_Node_Attributes.csv",header = TRUE)
names(NodeAttribute)
colnames(NodeAttribute)[1] = "Attraction"

Land <- as.character(NodeAttribute$Land)
LandColour <- as.character(NodeAttribute$LandColour)
ClassColour <- as.character(NodeAttribute$Class.Colour)
Class <- as.character(NodeAttribute$Class)
RideType <- as.character(NodeAttribute$Ride.Type)
RideColour <- as.character(NodeAttribute$Ride.Type.Colour)
BigThrills <- as.character(NodeAttribute$BigThrills)
BigThrillsColour <- as.character(NodeAttribute$BigThrillColour)
ParadeNode <- as.character(NodeAttribute$ParadeNode)
ParadeLab <- as.character(NodeAttribute$ParadeLab)
NodeSLabel <- as.character(NodeAttribute$NodeSLabel)
PPColour_NP <- as.character(NodeAttribute$PPColour_NP)
PPColour_P <- as.character(NodeAttribute$PPColour_P)
BTParade <- as.character(NodeAttribute$BT_Parade)
BTNoParade <- as.character(NodeAttribute$BT_NoParade)

## Adding Attribute Data
V(NoParade)$Land <- Land
V(NoParade)$LandColour <- LandColour
V(NoParade)$Class <- Class
V(NoParade)$ClassColour<- ClassColour
V(NoParade)$RideType<- RideType
V(NoParade)$RideColour<- RideColour
V(NoParade)$BigThrills <- BigThrills
V(NoParade)$BigThrillsColour <- BigThrillsColour
V(NoParade)$PPColour_NP <- PPColour_NP
V(NoParade)$BTNoParade <- BTNoParade
vertex_attr(NoParade)

edge_attr(NoParade)
E(NoParade)
gsize(NoParade)

write.table((edge_attr(NoParade)),file="NoPEdge.txt",row.names = FALSE)
#Connections go down each row. 
#\#\#\#\#\

# Plotting GraphS

## Setting up Additional Variables
l <- layout_with_fr(NoParade)
plot(NoParade,
     edge.arrow.size=0,
     edge.color="black",
     layout=l,
     vertex.label=NA,
     vertex.color=V(NoParade)$ClassColour,
     vertex.size=8)

#Themed Land Output
LandNames <- c("Main Street USA","Fantasyland","Adventureland","Frontierland","Liberty Square","Tomorrowland")
LndColour <- c("#D2242D","#D7147D","#F78A2F","#856858","#1AB1E6","#254390")

plot(NoParade,
     edge.arrow.size=.1,
     vertex.color=V(NoParade)$LandColour,
     vertex.size=8,
     vertex.label=NA,
     vertex.frame.color="black",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)
legend(x=-1.935702, y=0.7817387, LandNames, pch=21,
       col="black", pt.bg=LndColour, pt.cex=2, cex=0.8, bty="n", ncol=1)
text(x=-2.008949,y=0.9881603,pos=4,labels="Magic Kingdom Themed Lands",cex=NULL)

# OUTPUT Two
# Plotting Based on Node Type
Class #"Entrance" #"Pathway" #"Attraction"
ClassColour #"#FFFE00" #"#00CDFF" #"#FF3200"

plot(NoParade,
     edge.arrow.size=.1, #You can make this 0 to get rid of arrows. 
     vertex.color=V(NoParade)$ClassColour,
     vertex.size=8,
     vertex.label=NA,
     vertex.frame.color="black",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)
text(x=-2.008949,y=0.9881603,pos=4,labels="Magic Kingdom Network",cex=NULL)
legend(x=-1.935702, y=0.7817387, c("Entrance","Pathway","Attraction"), pch=21,
       col="black", pt.bg=c("#FFFE00","#00CDFF","#FF3200"), pt.cex=2, cex=0.8, bty="n", ncol=1)


# OUTPUT THREE: Big Thrill Attractions


## Making Legend
Legend <- data.frame(NodeAttribute$BigThrills,NodeAttribute$BigThrillColour)
Legend <- na.omit(Legend)
names(Legend)
colnames(Legend)[1] = "BigThrill"
colnames(Legend)[2]="Colour"
colnames(Legend)[2]="Colour"
Legend$Colour <- as.character(Legend$Colour)
Legend

## Plotting
plot(NoParade,
     edge.arrow.size=.1, 
     vertex.color=V(NoParade)$BigThrillsColour,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)
text(x=-2.539197,y=0.9976618,pos=4,labels="'Big Thrills' Attraction Name",cex=NULL)
legend(x=-2.580599, y=0.8906897, Legend$BigThrill, pch=21,
       col="black", pt.bg=Legend$Colour,
       pt.cex=2, cex=.8, bty="n", ncol=1)


## Parade Graph
Parade<- as.matrix(read.csv("MKN_Time_Parade.csv",header = TRUE))
Attraction <- Parade[,1]
Parade <- Parade[, -1]
colnames(Parade) <- rownames(Parade) <- Attraction
Parade[is.na(Parade)] <- 0
Parade<- graph.adjacency(Parade, weighted = TRUE)
plot(Parade,
     edge.arrow.size=0,
     edge.color=E(Parade)$EColour)

#Parade Route Edge Attributes
edge_attr(Parade)
write.table((edge_attr(Parade)),file="PEdge.txt",row.names = FALSE)

#Changing Parade Route Edge Attributes
EdgeAttribute<- read.csv("MKN_Edge_Attributes.csv",header = TRUE)
names(EdgeAttribute)

EdgeColour <- as.character(EdgeAttribute$E.Colour)
E(Parade)$EColour <- EdgeColour

#Checking the Edge IDs
AO<- get.edge.ids(Parade,c("Astro Orbiter","PeopleMover"))
AO #That's correct.

#Changing Parade Route Node Attributes
V(Parade)$ParadeNode <- ParadeNode
V(Parade)$ParadeLab <- ParadeLab
V(Parade)$NodeSLabel <- NodeSLabel
V(Parade)$PPColour_P <- PPColour_P
V(Parade)$BTParade <- BTParade

#Plotting the Parade Park Route
plot(Parade,
     edge.arrow.size=.1, 
     vertex.color=V(Parade)$ParadeNode,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="Black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color=E(Parade)$EColour,
     layout=l,
     frame=FALSE)
legend(x=-2.151107, y=0.0221884, c("Passes Directly","Does Not Pass"), pch=21,
       col="black", pt.bg=c("#443C3C","#CCCCBE"), pt.cex=2, cex=.8, bty="n", ncol=1)
legend(x=0.184514, y=0.991471, c("Impacted by Parade Route"), lty=1,
       col="#BA0A30", pt.cex=2, cex=.8, bty="n", ncol=1)
text(x=-2.046004,y=0.2557505,pos=4,labels="Parade Route",cex=NULL)

gsize(Parade)


## Calculating Shortest Paths from ALL NODES

Parade_ShortestPathTime <- (s.paths <- shortest.paths(Parade, algorithm = "dijkstra")) #Shows all Shortest Paths Between Nodes
write.csv(Parade_ShortestPathTime,file="Parade_ShortPathTime_V2.csv")
NoParade_ShortestPathTime <- (s.paths <- shortest.paths(NoParade, algorithm = "dijkstra"))
write.csv(NoParade_ShortestPathTime,file="NoParade_ShortPathTime_V2.csv")
#Shortest Path Times to Big Thrill Rides are greater during the parade.

# Calculating Shortest Paths from CC to Big Thrill Rides
BTRides <- c("Pirates of the Caribbean",
             "Splash Mountain",
             "Big Thunder Mountain Railroad",
             "Peter Pan's Flight",
             "The Barnstormer",
             "Seven Dwarfs Mine Train",
             "Space Mountain")
shortest_paths(Parade,"Cinderella's Castle",BTRides)
shortest_paths(NoParade,"Cinderella's Castle",BTRides)

Short <- shortest_paths(Parade,
                        from = V(Parade)[name=="Cinderella's Castle"],
                        to = V(Parade)[name=="Splash Mountain"],
                        output = "both") #Nodes and Edges Listed
Short

#Checking the Shortest Path Times
A <- E(Parade)$weight[get.edge.ids(Parade,c("Cinderella's Castle","ALB"))]
B <- E(Parade)$weight[get.edge.ids(Parade,c("ALB","Swiss Family Treehouse"))]
C <- E(Parade)$weight[get.edge.ids(Parade,c("Swiss Family Treehouse","Jungle Cruise"))]
D <- E(Parade)$weight[get.edge.ids(Parade,c("Jungle Cruise","Pirates of the Caribbean"))]
E <- E(Parade)$weight[get.edge.ids(Parade,c("Pirates of the Caribbean","FLB1"))]
G <- E(Parade)$weight[get.edge.ids(Parade,c("FLB1","Splash Mountain"))]
Test1 <- c(A,B,C,D,E,G)
sum(Test1) #19

# Plotting Peter Pan's Flight
l <- layout_with_fr(NoParade)

#No Parade
plot(NoParade,
     edge.arrow.size=.1, 
     vertex.color=V(NoParade)$PPColour_NP,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)

#Parade Running
plot(Parade,
     edge.arrow.size=.1, 
     vertex.color=V(Parade)$PPColour_P,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)

#Plotting Big Thuder Mountain
#No Parade
plot(NoParade,
     edge.arrow.size=.1, 
     vertex.color=V(NoParade)$BTNoParade,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)
#Parade
plot(Parade,
     edge.arrow.size=.1, 
     vertex.color=V(Parade)$BTParade,
     vertex.size=8,
     vertex.label= NA,
     vertex.label.color="black",
     vertex.label.cex=0.8, 
     vertex.label.dist=1,
     vertex.frame.color="black",
     vertex.label.family="Arial",
     edge.curved=.2,
     edge.color="#616161",
     layout=l,
     frame=FALSE)

#Chi Squared

AllChi<- read.csv("All_Chi.csv",header = TRUE)
All_Chi <- chisq.test(AllChi)
All_Chi

BTAChi <- read.csv("Chi_Rides.csv",header = TRUE)
BTA_Chi <- chisq.test(BTAChi)
BTA_Chi