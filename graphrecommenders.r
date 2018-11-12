
COCITATION<-function(HASH=HASH,numbers=10, n=5){

HASHM<-HASH[,c('Source.User','Dest.User')]
		HASHM<-unique(HASHM)
		dim(HASHM)
#PICK UP THE REAL DEST.DOMAIN
		#HASH$EQUAL<-gsub('.+@','',HASH$Dest.User)
		T<-as.matrix(HASHM)
		dim(T)
		#[1] 27170     2
		g<-graph.edgelist(T)
		#plot(g)
		g
		#IGRAPH DN-- 34072 27170 -- 
		#+ attr: name (v/c)
		
		#access a particular vertex
		#print(V(g)[10])
		#Vertex sequence:
	


		#1#CALCULATE COCITATION
		COCIT<-cocitation(g, v=V(g)[numbers])
		sum(COCIT)
		#WHICH COCIT ?
		#which(COCIT==1) # will return the index 
		#colnames(COCIT)[which(COCIT==1)] will return the cocited person
			

		#	MAX COCITED 
		#	colnames(COCIT)[which(COCIT==max(COCIT))]
		U<-COCIT[order(-COCIT)]
		#print(head(U))
		#[1] 5 3 2 2 2 2
		
		print(V(g)[numbers])
		
		
		SIM<-similarity.invlogweighted(g,vids=V(g)[numbers])
		#[1] "adam.
		dim(SIM)
		#[1]     1 34072
		max(SIM)
		#[1] 21.76621
		colnames(SIM)<-colnames(COCIT)
		colnames(SIM)[which(SIM==max(SIM))]
		#[1] "dineshbabu.jayaraman.thomsonreuters.com@reuters.net"	
		# PICKING UP ALL THE RECOM
		Z<-SIM[order(-SIM)]
		#print(colnames(SIM)[which(SIM==Z)[1:n]])
		#[1] "adam.
		print("intersection of Recommended users for user: ")
		print (intersect(colnames(COCIT)[which(COCIT==U)[1:n]],colnames(SIM)[which(SIM==Z)[1:n]]))
		#print("all Recommended users for user: ")
		return(data.frame(cbind(colnames(COCIT)[which(COCIT==U)[1:n]],colnames(SIM)[which(SIM==Z)[1:n]])))
		
	}
	COCITATION(HASH,n=2)

LADAR<-function(HASH=HASH,numbers=10, n=5){

HASHM<-HASH[,c('Source.User','Dest.User')]
		HASHM<-unique(HASHM)
		dim(HASHM)
#PICK UP THE REAL DEST.DOMAIN
		HASH$EQUAL<-gsub('.+@','',HASH$Dest.User)
		T<-as.matrix(HASHM)
		dim(T)
		#[1] 27170     2
		g<-graph.edgelist(T)
		#plot(g)
		g
		#IGRAPH DN-- 34072 27170 -- 
		#+ attr: name (v/c)
		
		#access a particular vertex
		#print(V(g)[10])
		#Vertex sequence:
		#[1] "kashief.x.abdurahman@jpmorgan.com"


		#1#CALCULATE COCITATION
		COCIT<-similarity.invlogweighted(g,vids=V(g)[numbers])
		sum(COCIT)
		#WHICH COCIT ?
		#which(COCIT==1) # will return the index 
		#colnames(COCIT)[which(COCIT==1)] will return the cocited person
			

		#	MAX COCITED 
		#	colnames(COCIT)[which(COCIT==max(COCIT))]
		U<-COCIT[order(-COCIT)]
		print(head(U))
		#[1] 5 3 2 2 2 2
		print("Recommended users for user: ")
		print(V(g)[numbers])
		print(colnames(COCIT)[which(COCIT==U)[1:n]])

		SIM<-similarity.invlogweighted(g,vids=V(g)[numbers])
		#[1] "adam.
		dim(SIM)
		#[1]     1 34072
		max(SIM)
		#[1] 21.76621
		colnames(SIM)<-colnames(COCIT)
		colnames(SIM)[which(SIM==max(SIM))]
		#[1] "dineshbabu.jayaraman.thomsonreuters.com@reuters.net"	
		# PICKING UP ALL THE RECOM
		Z<-SIM[order(-SIM)]
		colnames(SIM)[which(SIM==Z)[1:5]]
		}
LADAR(HASH,n=2)