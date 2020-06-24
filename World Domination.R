df<-read.csv("World Map Graph.csv", header=TRUE)



m=matrix(0,nrow=249, ncol=249)
x<-unique(df$country_code)
rownames(m) <- as.character(x)
colnames(m) <- as.character(x)
degree<-matrix(0,nrow=249, ncol=1)


for (i in 1:471) {

if   (df$country_border_code[i]!="" && !is.na(df$country_border_code[i])){
m[as.character(df$country_code[i]),as.character(df$country_border_code[i])]=1

}
}
for (i in 1:249){
degree[i]=sum(m[i,])
}

dom<-matrix(0,nrow=249, ncol=1)
bool<-TRUE


while (bool){
if (max(degree)!=0){
dom[which(degree==max(degree))]<-1

    for (i in 1:249){
       if (m[which(degree==max(degree)), i]==1){
           
           m[i,]=0
           m[,i]=0
       }
    }
m[which(degree==max(degree)),]=0
m[,which(degree==max(degree))]=0
for (i in 1:249){
    degree[i]=sum(m[i,])
}

}


else{bool=FALSE}

}
