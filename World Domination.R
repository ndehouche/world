            library(lpSolve)
            library(rworldmap)
            df<-read.csv("World Domination.csv", header=TRUE)
            
            
            
            m=matrix(0,nrow=249, ncol=249, byrow = TRUE)
            x<-unique(df$country_code)
            rownames(m) <- as.character(x)
            colnames(m) <- as.character(x)
            deg<-matrix(0,nrow=249, ncol=1)
            
            
            for (i in 1:471) {
            
            if   (df$country_border_code[i]!="" && !is.na(df$country_border_code[i])){
            m[as.character(df$country_code[i]),as.character(df$country_border_code[i])]=1
            
            }
            }
            
            
            for (i in 1:249) {
            deg[i]<-sum(m[i,])
            m[i,i]<-1      
                }
            
            
            #Dominating set
            
            f.obj <- replicate(249, 1)
            f.dir <- replicate(249,">=")
            f.rhs <- replicate(249, 1)
            for (i in 1:249) {
            if (sum(m[i,])==0){
                f.rhs[i]<-0
                
            }
            
            }
            lp("min", f.obj, m, f.dir, f.rhs, int.vec = 1:249, all.bin = TRUE)
            x<-lp("min", f.obj, m, f.dir, f.rhs, int.vec = 1:249, all.bin = TRUE)$solution
            
            rownames(m[which(x==1),])
            rownames(m[which( (x==1) |(deg==0) ),])
            
            
            #Double-dominating set
            f.rhs <- replicate(249, 2)
            for (i in 1:249) {
                if (sum(m[i,])==0){
                    f.rhs[i]<-0
                    
                }
            else if (sum(m[i,])==1){
                    f.rhs[i]<-1
                    
                }
                
            }
            lp("min", f.obj, m, f.dir, f.rhs, int.vec = 1:249, all.bin = TRUE)
            x<-lp("min", f.obj, m, f.dir, f.rhs, int.vec = 1:249, all.bin = TRUE)$solution
            
            
            rownames(m[which( (x==1) |(deg==1) |(deg==0) ),])
