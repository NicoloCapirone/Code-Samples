

t4DAG <- read.csv("/Users/nicolocapirone/Documents/Uni/BU/Causal Inference/Problem Sets/Transportability Code/t4DAG.csv")




t4DAG$Y <- t4DAG$Z*0.5 - t4DAG$X + 1

t4DAG$Y <- round(t4DAG$Y)




#Probabilities of Z

t4tabz <- as.data.frame(prop.table(table(t4DAG$Z)))

t4tabz$Var1 <- as.integer(levels(t4tabz$Var1))

t4pz <- c()


for (i in 1:length(t4DAG$Z)){
  for (j in 1:length(t4tabz$Var1)){
    if (t4DAG$Z[i] == t4tabz$Var1[j]){
      t4pz <- append(t4pz, t4tabz$Freq[j], after = length(t4pz))
    }
  }  
}


t4DAG <- cbind(t4DAG, t4pz)

#Probabilities of X




t4tabx <- as.data.frame(prop.table(table(t4DAG$X)))

t4tabx$Var1 <- as.integer(levels(t4tabx$Var1))

t4px <- c()

for (i in 1:length(t4DAG$X)){
  if (t4DAG$X[i] == 1){
    t4px <- append(t4px, t4tabx$Freq[2], after = length(t4px))
  }  else{
    t4px <- append(t4px, t4tabx$Freq[1], after = length(t4px))
  }
}


t4DAG <- cbind(t4DAG, t4px)



#Probabilities of Y

t4taby <- as.data.frame(prop.table(table(t4DAG$Y)))

t4taby$Var1 <- as.integer(levels(t4taby$Var1))

t4py <- c()

for (i in 1:length(t4DAG$Y)){
  for (j in 1:length(t4taby$Var1)){
    if (t4DAG$Y[i] == t4taby$Var1[j]){
      t4py <- append(t4py, t4taby$Freq[j], after = length(t4py))
    }
  }  
}


t4DAG <- cbind(t4DAG, t4py)



#PXYZ




t4PYZX <- data.frame(Y = t4DAG$Y)

g = 0

c = 1

for (l in 0:1){
  for (v in c(14:27)){
    
    c = 1 + c
    
    t4tabyzs <- as.data.frame(prop.table(table(t4DAG$Y[t4DAG$X == l & t4DAG$Z == v])))
    
    t4tabyzs$Var1 <- as.integer(levels(t4tabyzs$Var1))
    
    if (length(t4tabyzs$Var1) != 0){
      
      t4pyzs <- c()
      
      for (k in 1:length(t4DAG$Y)){
        
        for (j in 1:length(t4tabyzs$Var1)){
          
          if (t4DAG$Y[k] == t4tabyzs$Var1[j]){
            
            t4pyzs <- append(t4pyzs, t4tabyzs$Freq[j], after = length(t4pyzs))
            
          } 
          
          else {
            
            g = g +1 
            
          }
          
        }
        
        if (g == length(t4tabyzs$Var1)){
          
          t4pyzs <- append(t4pyzs, 0, after = length(t4pyzs))
          
        }
        
        g=0
        
      }
      
    }
    else {
      t4pyzs <- rep(0, each=2500)
    }
    t4PYZX <- cbind(t4PYZX, t4pyzs)  
    
    colnames(t4PYZX)[c] = paste("Y&x(",l,")&z(",v,")", sep = "")
    
    t4pyzs <- c()
    
  }
  
}


t4pyzx <- distinct(t4PYZX, Y, .keep_all = TRUE)

t4pyzx <- t4pyzx[order(t4pyzx$Y),]




t4pyz0 <- t4pyzx[1:15]

t4pyz1 <- t4pyzx[c(1,16:29)]

t4pyz0 <- data.frame(t(t4pyz0[-1]))

t4pyz1 <- data.frame(t(t4pyz1[-1]))





for (i in 1:8){
  colnames(t4pyz0)[i] = paste("y=",i+6, sep = "")
  colnames(t4pyz1)[i] = paste("y=",i+6, sep = "")
}





t4y0 <- 0

t4Y0 <- c()

for (j in 1:8){
  for (i in 1:14){
    t4z0 <- t4pyz0[i,j]*t4tabz$Freq[i]
    t4y0 <- t4z0 + t4y0
  }
  t4Y0 <- append(t4Y0, t4y0, after = length(t4Y0))
  t4y0 <- 0
}




t_s4y0 <- 0

t_s4Y0 <- c()

for (j in 1:8){
  for (i in 1:14){
    t_s4z0 <- s4pyz0[i,j]*t4tabz$Freq[i]
    t_s4y0 <- t_s4z0 + t_s4y0
  }
  t_s4Y0 <- append(t_s4Y0, t_s4y0, after = length(t_s4Y0))
  t_s4y0 <- 0
}






t4y1 <- 0

t4Y1 <- c()

for (j in 1:8){
  for (i in 1:13){
    t4z1 <- t4pyz1[i,j]*t4tabz$Freq[i]
    t4y1 <- t4z1 + t4y1
  }
  t4Y1 <- append(t4Y1, t4y1, after = length(t4Y1))
  t4y1 <- 0
}




t_s4y1 <- 0

t_s4Y1 <- c()

for (j in 1:8){
  for (i in 1:13){
    t_s4z1 <- s4pyz1[i,j]*t4tabz$Freq[i]
    t_s4y1 <- t_s4z1 + t_s4y1
  }
  t_s4Y1 <- append(t_s4Y1, t_s4y1, after = length(t_s4Y1))
  t_s4y1 <- 0
}







results_tsn <- data.frame(t_s4Y1,t_s4Y0,t4Y1,t4Y0)

write.table(results_tsn, file = "results_tsn.csv", sep = ",")


tsy1 <- sum(results_tsn[,1]*t4taby$Var1)
tsy0 <- sum(results_tsn[,2]*t4taby$Var1)
ty1 <- sum(results_tsn[,3]*t4taby$Var1)
ty0 <- sum(results_tsn[,4]*t4taby$Var1)


ATE3 <- tsy1-tsy0
ATE4 <- ty1-ty0



