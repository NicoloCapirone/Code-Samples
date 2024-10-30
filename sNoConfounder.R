s4DAG <- read.csv("./s4DAG.csv")



s4DAG$Y <- s4DAG$Z*0.5 - s4DAG$X + 1

s4DAG$Y <- round(s4DAG$Y)



#Probabilities of Z

s4tabz <- as.data.frame(prop.table(table(s4DAG$Z)))

s4tabz$Var1 <- as.integer(levels(s4tabz$Var1))

s4pz <- c()


for (i in 1:length(s4DAG$Z)){
  for (j in 1:length(s4tabz$Var1)){
    if (s4DAG$Z[i] == s4tabz$Var1[j]){
      s4pz <- append(s4pz, s4tabz$Freq[j], after = length(s4pz))
    }
  }  
}


s4DAG <- cbind(s4DAG, s4pz)

#Probabilities of X




s4tabx <- as.data.frame(prop.table(table(s4DAG$X)))

s4tabx$Var1 <- as.integer(levels(s4tabx$Var1))

s4px <- c()

for (i in 1:length(s4DAG$X)){
  if (s4DAG$X[i] == 1){
    s4px <- append(s4px, s4tabx$Freq[2], after = length(s4px))
  }  else{
    s4px <- append(s4px, s4tabx$Freq[1], after = length(s4px))
  }
}


s4DAG <- cbind(s4DAG, s4px)



#Probabilities of Y

s4taby <- as.data.frame(prop.table(table(s4DAG$Y)))

s4taby$Var1 <- as.integer(levels(s4taby$Var1))

s4py <- c()

for (i in 1:length(s4DAG$Y)){
  for (j in 1:length(s4taby$Var1)){
    if (s4DAG$Y[i] == s4taby$Var1[j]){
      s4py <- append(s4py, s4taby$Freq[j], after = length(s4py))
    }
  }  
}


s4DAG <- cbind(s4DAG, s4py)



#PXYZ


prop.table(table(s4DAG$Z))

s4PYZX <- data.frame(Y = s4DAG$Y)

g = 0

c = 1

for (l in 0:1){
  for (v in c(14:27)){
    
    c = 1 + c
    
    s4tabyzs <- as.data.frame(prop.table(table(s4DAG$Y[s4DAG$X == l & s4DAG$Z == v])))
    
    s4tabyzs$Var1 <- as.integer(levels(s4tabyzs$Var1))
    
    if (length(s4tabyzs$Var1) != 0){
      
      s4pyzs <- c()
      
      for (k in 1:length(s4DAG$Y)){
        
        for (j in 1:length(s4tabyzs$Var1)){
          
          if (s4DAG$Y[k] == s4tabyzs$Var1[j]){
            
            s4pyzs <- append(s4pyzs, s4tabyzs$Freq[j], after = length(s4pyzs))
            
          } 
          
          else {
            
            g = g +1 
            
          }
          
        }
        
        if (g == length(s4tabyzs$Var1)){
          
          s4pyzs <- append(s4pyzs, 0, after = length(s4pyzs))
          
        }
        
        g=0
        
      }
      
    }
    else {
      s4pyzs <- rep(0, each=2500)
      print(paste(l,v))
    }
    s4PYZX <- cbind(s4PYZX, s4pyzs)  
    
    colnames(s4PYZX)[c] = paste("Y&x(",l,")&z(",v,")", sep = "")
    
    s4pyzs <- c()
    
  }
  
}


s4pyzx <- distinct(s4PYZX, Y, .keep_all = TRUE)

s4pyzx <- s4pyzx[order(s4pyzx$Y),]




s4pyz0 <- s4pyzx[1:15]

s4pyz1 <- s4pyzx[c(1,16:29)]

s4pyz0 <- data.frame(t(s4pyz0[-1]))

s4pyz1 <- data.frame(t(s4pyz1[-1]))



for (i in 1:8){
  colnames(s4pyz0)[i] = paste("y=",i+6, sep = "")
  colnames(s4pyz1)[i] = paste("y=",i+6, sep = "")
}








s4y0 <- 0
s4Y0 <- c()

for (j in 1:8){
  for (i in 1:14){
    s4z0 <- s4pyz0[i,j]*s4tabz$Freq[i]
    s4y0 <- s4z0 + s4y0
  }
  s4Y0 <- append(s4Y0, s4y0, after = length(s4Y0))
  s4y0 <- 0
}





s_t4y0 <- 0
s_t4Y0 <- c()

for (j in 1:8){
  for (i in 1:14){
    s_t4z0 <- t4pyz0[i,j]*s4tabz$Freq[i]
    s_t4y0 <- s_t4z0 + s_t4y0
  }
  s_t4Y0 <- append(s_t4Y0, s_t4y0, after = length(s_t4Y0))
  s_t4y0 <- 0
}


print(s_t4Y0)

print(s4Y0)




s4y1 <- 0

s4Y1 <- c()

for (j in 1:8){
  for (i in 1:14){
    s4z1 <- s4pyz1[i,j]*s4tabz$Freq[i]
    s4y1 <- s4z1 + s4y1
  }
  s4Y1 <- append(s4Y1, s4y1, after = length(s4Y1))
  s4y1 <- 0
}






s_t4y1 <- 0

s_t4Y1 <- c()

for (j in 1:8){
  for (i in 1:14){
    s_t4z1 <- t4pyz1[i,j]*s4tabz$Freq[i]
    s_t4y1 <- s_t4z1 + s_t4y1
  }
  s_t4Y1 <- append(s_t4Y1, s_t4y1, after = length(s_t4Y1))
  s_t4y1 <- 0
}



results_stn <- data.frame(s_t4Y1,s_t4Y0,s4Y1,s4Y0)

write.table(results_stn, file = "results_stn.csv", sep = ",")


sty1 <- sum(results_stn[,1]*s4taby$Var1)
sty0 <- sum(results_stn[,2]*s4taby$Var1)
sy1 <- sum(results_stn[,3]*s4taby$Var1)
sy0 <- sum(results_stn[,4]*s4taby$Var1)


ATE1 <- sty1-sty0
ATE2 <- sy1-sy0




