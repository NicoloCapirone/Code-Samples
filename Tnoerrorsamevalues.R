#T



t3DAG <- read.csv("./t3DAG.csv")




t3DAG$Y <- t3DAG$Z*0.5 - t3DAG$X + t3DAG$U*20 + 1

t3DAG$Y <- round(t3DAG$Y)



#Probabilities of Z

t3tabz <- as.data.frame(prop.table(table(t3DAG$Z)))

t3tabz$Var1 <- as.integer(levels(t3tabz$Var1))

t3pz <- c()


for (i in 1:length(t3DAG$Z)){
  for (j in 1:length(t3tabz$Var1)){
    if (t3DAG$Z[i] == t3tabz$Var1[j]){
      t3pz <- append(t3pz, t3tabz$Freq[j], after = length(t3pz))
    }
  }  
}


t3DAG <- cbind(t3DAG, t3pz)

#Probabilities of X




t3tabx <- as.data.frame(prop.table(table(t3DAG$X)))

t3tabx$Var1 <- as.integer(levels(t3tabx$Var1))

t3px <- c()

for (i in 1:length(t3DAG$X)){
  if (t3DAG$X[i] == 1){
    t3px <- append(t3px, t3tabx$Freq[2], after = length(t3px))
  }  else{
    t3px <- append(t3px, t3tabx$Freq[1], after = length(t3px))
  }
}


t3DAG <- cbind(t3DAG, t3px)



#Probabilities of Y

t3taby <- as.data.frame(prop.table(table(t3DAG$Y)))

t3taby$Var1 <- as.integer(levels(t3taby$Var1))

t3py <- c()

for (i in 1:length(t3DAG$Y)){
  for (j in 1:length(t3taby$Var1)){
    if (t3DAG$Y[i] == t3taby$Var1[j]){
      t3py <- append(t3py, t3taby$Freq[j], after = length(t3py))
    }
  }  
}


t3DAG <- cbind(t3DAG, t3py)



#PXYZ




t3PYZX <- data.frame(Y = t3DAG$Y)

g = 0

c = 1

for (l in 0:1){
  for (v in c(14:27)){
    
    c = 1 + c
    
    t3tabyzs <- as.data.frame(prop.table(table(t3DAG$Y[t3DAG$X == l & t3DAG$Z == v])))
    
    t3tabyzs$Var1 <- as.integer(levels(t3tabyzs$Var1))
    
    if (length(t3tabyzs$Var1) != 0){
      
      t3pyzs <- c()
      
      for (k in 1:length(t3DAG$Y)){
        
        for (j in 1:length(t3tabyzs$Var1)){
          
          if (t3DAG$Y[k] == t3tabyzs$Var1[j]){
            
            t3pyzs <- append(t3pyzs, t3tabyzs$Freq[j], after = length(t3pyzs))
            
          } 
          
          else {
            
            g = g +1 
            
          }
          
        }
        
        if (g == length(t3tabyzs$Var1)){
          
          t3pyzs <- append(t3pyzs, 0, after = length(t3pyzs))
          
        }
        
        g=0
        
      }
      
    }
    else {
      t3pyzs <- rep(0, each=2500)
      print(paste(l,v))
    }
    t3PYZX <- cbind(t3PYZX, t3pyzs)  
    
    colnames(t3PYZX)[c] = paste("Y&x(",l,")&z(",v,")", sep = "")
    
    t3pyzs <- c()
    
  }
  
}


t3pyzx <- distinct(t3PYZX, Y, .keep_all = TRUE)

t3pyzx <- t3pyzx[order(t3pyzx$Y),]




t3pyz0 <- t3pyzx[1:15]

t3pyz1 <- t3pyzx[c(1,16:29)]

t3pyz0 <- data.frame(t(t3pyz0[-1]))

t3pyz1 <- data.frame(t(t3pyz1[-1]))


for (i in 1:5){
  colnames(t3pyz0)[i] = paste("y=",i+13, sep = "")
  colnames(t3pyz1)[i] = paste("y=",i+13, sep = "")
}






t3y0 <- 0

t3Y0 <- c()

for (j in 1:4){
  for (i in 1:14){
    t3z0 <- t3pyz0[i,j]*t3tabz$Freq[i]
    t3y0 <- t3z0 + t3y0
  }
  t3Y0 <- append(t3Y0, t3y0, after = length(t3Y0))
  t3y0 <- 0
}




t_s3y0 <- 0

t_s3Y0 <- c()

for (j in 1:4){
  for (i in 1:14){
    t_s3z0 <- s3pyz0[i,j]*t3tabz$Freq[i]
    t_s3y0 <- t_s3z0 + t_s3y0
  }
  t_s3Y0 <- append(t_s3Y0, t_s3y0, after = length(t_s3Y0))
  t_s3y0 <- 0
}






t3y1 <- 0

t3Y1 <- c()

for (j in 1:4){
  for (i in 1:14){
    t3z1 <- t3pyz1[i,j]*t3tabz$Freq[i]
    t3y1 <- t3z1 + t3y1
  }
  t3Y1 <- append(t3Y1, t3y1, after = length(t3Y1))
  t3y1 <- 0
}




t_s3y1 <- 0

t_s3Y1 <- c()

for (j in 1:4){
  for (i in 1:14){
    t_s3z1 <- s3pyz1[i,j]*t3tabz$Freq[i]
    t_s3y1 <- t_s3z1 + t_s3y1
  }
  t_s3Y1 <- append(t_s3Y1, t_s3y1, after = length(t_s3Y1))
  t_s3y1 <- 0
}




results_ts <- data.frame(t_s3Y1,t_s3Y0,t3Y1,t3Y0)

write.table(results_ts, file = "results_ts.csv", sep = ",")

