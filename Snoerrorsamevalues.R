library(faux)




s3DAG <- read.csv("./s3DAG.csv")



s3DAG$Y <- s3DAG$Z*0.5 - s3DAG$X + s3DAG$U*20 + 1

s3DAG$Y <- round(s3DAG$Y)



#Probabilities of Z

s3tabz <- as.data.frame(prop.table(table(s3DAG$Z)))

s3tabz$Var1 <- as.integer(levels(s3tabz$Var1))

s3pz <- c()


for (i in 1:length(s3DAG$Z)){
  for (j in 1:length(s3tabz$Var1)){
    if (s3DAG$Z[i] == s3tabz$Var1[j]){
      s3pz <- append(s3pz, s3tabz$Freq[j], after = length(s3pz))
    }
  }  
}


s3DAG <- cbind(s3DAG, s3pz)

#Probabilities of X


s3tabx <- as.data.frame(prop.table(table(s3DAG$X)))

s3tabx$Var1 <- as.integer(levels(s3tabx$Var1))

s3px <- c()

for (i in 1:length(s3DAG$X)){
  if (s3DAG$X[i] == 1){
    s3px <- append(s3px, s3tabx$Freq[2], after = length(s3px))
  }  else{
    s3px <- append(s3px, s3tabx$Freq[1], after = length(s3px))
  }
}


s3DAG <- cbind(s3DAG, s3px)



#Probabilities of Y

s3taby <- as.data.frame(prop.table(table(s3DAG$Y)))

s3taby$Var1 <- as.integer(levels(s3taby$Var1))

s3py <- c()

for (i in 1:length(s3DAG$Y)){
  for (j in 1:length(s3taby$Var1)){
    if (s3DAG$Y[i] == s3taby$Var1[j]){
      s3py <- append(s3py, s3taby$Freq[j], after = length(s3py))
    }
  }  
}


s3DAG <- cbind(s3DAG, s3py)



#PXYZ

s3PYZX <- data.frame(Y = s3DAG$Y)

g = 0

c = 1

for (l in 0:1){
  for (v in c(14:27)){
    
    c = 1 + c
    
    s3tabyzs <- as.data.frame(prop.table(table(s3DAG$Y[s3DAG$X == l & s3DAG$Z == v])))
    
    s3tabyzs$Var1 <- as.integer(levels(s3tabyzs$Var1))
    
    if (length(s3tabyzs$Var1) != 0){
      
      s3pyzs <- c()
      
      for (k in 1:length(s3DAG$Y)){
        
        for (j in 1:length(s3tabyzs$Var1)){
          
          if (s3DAG$Y[k] == s3tabyzs$Var1[j]){
            
            s3pyzs <- append(s3pyzs, s3tabyzs$Freq[j], after = length(s3pyzs))
            
          } 
          
          else {
            
            g = g +1 
            
          }
          
        }
        
        if (g == length(s3tabyzs$Var1)){
          
          s3pyzs <- append(s3pyzs, 0, after = length(s3pyzs))
          
        }
        
        g=0
        
      }
      
    }
    else {
      s3pyzs <- rep(0, each=2500)
      print(paste(l,v))
    }
    s3PYZX <- cbind(s3PYZX, s3pyzs)  
    
    colnames(s3PYZX)[c] = paste("Y&x(",l,")&z(",v,")", sep = "")
    
    s3pyzs <- c()
    
  }
  
}


s3pyzx <- distinct(s3PYZX, Y, .keep_all = TRUE)

s3pyzx <- s3pyzx[order(s3pyzx$Y),]




s3pyz0 <- s3pyzx[1:15]

s3pyz1 <- s3pyzx[c(1,16:29)]

s3pyz0 <- data.frame(t(s3pyz0[-1]))

s3pyz1 <- data.frame(t(s3pyz1[-1]))




for (i in 1:4){
  colnames(s3pyz0)[i] = paste("y=",i+13, sep = "")
  colnames(s3pyz1)[i] = paste("y=",i+13, sep = "")
}






s3y0 <- 0

s3Y0 <- c()

for (j in 1:4){
  for (i in 1:14){
    s3z0 <- s3pyz0[i,j]*s3tabz$Freq[i]
    s3y0 <- s3z0 + s3y0
  }
  s3Y0 <- append(s3Y0, s3y0, after = length(s3Y0))
  s3y0 <- 0
}




s_t3y0 <- 0

s_t3Y0 <- c()

for (j in 1:4){
  for (i in 1:14){
    s_t3z0 <- t3pyz0[i,j]*s3tabz$Freq[i]
    s_t3y0 <- s_t3z0 + s_t3y0
  }
  s_t3Y0 <- append(s_t3Y0, s_t3y0, after = length(s_t3Y0))
  s_t3y0 <- 0
}






s3y1 <- 0

s3Y1 <- c()

for (j in 1:4){
  for (i in 1:14){
    s3z1 <- s3pyz1[i,j]*s3tabz$Freq[i]
    s3y1 <- s3z1 + s3y1
  }
  s3Y1 <- append(s3Y1, s3y1, after = length(s3Y1))
  s3y1 <- 0
}




s_t3y1 <- 0

s_t3Y1 <- c()

for (j in 1:4){
  for (i in 1:14){
    s_t3z1 <- t3pyz1[i,j]*s3tabz$Freq[i]
    s_t3y1 <- s_t3z1 + s_t3y1
  }
  s_t3Y1 <- append(s_t3Y1, s_t3y1, after = length(s_t3Y1))
  s_t3y1 <- 0
}




results_st <- data.frame(s_t3Y1,s_t3Y0,s3Y1,s3Y0)


write.table(results_st, file = "results_st.csv", sep = ",")






