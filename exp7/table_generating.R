# Generating LaTeX tables

setwd("Directory of your RData files generated using exp7_VAE.R")

df <- data.frame(Your_Rdata, row.names = 
                   paste0(rep("$X_{", 20), seq(1, 20), rep("}$", 20)))
df

#flag[seq(1,41,2),] <- round(flag[seq(1,41,2), ], 0)

namecol <- rep(0, 42)
for (i in 0:20){
  namecol[2*i+1] <- paste0("PC", i)
  namecol[2*i+2] <- paste0("CV", i)
}

dim(df)
colnames(df) <- namecol 

require(xtable)
mat <- xtable(df, digits=c(0, rep(c(0, 2), 21)))
print(mat, sanitize.text.function = function(x){x})

