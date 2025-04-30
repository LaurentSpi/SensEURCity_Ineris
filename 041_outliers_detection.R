rm(list = ls())

# Charger le fichier de configuration global
setwd("C:/Users/diallo/OneDrive - INERIS/Documents/Ineris1/ALT_SensEURCity")
source("00_paths_and_setting.R")

# Import libraries
library(raster)
library(sf)
library(RColorBrewer)
library(fields)
library(ggplot2)
library(dplyr)
library(data.table)
library(chron)
library(Rcpp)
library(optimization)
library(pracma)


#################################################################################
#                               OUTLIERS DETECTION                              #
#                      Created 30/08/2021 & updated 16/05/2022                  #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(file_LCS_df_all_clean_groups_Rda)

#####################################
#        OUTLIERS DETECTION         #          
#####################################

# Init var
nbr_groups=length(unique(LCS_df_all_clean_groups$Group))
group_name=unique(LCS_df_all_clean_groups$Group)
dataout <- c()

#dataout  <- foreach(i = 1:nbr_groups, .combine='rbind') %dopar% { # loop in parallel mode

for (i in 1:nbr_groups){ # Loop over groups
  print(i)
  Group=group_name[i]
  df <- LCS_df_all_clean_groups[which(LCS_df_all_clean_groups$Group==Group),]
  df_group=paste0(unique(df$Typology)," ",unique(df$Season)," ",unique(df$Clust))
  print(df_group)
  
  #####################################
  #1 Square root transormation
  PM25 <- df$PM2.5
  xc <- sqrt(PM25 + (1-min(PM25)))
  if (length(xc[!duplicated(xc)]) > 1 & length(xc) > 2) {
    df1=df
    df1$PM2.5=xc
    
    
    
    #####################################
    #2 Mean and standard deviation calculation removing the ith observation
    mkj_all=c()
    skj_all=c()
    for (j in 1:length(xc)){
      mkj=mean(xc[-j])
      skj=sd(xc[-j])
      mkj_all=rbind(mkj_all,mkj)
      skj_all=rbind(skj_all,skj)
    }
    
    #####################################
    #3 Optimization of the log likelihood function 
    
    # Init var
    mkj_all=as.vector(mkj_all) # meanof the truncated normal distribution
    skj_all=as.vector(skj_all) # standard deviation of the truncated normal distribution
    tk=c(); nk=c() # sandard deviation and mean of the underlying normal distribution to be caluclated by the optimization
    z=3 # confidence level
    outliers = rep("Accepted value",length(mkj_all))
    
    
    # Estimate interval for each observation
    for (kk in 1:length(mkj_all)){
      
      # Likelihood function
      L <- function(x){
        term1 = (1/(x[1]*sqrt(2*pi))) * (exp((-1/2)*((xc[kk]-x[2])/x[1])**2))
        term2 = (1/2) * (1 + erf((1-x[2])/(x[1]*sqrt(2))))
        y <- sum(log(term1)-(log(x[1]*(1-term2))))
      }
      
      # Define parameters	
      x=c(skj_all[kk],mkj_all[kk])
      
      # Init parameter bounds
      skj_all_lower=skj_all[kk]*1; skj_all_upper=skj_all[kk]*50
      mkj_all_lower=mkj_all[kk]*0; mkj_all_upper=mkj_all[kk]*1
      
      # Optimization with Nelder-Mead
      #L_nm = optim_nm(L, start=c(tkj_all,nkj_all), trace=TRUE)#,exit=10000) 
      #L_nm = optim_nm(L, k=2, trace=TRUE)#,exit=10000)
      
      # Optimization with Simulated Annealing
      L_sa = optim_sa(L, start=c(x[1],x[2]),lower=c(skj_all_lower,mkj_all_lower),upper=c(skj_all_upper,mkj_all_upper),trace=TRUE) 
      
      # Calculate interval
      tk[kk]=L_sa$par[1]
      nk[kk]=L_sa$par[2]
      upper = nk[kk]+z*tk[kk]
      lower = nk[kk]-z*tk[kk]
      if (lower<0){ lower=0 }
      
      # Define outliers
      if (xc[kk]<lower | xc[kk]>upper){ outliers[kk]="outlier"}#; print(paste0("outlier in", kk, "=> for ", xc, " upper=",upper," and lower=",lower))}
      
      #print(xc[kk])
      #print(upper)
      #print(lower)
      #print(outliers[kk])
      
    }
    
    #TMP
    #x=c(skj_all,mkj_all)
    #skj_all_lower=skj_all*1; skj_all_upper=skj_all*5
    #mkj_all_lower=mkj_all*0; mkj_all_upper=mkj_all*2
    #L_sa = optim_sa(L, start=c(x[,1],x[,2]),lower=c(skj_all_lower,mkj_all_lower),upper=c(skj_all_upper,mkj_all_upper),trace=TRUE)
    
    #upper = mkj_all+z*skj_all
    #lower = mkj_all-z*skj_all
    
    #idx_out=which(xc<lower | xc >upper)
    #outliers <- rep("Accepted value",length(df[,1]))
    #df$outliers=outliers
    #df$outliers[idx_out]="Outlier"
    
    #5 Backtransformation
    PM25c=(xc**2)-(min(xc))    
    
    #6 Assign outliers flag
    df$outliers=outliers
    
    ID_out=unique(df[which(df$outliers=="Outlier"),1])
    df_out=df[which(df$ID %in% ID_out),]
    df$outliers=as.factor(df$outliers)
    
    # Plot	
    png(filename=file.path(path_figures_outliers, paste0("_04_Outliers_",Group,".png")), width=1000, height=600, type="cairo",bg = "white")
    p4 <- ggplot(df, aes(x=datetime,y=PM25)) + geom_point(aes(color=outliers)) +
      #xlim(c(0, 15))+#ylim(c(0, 3e6))+
      scale_color_manual(values=c("black", "red"))+
      labs(title=paste0(Group,": ",df_group),x="",y=bquote(.(pollutant_name) ~ (mu*g/m^3)))+
      theme_bw()+
      theme_minimal()+
      theme(plot.title = element_text(size=24),
            axis.text=element_text(size=24),
            axis.title=element_text(size=24),
            legend.text = element_text(size =24),
            legend.title = element_blank(),
            legend.spacing.x = unit(0.3, 'cm'),
            legend.position= "right")
    print(p4)
    dev.off()
    
    #7 Concatenate df
    if (nbr_groups == 1){
      
      dataout <- df
    }else{
      dataout <- rbind(dataout,df)
    }
    
    #df
    
  }
  
}

#####################################
#            SAVE DATA              #          
#####################################
LCS_df_all_clean_groups_outliers <- dataout
save(LCS_df_all_clean_groups_outliers, file = file_LCS_df_all_clean_groups_outliers_Rda)

#####################################
#            CHECK PLOT             #          
#####################################

#Sens1=dataout[which(dataout$ID=="LTD_55069"),]
#SensName=Sens1$ID
#
#png(filename=paste0(dirout2,"Outliers_",SensName,".png"), width=1000, height=600, type="cairo",bg = "white")
#p5 <- ggplot(Sens1, aes(x=datetime,y=PM2.5)) + geom_point(aes(color=outliers)) +
##xlim(c(0, 15))+#ylim(c(0, 3e6))+
#scale_color_manual(values=c("black", "red"))+
#labs(title="",x=bquote(.(pol) ~ (mu*g/m^3)), y = "Frequency")+
#theme_bw()+
#theme_minimal()+
#theme(plot.title = element_text(size=24),
#        axis.text=element_text(size=24),
#        axis.title=element_text(size=24),
#        legend.text = element_text(size =24),
#        legend.title = element_blank(),
#        legend.spacing.x = unit(0.3, 'cm'),
#        legend.position= "right")
#print(p5)
#dev.off()



# ggplot(df, aes(x=datetime,y=PM25)) + geom_point(aes(color=outliers)) +
#   #xlim(c(0, 15))+#ylim(c(0, 3e6))+
#   scale_color_manual(values=c("black", "red"))+
#   labs(title=paste0(Group,": ",df_group),x="",y=bquote(.(pol) ~ (mu*g/m^3)))+
#   theme_bw()+
#   theme_minimal()+
#   theme(plot.title = element_text(size=24),
#         axis.text=element_text(size=24),
#         axis.title=element_text(size=24),
#         legend.text = element_text(size =24),
#         legend.title = element_blank(),
#         legend.spacing.x = unit(0.3, 'cm'),
#         legend.position= "right")
# print(p4)
# dev.off()