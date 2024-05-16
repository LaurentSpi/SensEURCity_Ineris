rm(list = ls())

# Set directory
setwd("C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/")

# Import libraries
library(raster)
library(maptools)
library(RColorBrewer)
library(fields)
library(rgdal)
library(ggplot2)
library(dplyr)
library(data.table)
library(chron)
library(optimization)
library(pracma)
# library(foreach)
# library(doParallel)
# registerDoParallel(cores=16)

#################################################################################
#                               OUTLIERS DETECTION                              #
#                      Created 30/08/2021 & updated 16/05/2022                  #
#            Author: Alicia Gressent (INERIS) alicia.gressent@ineris.fr         #
#################################################################################

#####################################
#           INITIALIZATION          #          
#####################################

# Directory paths
indir   <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/INPUTS/"# path for input directory
outdir  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/"# path for output directory
outdir2  <-"C:/Users/gressent/Desktop/Encadrement/STAGES/Stage_SENSEURCITY/SENSEURCITY_CAL/OUTPUTS/figs/"# path for output directory

# Init variables
loc <-"Netherlands" # estimation location
pol <-"PM25" # pollutant

#####################################
#            READ DATA              #          
#####################################
print("READ REF AND SENSOR DATA")

# Load .Rda
load(paste0(outdir,"LCS_df_all_clean_groups.Rda"))

#####################################
#        OUTLIERS DETECTION         #          
#####################################]

# Init var
nbr_groups=length(unique(sensdataff$Group))
group_name=unique(sensdataff$Group)
dataout <- c()

#dataout  <- foreach(i = 1:nbr_groups, .combine='rbind') %dopar% { # loop in parallel mode

for (i in 1:nbr_groups){ # Loop over groups

    Group=group_name[i]
    df <- sensdataff[which(sensdataff$Group==Group),]
    df_group=paste0(unique(df$Typology)," ",unique(df$Season)," ",unique(df$Clust))
    #print(df_group)

    ## Plot distribution
    # png(filename=paste0(dirout2,"Distribution_PM25_",Group,".png"), width=600, height=600, type="cairo",bg = "white")
    # p1 <- ggplot(df, aes(x=PM2.5)) + geom_histogram(color="#333333",fill="#333333",alpha=0.5) +
    # xlim(c(0, 200))+#ylim(c(0, 3e6))+
    # labs(title="",x=bquote(.(pol) ~ (mu*g/m^3)), y = "Frequency")+
    # theme_bw()+
    # theme_minimal()+
    # theme(plot.title = element_text(size=24),
    #         axis.text=element_text(size=24),
    #         axis.title=element_text(size=24),
    #         legend.text = element_text(size =24),
    #         legend.title = element_blank(),
    #         legend.spacing.x = unit(0.3, 'cm'),
    #         legend.position= "top")
    # p1
    # dev.off()

    #####################################
    #1 Square root transormation
    PM25 <- df$PM2.5
    xc <- sqrt(PM25 + (1-min(PM25)))
    if (length(xc[!duplicated(xc)]) > 1 & length(xc) > 2) {
    df1=df
    df1$PM2.5=xc

    ## Plot distribution
     # png(filename=paste0(dirout2,"Distribution_PM25_",Group,"_sqrt_transformed.png"), width=600, height=600, type="cairo",bg = "white")
     # p2 <- ggplot(df1, aes(x=PM2.5)) + geom_histogram(color="#333333",fill="#333333",alpha=0.5) +
     # #xlim(c(0, 15))+#ylim(c(0, 3e6))+
     # labs(title="",x=bquote(.(pol) ~ (mu*g/m^3)), y = "Frequency")+
     # theme_bw()+
     # theme_minimal()+
     # theme(plot.title = element_text(size=24),
     #         axis.text=element_text(size=24),
     #         axis.title=element_text(size=24),
     #         legend.text = element_text(size =24),
     #         legend.title = element_blank(),
     #         legend.spacing.x = unit(0.3, 'cm'),
     #         legend.position= "top")
     # p2
     # dev.off()

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

    # png(filename=paste0(dirout2,"Density_PM25_",Group,"_sqrt_transformed.png"), width=600, height=600, type="cairo",bg = "white")
    # p3 <- ggplot(df1, aes(x=PM2.5)) + geom_density(color="#333333",fill="#333333",alpha=0.5) +
    # #xlim(c(0, 500))+#ylim(c(0, 3e6))+
    # labs(title="",x=bquote(.(pol) ~ (mu*g/m^3)), y = "Frequency")+
    # theme_bw()+
    # theme_minimal()+
    # theme(plot.title = element_text(size=24),
    #         axis.text=element_text(size=24),
    #         axis.title=element_text(size=24),
    #         legend.text = element_text(size =24),
    #         legend.title = element_blank(),
    #         legend.spacing.x = unit(0.3, 'cm'),
    #         legend.position= "top")
    # p3
    # dev.off()

    #####################################
    #3 Optimization of the log likelihood function 

    # Init var
    mkj_all=as.vector(mkj_all) # meanof the truncated normal distribution
    skj_all=as.vector(skj_all) # standard deviation of the truncated normal distribution
    tk=c(); nk=c() # sandard deviation and mean of the underlying normal distribution to be caluclated by the optimization
    z=3 # confidence level
    outliers = rep("Accepted value",length(mkj_all))

#    # Likelihood function
#    L <- function(x){
#         term1 = (1/(x[1]*sqrt(2*pi))) * (exp((-1/2)*((xc-x[2])/x[1])**2))
#         term2 = (1/2) * (1 + erf((1-x[2])/(x[1]*sqrt(2))))
#         y <- sum(log(term1)-(log(x[1]*(1-term2))))
#    }
#
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
          mkj_all_lower=mkj_all[kk]*0; mkj_all_upper=mkj_all[kk]*2

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
    png(filename=paste0(outdir2,"04_Outliers_",Group,"_v2.png"), width=1000, height=600, type="cairo",bg = "white")
    p4 <- ggplot(df, aes(x=datetime,y=PM25)) + geom_point(aes(color=outliers)) +
    #xlim(c(0, 15))+#ylim(c(0, 3e6))+
    scale_color_manual(values=c("black", "red"))+
    labs(title=paste0(Group,": ",df_group),x="",y=bquote(.(pol) ~ (mu*g/m^3)))+
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

save(dataout,file=paste0(outdir,"LCS_df_all_data_clean_groups_outliers_v2.Rda"))

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

