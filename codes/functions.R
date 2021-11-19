#############Functions used to construct the simulated frequencies and analysis##############
#############################################################################################
###########################bin-function######################################################
############# This function is used to calculate the simulated deviations####################
#############################################################################################
#### This function takes the following arguments#############################################
##### - n is the number of units in a field
##### - p is the share of women in field 
##### - s is the vector of department sizes
bin<-function(n,p,s)
{
  ###one iteration of the loop calculates the deviation for different sized departments for one p.
  ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
  k<-sample(s,n,replace=T)##additional randomization, not really necessary, could be deleted.
  tot_freq<-c()
  rel_freq<-c()
  for(i in 1:length(k))
  {
    tot_freq[i]<-rbinom(1,k[i],p)###for each department, draw from a binomial with size k[i]
    rel_freq[i]<-tot_freq[i]/k[i]###for calculating the relative frequency
  }
  mean_w<-mean(tot_freq)
  mean_w_share<-mean(rel_freq)
  mean_dev<-tot_freq-mean_w
  share_dev<-rel_freq-mean_w_share
  ad<-abs(share_dev)
  mad<-mean(ad)
  MAD<-mad/sd(share_dev)
  return(list(mean_dev=mean_dev,mean_w_share=mean_w_share,mean_dev,share_dev=share_dev, MAD=MAD))
}

bin_ex<-function(n,p,s)
{
  ################################################################################################
  ###one iteration of the loop calculates the deviation for different sized departments for one p.
  ################################################################################################
  k<-s
  tot_freq<-c()
  rel_freq<-c()
  for(i in 1:length(k))
  {
    tot_freq[i]<-rbinom(1,k[i],p)###for each department, draw from a binomial with size k[i]
    rel_freq[i]<-tot_freq[i]/k[i]
  }
  mean_w<-mean(tot_freq)
  mean_w_share<-mean(rel_freq)
  mean_dev<-tot_freq-mean_w
  data<-cbind(rel_freq,s)
  df<-as.data.frame(data)
  df.expanded <- df[rep(row.names(df), df[,2]), 1:2]
  df.expanded<-cbind(df.expanded,mean(df.expanded[,1]))
  ###Expand the share_dev by the corresponding number of total department sizes###
  share_dev<-df.expanded[,1]-df.expanded[,3]
  share_dev<-as.vector(share_dev)
  ad<-abs(share_dev)
  mad<-mean(ad)
  MAD<-mad/sd(share_dev)
  return(list(mean_dev=mean_dev,mean_w_share=mean_w_share,mean_dev,share_dev=share_dev, MAD=MAD))
  # dev.off()
}

simu_fun_new<-function(FE,k,p,reps,value)
{
  s<-length(FE)
  ###for each department, draw from a binomial the share of women###
  bin_new<-function(k,p)
  {
    tot_freq<-c()#matrix(NA, nrow = n, ncol = length(k))
    rel_freq<-c()#matrix(NA, nrow = n, ncol = length(k))
    for(i in 1:length(k))
    {
      tot_freq[i]<-rbinom(1,k[i],p[i])###for each department, draw from a binomial with size k[i]
      rel_freq[i]<-tot_freq[i]/k[i]
    }
    counts<-as.data.frame(table(tot_freq))
    if((length(subset(counts$Freq,counts$tot_freq==value)>0)))
    {
      counts<-subset(counts$Freq,counts$tot_freq==value)
    }else{
      counts=0
    }
    return(counts)
  }
  results<-replicate(reps,bin_new(k,p))
  results<-unlist(results)
  m.res<-mean(results)###this is the expected value of zeros based on the simulated pattern
  counts<-as.data.frame(table(FE))
  counts_res<-as.data.frame(table(results))
  if(length(subset(counts$Freq,counts$FE==value)>0))
  {
    counts1<- subset(counts$Freq,counts$FE==value)
  } else{ counts1<-0}
  return(list(counts1=counts1, m.res= m.res,counts=counts))
}


boot_fun<-function(z,data,means,seed=10,reps=100){
  mefe<-c()
  dev<-c()
  for(j in 1:100)
  {
    data_simulated<-c()
    for(i in 1:length(code))
    {
      df_s<-subset(data,data[,5]==code[i])
      p<-subset(means[,2],means[,1]==code[i])
      p=as.numeric(p)
      s<-dim(df_s)[1]## the number of departments
      k<-df_s$Size 
      # the vector of department sizes
      ###for each department, draw from a binomial the share of women###
      bin_new<-function(k,p)
      {
        tot_freq<-c()#matrix(NA, nrow = n, ncol = length(k))
        rel_freq<-c()#matrix(NA, nrow = n, ncol = length(k))
        for(i in 1:length(k))
        {
          tot_freq[i]<-rbinom(1,k[i],p)###for each department, draw from a binomial with size k[i]
          #rel_freq[i]<-tot_freq/k
        }
        counts<-as.data.frame(table(tot_freq))
        if((length(subset(counts$Freq,counts$tot_freq==z)>0)))
        {
          counts<-subset(counts$Freq,counts$tot_freq==z)
        }else {
          counts=0
        }
        return(list(counts=counts,tot_freq=tot_freq,k=k))
      }
      data_simulated<-c(data_simulated,bin_new(k,p)$tot_freq)
    }
    data_simulated<-data.frame(data_simulated,data$Size-data_simulated,data$Size,data$Share,data$Code)
    names(data_simulated)<-c("FE","M","Size","Share","Code")
    results1<-c()
    counts<-c()
    reps<-100
    for(i in 1:length(code))
    {
      results1[i]<-simu_fun(code[i],data_simulated,reps = reps,z)$m.res
      counts[i]<-simu_fun(code[i],data_simulated,reps = reps,z)$counts1
    }
    diff<-counts-results1##if this difference is negative: then on average, there are too few zero women departments.
    mefe[j]<-sum(diff)###this is then: 0.4 fewer zero women departments than expected.
    print(j)
    print(mefe)
    dev[j]<-sum(diff<0)/length(code)
  }
  write.table(mefe,paste0("uc",z))
  write.table(dev,paste0("dev",z))
}

#####Now do this for all departments, #####
simu_fun<-function(code,data,reps,z)
{
  df_s<-subset(data,data[,5]==code)
  p<-subset(means[,2],means[,1]==code)
  p=as.numeric(p)
  s<-dim(df_s)[1]## the number of departments
  k<-df_s$Size ### the vector of department sizes
  ###for each department, draw from a binomial the share of women###
  bin_new<-function(k,p)
  {
    tot_freq<-c()
    rel_freq<-c()
    for(i in 1:length(k))
    {
      tot_freq[i]<-rbinom(1,k[i],p)
      rel_freq[i]<-tot_freq[i]/k[i]
    }
    counts<-as.data.frame(table(tot_freq))
    if((length(subset(counts$Freq,counts$tot_freq==z)>0)))
    {
      counts<-subset(counts$Freq,counts$tot_freq==z)
    }else {
      counts=0
    }
    return(counts)
  }
  results<-replicate(reps,bin_new(k,p))
  results<-unlist(results)
  m.res<-mean(results)
  counts<-as.data.frame(table(df_s$FE))
  counts_res<-as.data.frame(table(results))
  if((length(subset(counts$Freq,counts$Var1==z)>0)))
  {
    counts1<- subset(counts$Freq,counts$Var1==z)
  } else{ counts1<-0}
  return(list(counts1=counts1, m.res= m.res,counts=counts)) 
}

esti_fisher<-function(f_z,means)####arguments: z from the paper, i.e. f_z=0 # of zero women departments, means: 50x2 tibble: discipline code s, p_s
{
  E<-c()
  E.hat<-c()
  counts1=c()
  z=c()
  #  z_c=c()
  p.val<-c()
  p_val_c<-c()
  w=c()
  dev=c()
  b.test<-c()
  var_s<-c()
  Code=unique(data[,5])
  #####I program this by discipline, however the procedure is as described in the paper.
  for(i in 1:S)
  {
    code=Code[i]
    df_s<-subset(data,data[,5]==code)###subsets the data for one discipline given by "code"
    p<-subset(means[,2],means[,1]==code) ###Calculate the discipline mean (p.hat) 
    p=as.numeric(p)###p_s
    n<-dim(df_s)[1]## the number of departments### 
    k<-df_s$Size ### the vector of department sizes, within one discipline, i.e. n_d for all d in s.
    fe_f<-df_s$FE## the number of women in each department
    p_zds=dbinom(f_z,k,p)##p_d|s(z) from the paper
    E[i]<-sum(dbinom(f_z,k,p))##
    w[i]<-sum((1-p_zds)*p_zds)#(n-1)#*(1/E[i])#E[i]#n#sum(k) 
    # E.hat[i]<-sum(dbinom(f_z,X[,i],p.hat[i]))
    ### Now: calculate the number of zero women departments in the sample: 
    counts<-as.data.frame(table(fe_f)) ###these are the counts from the 
    var_s[i]=sum(( 1- p_zds)* p_zds)
    if((length(subset(counts$Freq,counts$fe_f==f_z)>0)))
    {
      counts1[i]<- subset(counts$Freq,counts$fe_f==f_z)
    } else{ counts1[i]<-0}
    dev[i]<-counts1[i]-E[i]#as.character.factor(counts$Freq)#as.numeric(levels(counts))[f]-E###from the analytical probability
    #print(dev)
    lt=dev[i]<0###left-tailed: when lower.tail is true: it is a left tailed test, i.e. H_0:mu=mu_fz, H_a: mu<mu_fz
    theta_null<-E[i]/n
    theta.hat<-counts1[i]/n
    ltt=ifelse(lt==1,1,-1)
    Y_sdi<-fe_f==f_z
    Z1<-sqrt(n)*(1/n)*sum(Y_sdi- p_zds)
    Z2<-sqrt(((sum((1-p_zds)*p_zds))/n))
    Z_score= Z1/Z2
    z[i]<-Z1/Z2
    ####Possible: continuity correction
    ## Z1<-sqrt(n)*(1/n)*sum(Y_sdi-p_zds-((ltt*(1/2)+(1-ltt)*0.5)))
    ## Z2<-sqrt(((sum((1-p_zds)*p_zds))/n))
    ## Z_cont=Z1/Z2
    ## z_c[i]=Z_cont #(counts1[i]-n*theta_null-ltt*(1/2)/S)/(sqrt(n*theta_null*(1-theta_null)))
  }
  #####Calculate the individual ####
  for(i in 1:S)
  {
    p.val[i]= 2*pnorm((z[i]), lower.tail=FALSE)#for a one-sided testifelse(lt==1,pnorm(z[i]),1-pnorm(z[i]))
  }
  Z1<-sum(z)
  Z2<-sqrt(S)
  Z_s= Z1/Z2
  p_val_Z_s<-2*pnorm((Z_s), lower.tail=FALSE)#for two-sided test ifelse(Z_s<0,pnorm(Z_s),1-pnorm(Z_s))
  p_val_Z_s
  Z_sw<-sum(z*w)/sqrt(sum(w^2))
  p_val_Z_sw<-ifelse( Z_sw<0,pnorm( Z_sw),1-pnorm( Z_sw))
  p_val_Z_sw
  #######################################################################################
  b.test= ifelse(lt==1,binom.test(sum(dev<0),S, 0.5, alternative = "greater")$p.value, binom.test(sum(dev>0),S, 0.5, alternative = "greater")$p.value)
  Z<-cbind(sum(dev),Z_s,Z_sw,p_val_Z_s,p_val_Z_sw,b.test,f_z)
  Z.names<-cbind("sum(dev)","Z_s","Z_sw","p_val_Z_s","p_val_Z_sw","b.test","z")
  write.table(Z,file=paste0("table_tests_",f_z))
  write.table(Z,file="table_tests_all_new",append=T,col.names = ifelse(f_z==100,T,F),row.names=F)
  return(Z)
}
