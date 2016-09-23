#df<-read.table("/home/kashish/Downloads/t20_csv/298795.csv", header = FALSE, sep = ",", col.names = paste0("V",seq_len(11)), fill = TRUE)


setwd("/home/kashish/Downloads/t20_csv")
file_list<-list.files()
average <- array(dim=c(1,10))
#no_of_innings <- array(dim=c(1,10))
no_of_balls <- array(dim=c(1,10))
no_of_balls <- c(0,0,0,0,0,0,0,0,0,0)
#no_of_innings <- c(0,0,0,0,0,0,0,0,0,0)
print(no_of_balls)
for (file in file_list){
  
  #if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=FALSE, sep=",",quote='',col.names = paste0("V",seq_len(11)), fill=TRUE)
  }
  else
  {
    # if the merged dataset does exist, append to it
    #if (exists("dataset")){
    temp_dataset <-read.table(file, header=FALSE,  sep=",",quote='',col.names = paste0("V",seq_len(11)), fill = TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}

ndataset <- subset(dataset, (dataset[,2]==1 | dataset[,2]==2))
temp1 =0
sum1 = 0
count1 = 0


for(i in 1:nrow(ndataset))
{
  if(ndataset[i,1]=="version")
  {
    ndataset[i,8] = 0
    ndataset[i,9] = 0
  }
}
balls=0
match = 0
average = c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:nrow(ndataset))
{
  if(ndataset[i,1]=="version")
  {
    #no_of_innings[1] = no_of_innings[1] + 1
    temp1 = 0
    balls=0
    match= match +1
    count1 = 0
  }
  
  if(ndataset[i,2]==1)
  {
    if(temp1 == 0)
    {
      average[count1+1]=average[count1+1]+ndataset[i,8]
      average[count1+1]=average[count1+1]+ndataset[i,9]
      
      if(ndataset[i,9]==0 && ndataset[i,1]!="version")
      {
        balls= balls +1
      }
      
    }
    
    if(ndataset[i,10] == 'caught' | ndataset[i,10] == 'bowled' | ndataset[i,10] == 'lbw' | ndataset[i,10] == 'run out')
    {
      no_of_balls[count1+1]=no_of_balls[count1+1]+balls
      balls = 0
      count1= count1+1
    }
  }
  
}


for(i in 1:nrow(ndataset))
{
  if(ndataset[i,1]=="version")
  {
    #no_of_innings[1] = no_of_innings[1] + 1
    temp1 = 0
    balls = 0
    count1 = 0
  }

  if(ndataset[i,2]==2)
  {
    if(temp1 == 0)
    {
      #print(ndataset[i,8])
      average[count1+1]=average[count1+1]+ndataset[i,8]
      average[count1+1]=average[count1+1]+ndataset[i,9]
      if(ndataset[i,9]==0 && ndataset[i,1]!="version")
      {
        balls= balls +1
      }
      
      #print(ndataset[i,10],i)
      #sum1 = sum1+ndataset[i,8]
    }

    if(ndataset[i,10] == 'caught' | ndataset[i,10] == 'bowled' | ndataset[i,10] == 'lbw' | ndataset[i,10] == 'run out')
    {
      no_of_balls[count1+1] = no_of_balls[count1+1] + balls
      #print(no_of_innings[count1+2],count1)
      balls = 0
      count1= count1+1
    }
  }

}
# temp2 = 0
# count2 = 0
# sum2 = 0
# for(i in 1:nrow(ndataset))
# {
#   if(ndataset[i,2]==2)
#   {
#     if(temp2 == 0)
#     {
#       sum2 = sum2+ndataset[i,8];
#     }
#     if(ndataset[i,10] != "")
#     {
#       temp2 = 1
#     }
#     
#   }
#   if(ndataset[i,1]=="version")
#   {
#     temp2 = 0
#     count2= count2+1
#   }
# }

Avg = average/no_of_balls
#Avg = (sum1+sum2)/(count1+count2)