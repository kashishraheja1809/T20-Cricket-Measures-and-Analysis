# Scorecard Link: http://www.espncricinfo.com/twenty20wc/engine/match/287874.html


mydata = read.csv("/home/kashish/Downloads/T20Matches/287874_req12.csv",row.names = NULL, header = F,skip = 21,colClasses = c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

sum1 = 0
for( i in 1:nrow(mydata))
{
  if(mydata[i,1]==1)
  {
    sum1 = sum1 + mydata[i,7]
    sum1 = sum1 + mydata[i,8]
  }
}
target = sum1+1
wickets = array()
j = 1
runs_required = array()
inc = 1
rate = c(1.302922, 1.311687, 1.289697, 1.372977, 1.451710, 1.515652, 1.563730, 1.712957, 1.479592, 1.411862)
wk_fell = 0
pres <- array()
sum2 = 0
k=1

wicket_part <- array(dim = c(1,10))

wicket_part[1:10] = c(25.500698, 26.689417, 25.145484, 21.861925, 19.394195, 14.127869, 10.743169,  9.443223,  6.109551, 4.23423)
x = sum(wicket_part[wk_fell:10])
no_of_balls = 120
for (i in 1:nrow(mydata))
{
  if(mydata[i,1]==2)
  {
    sum2 = sum2 + mydata[i,7]
    sum2 = sum2 + mydata[i,8]
    runs_required[inc] = target - sum2
    inc = inc + 1
    avg_rate = rate[wk_fell+1]
    
    pres[k]= (((((target-sum2)/no_of_balls)/avg_rate) + ((target - sum2)/(sum(wicket_part[(wk_fell+1):10]))))/2)*100
    
    k = k+1 
    if(mydata[i,8]==0)
    {
      no_of_balls= no_of_balls - 1
    }
    
    if(mydata[i,9]!='')
    {
      wk_fell = wk_fell + 1
      wickets[j] = 120 - no_of_balls
      j = j+1
    }
    
  }
  
}
balls = 1:length(pres)
#plot(balls,pres, col = ifelse(balls = wickets, "red"), pch = ifelse(balls = wickets, 19, 1), cex = ifelse(balls = wickets, 2, 1))
par(mar = c(5,5,2,5))
plot(balls, pres, xlab = "", ylab="", axes = FALSE)
lines(balls,pres)
axis(2, ylim = c(0, max(pres)), las=1)
mtext("Pressure", side = 2, line = 2.5)
box()
par(new = T)
plot(balls, runs_required,xlab = "", ylab ="", axes = FALSE, col="red")
lines(balls, runs_required,col = "red")
axis(side = 4,las = 1, col.axis = "red")
mtext(side = 4, line = 2.5, 'Runs Required', col = "red")
axis(1, xlim = 1:length(pres))
mtext("Balls", side = 1, line = 2.5)