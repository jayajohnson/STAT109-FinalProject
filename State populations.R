state_pops <- read.csv("rawdata/State populations 2020.csv")[,c(2,3)]

state_pops <- state_pops[-31,] # Remove entry for Puerto Rico

state_pops <- state_pops[order(state_pops$State),]

state_pops <- t(state_pops)

colnames(state_pops) <- c(
  "AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN",
  "IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
  "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
  "VT","VA","WA","WV","WI","WY"
)

state_pops <- state_pops[,order(colnames(state_pops))]

write.csv(state_pops, "output/State populations 2020.csv")



