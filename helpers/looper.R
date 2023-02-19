

volume_of_cone <- function(radius, height) {
  return(pi*radius^2*(height/3))
}

for(i in seq(8,20, by=.1)){
  print(i)
  for(j in seq(1,20, by = .1)){
    for(k in seq(1,20, by = .1)) {
full_height <- i
fill_height <- full_height - 8
bottom_radius <- j
top_radius <- k

volume_of_partial_cone <- function(radius, height, top_radius, top_height)
{
  full_cone <- volume_of_cone(radius, height)
  partial_cone <- full_cone - volume_of_cone(top_radius, top_height)
  return(partial_cone)
}

a = round(volume_of_partial_cone(bottom_radius, full_height, top_radius, fill_height),3)


volume_of_partial_upside_down_cone <- function(radius, height, top_radius, top_height)
{
  #full_cone <- volume_of_cone(radius, height)
  partial_cone <- volume_of_cone(top_radius, top_height)
  return(partial_cone)
} 

b = round((volume_of_partial_upside_down_cone(bottom_radius, full_height, top_radius, fill_height)),3)

if (a==b) 
{
  print('they match')
  print(i)
  print(j)
  print(k)
  
}
  

}}

}