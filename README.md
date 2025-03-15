# March Capness  

THIS README IS ACTIVELY UNDER CONSTRUCTION.  IT WILL BE COMPLETE BEFORE TOURNAMENT DEPLOYMENT.   

This application provides the scoring and UI display for the annual March Madness Salary Cap Tournament.

## Contest Format
You have a $100 salary cap to buy a group of teams from the pool that are playing in the NCAA tournament. The cost of each team is determined by its seed, as shown below:
Seed

| Seed      |  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  | 10  | 11  | 12  | 13  | 14  | 15  | 16  |
| --------- |:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Cost ($)  | 25  | 19  | 13  | 12  | 11  | 10  |  8  |  5  |  5  |  4  |  4  |  3  |  2  |  2  |  1  |  1  |


 

Example: You can buy all 4 #1 seeds for $25 each and root for the chalk. Or you can buy a #1 seed ($25), a 2-seed ($19), a 3-seed ($13), a 4-seed ($12), a 5-seed ($11), then fill out your roster with all 4 8-seeds ($20). There are no restrictions on how you manage your cap money -- as long as you’re at or below the $100 cap, anything goes.
Your score is determined by the total games won by the teams in your group for the main tournament of 63 games (in other words, we are ignoring the play-in games (the NCAA calls them “First Four”) that are happening on Tuesday and Wednesday – they will not count towards your final win total, and you can wait until after these games happen before you pick your group of teams). At the end of the tournament, we’ll total up wins. A total of around 22 wins usually takes it (the all-time record is 24 wins, done in 2014 and 2018).


## The Application
The first version of this application was live for the 2019 March Madness tournament.  The application was developed using R shiny.

### Application Requirements
The application requires a variety of package installs that can be found at the top of global.R  Also, the application requires a series of data files.

Data Files:
raw_selections.csv
all_teams.csv
raw_selections_real.csv ?

### Deployment
The application is currently deployed on Digital Ocean using their container registry and application service.  Full CI/CD pipeline is also implement, as any commit to the main branch will trigger an automatic re-deploy of the application.  


## Running the app yourself.
This is a completely self contained application.  

## Annual Setup
1. Get file from Paul
2. Clean up file from Paul (remove first four team losers and any weird characters)
3. Run build master rank values with updated dates for API
4. Run build master teams
5. Run clean_up_entry_file
6. Update the dates in the API calls standings_function.R


## Annual Setup Updates
1. Run build_march_madness_teams_db_table.R.  This will pull all teams from ESPN and create a list to be used by the app.  You will have to update the API call for appropriate date.
2. For entries, make sure the entry page is active on the UI.  Turn off other components.  Truncate both submission tables.  Turn on the IP check in server.
