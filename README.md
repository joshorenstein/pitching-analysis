Generalized Additive Model to rate MLB pitcher stuff and translate it to predicted FIP based on stuff. <br/>
1) Download and clean Statcast data (h/t to Ethan Moore and Bill Petti.) <br/>
2) Feature selection primarily using VIF testing on predictors <br/>
3) Fastball/Sinker models includes pitch velo, release point, batter side, spin rate, spin direction, break and plate location  <br/>
4) Breaking ball models includes pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo compared to fastball <br/>
5) Changeup models includes pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo, vert break and spin direction compared to fastball <br/>
5) Notes: This model uses 2020 MLB season as training data. '21 season data will be used as test set and GAM will be tuned once I've got some test data. Model was recently updated to include command and batted ball data to create a more complete profile. <br/>

MLB Leaderboard <br/>
[Leaderboard](https://github.com/joshorenstein/pitching-analysis/blob/main/results/leaderboard.pdf) <br/>
Pitching Stats for MLB Teams  <br/>
[Blue Jays](http://github.com/joshorenstein/pitching-analysis/blob/main/results/blue-jays.pdf) <br/>
[Giants](http://github.com/joshorenstein/pitching-analysis/blob/main/results/giants.pdf) <br/>
[Mariners](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mariners.pdf) <br/>
[Pirates](http://github.com/joshorenstein/pitching-analysis/blob/main/results/pirates.pdf) <br/>

