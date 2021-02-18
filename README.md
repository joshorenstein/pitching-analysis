### Expected FIP: <br/>
This is a Generalized Additive Model (GAM) to rate MLB pitcher stuff and translate it to predicted FIP based on stuff. <br/>
It's a GAM model grouped by pitch type and batter side that regresses HR rates and swinging strike rates based on the expected outcomes given the models below. Expected swing and miss rate is then fit to strikeout percentage. Walk rates are given from actual game data and then Expected FIP is fit from expected swinging strike rates, expected home run rates and actual walk rates. This model uses 2020 MLB season as training data. '21 season data will be used as test set and GAM will be tuned once I've got some test data.

## The scripts:
* Download and clean Statcast data (h/t to Ethan Moore and Bill Petti. This is mostly their code.) <br/>
* Feature selection primarily using VIF testing on predictors <br/>
* Fastball/Sinker models includes pitch velo, release point, batter side, spin rate, spin direction, break and plate location  <br/>
* Breaking ball models includes pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo compared to fastball <br/>
* Changeup models includes pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo, vert break and spin direction compared to fastball <br/>
*  Home run models are currently based on the above factors but with home run as the dependent variable instead of swinging strikes. This model will eventually be rebuilt with different variables.
* There is some calculation of FIP and some summary analysis.

## The fun stuff: <br/>
[MLB Leaders](https://github.com/joshorenstein/pitching-analysis/blob/main/results/leaderboard.pdf) <br/>
------ <br/>
[Blue Jays](http://github.com/joshorenstein/pitching-analysis/blob/main/results/blue-jays.pdf) <br/>
[Giants](http://github.com/joshorenstein/pitching-analysis/blob/main/results/giants.pdf) <br/>
[Mariners](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mariners.pdf) <br/>
[Pirates](http://github.com/joshorenstein/pitching-analysis/blob/main/results/pirates.pdf) <br/>

