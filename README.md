## Expected FIP <br/>

###### The model: <br/>
This is a Generalized Additive Model (GAM) to rate MLB pitcher stuff and translate it to predicted FIP based on stuff. <br/>
It's a series of models grouped by pitch type and batter side that regresses HR rates and swinging strike rates based on the expected outcomes given the models below. Expected swing and miss rate is then fit to strikeout percentage. Walk rates are given from actual game data and then Expected FIP is fit from expected swinging strike rates, expected home run rates and actual walk rates. This model uses 2020 MLB season as training data. '21 season data will be used as test set and GAM will be tuned once I've got some test data.

###### The point: <br/>
Determine how good a pitcher's stuff is and how it should translate to performance. See who has the best pitches in baseball. See which pitchers are optimizing their arsenal. Is a pitcher throwing his best pitches most often?

###### The fun stuff: <br/>
[MLB Leaders](https://github.com/joshorenstein/pitching-analysis/blob/main/results/leaderboard.pdf) <br/>
[Blue Jays](http://github.com/joshorenstein/pitching-analysis/blob/main/results/blue-jays.pdf) <br/>
[Giants](http://github.com/joshorenstein/pitching-analysis/blob/main/results/giants.pdf) <br/>
[Mariners](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mariners.pdf) <br/>
[Pirates](http://github.com/joshorenstein/pitching-analysis/blob/main/results/pirates.pdf) <br/>

###### The scripts: <br/>
* Download and clean Statcast data (h/t to Ethan Moore and Bill Petti. This is mostly their code.) <br/>
* Feature selection primarily using VIF testing on predictors <br/>
* Fastball/Sinker models include pitch velo, release point, batter side, spin rate, spin direction, break and plate location  <br/>
* Breaking ball models include pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo compared to fastball <br/>
* Changeup models include pitch velo, release point, batter side, spin rate, spin direction, break, plate location, and relative horizontal break and velo, vert break and spin direction compared to fastball <br/>
*  Home run models are currently based on the above factors but with home run as the dependent variable instead of swinging strikes. This model will eventually be rebuilt with different variables.
* There is some calculation of FIP and some summary analysis.

###### Code Example - Fastball Whiff Rate Model <br/>
```
train_select %>%
  filter(pitch_type %in% c("FF","SI")) %>% 
  group_by(pitch_type,p_throws,stand) %>%
  do(fit = gam(whiff ~ release_speed+release_pos_x+release_pos_z+
                 release_spin_rate+release_spin_direction+pfx_x+pfx_z
                 +plate_x+plate_z, data = .,family=binomial,method="REML",bs="re"))
```
