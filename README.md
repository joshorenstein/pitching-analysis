Generalized Additive Model to model MLB pitcher stuff <br/>
1) Download and clean Statcast data (h/t to Ethan Moore and Bill Petti.) <br/>
2) Feature selection primarily using VIF testing on predictors <br/>
3) Fastball/Sinker models includes pitch velo, release point, spin rate, spin direction, break and plate location  <br/>
4) Breaking ball and offspeed models includes pitch velo, release point, spin rate, spin direction, break, plate location, and relative horizontal break and velo compared to fastball <br/>
5) Notes: This model uses 2020 MLB season as training data. '21 season data will be used as test set and GAM will be tuned once I've got some test data. Model will also be updated to include command and batted ball data to create a more complete profile. <br/>

[Mets Pitching Staff](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mets-1.pdf) <br/>
[Pirates Pitching Staff](http://github.com/joshorenstein/pitching-analysis/blob/main/results/pirates.pdf) <br/>
[Red Sox Pitching Staff](http://github.com/joshorenstein/pitching-analysis/blob/main/results/red-sox.pdf) <br/>
[Mariners Pitching Staff](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mariners.pdf) <br/>
