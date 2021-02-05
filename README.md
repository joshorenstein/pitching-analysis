# Generalized Additive Model 
1) Uses pitch tracking data that predicts expected swinging strike rate based on pitch tracking data. <br/>
2) Fastball/Sinker models include these variables: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+plate_x+plate_z <br/>
3) Cutter/Curve/Slider models includes these variables: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z <br/>
4) Changeup/Splitter models inclues these variables: release_speed+release_pos_x+release_pos_z+release_spin_rate+release_spin_direction+pfx_x+pfx_z+hmov_diff+velo_diff+plate_x+plate_z <br/>
5) This model will be updated to include command and batted ball data to create a more complete profile

[Mets Pitching Staff](http://github.com/joshorenstein/pitching-analysis/blob/main/results/mets-1.pdf)
