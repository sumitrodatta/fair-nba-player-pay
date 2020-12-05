library(tidyverse)

salaries=read_csv("Player Salaries.csv")
advanced=read_csv("Advanced.csv")
cap_hist=read_csv("Salary Cap History.csv")
combined=left_join(salaries,advanced) %>% left_join(.,cap_hist) %>% 
  select(player:experience,g,mp,vorp,cap) %>% filter(!is.na(seas_id)) %>% 
  mutate(wins_added=vorp*2.7, predicted_sal=wins_added/41*cap, 
         act_minus_predict=salary-predicted_sal,diff_as_percent_of_cap=act_minus_predict/cap) %>% 
  select(seas_id,player_id,player,tm,season,hof:vorp,wins_added,salary,cap,
         predicted_sal,act_minus_predict,diff_as_percent_of_cap) %>% arrange(player,desc(season))

by_player=combined %>% group_by(player_id,player) %>% 
  summarize(tot_sal=sum(salary),tot_vorp=sum(vorp),tot_wins_added=sum(wins_added),
         tot_predict=sum(predicted_sal),tot_diff=sum(act_minus_predict),tot_percent=sum(diff_as_percent_of_cap))

write_csv(combined,"Player Cap Records.csv")
write_csv(by_player,"Total Pay by Player.csv")
