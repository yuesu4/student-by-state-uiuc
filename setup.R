library(tidyverse)

url = "https://raw.githubusercontent.com/wadefagen/datasets/master/students-by-state/uiuc-students-by-state.csv"
portion_in_state = read_csv(url)

Status_order = c(
  "Undergrad",
  "Grad",
  "Professional"
)

portion_in_state = portion_in_state|>
  select(-Total)

write_csv(x = portion_in_state,file = "data/portion_in_state.csv")

portion_in_state|>
  filter(State == "Illinois")|>
  filter(Year == 2002)|>
  pivot_longer(Undergrad:Grad, names_to = "Status", values_to = "Count")|>
  group_by(Status)|>
  summarise(Count = sum(Count))|>
  mutate(Status = factor(Status, levels = Status_order))|>
  ggplot() + 
  aes(x = Status, y = Count, fill = Status)|>
  geom_bar(stat = "identity") + theme_bw()
   
