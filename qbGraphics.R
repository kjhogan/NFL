final_first_two_v2 %>% 
  ggplot(mapping = aes(x = coll_int_pct, y = int_pct)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x="Coll Int%",
       y="NFL Int%",
       title="Int%: NFL vs. NCAA",
       subtitle="Comparing Int% by QBs drafted in the first two rounds",
       caption="Data from Pro Football Ref") +
  theme_ipsum_rc() -> int_pct_plot

final_first_two_v2 %>% 
  ggplot(mapping = aes(x = coll_int_pct, y = int_pct, label = Player)) +
  geom_label() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x="Coll Int%",
       y="NFL Int%",
       title="Int%: NFL vs. NCAA",
       subtitle="Comparing Int% by QBs drafted in the first two rounds",
       caption="Data from Pro Football Ref") +
  theme_ipsum_rc() -> int_pct_label_plot



final_first_two_v2 %>% 
  ggplot(mapping = aes(x = Player, y = int_pct)) +
  geom_label() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x="Coll Int%",
       y="NFL Int%",
       title="Int%: NFL vs. NCAA",
       subtitle="Comparing Int% by QBs drafted in the first two rounds",
       caption="Data from Pro Football Ref") +
  theme_ipsum_rc() -> int_pct_label_plot
  
  
  final_first_two_v2 %>% arrange(desc(int_pct)) %>% head(10) %>%
    ggplot(mapping = aes(x = reorder(Player,(int_pct)), y = int_pct)) +
             geom_col(fill = "darkslategray4") + 
             labs(x = "",
                  y="NFL Int%",
                  title="Top 10 Career NFL Int%",
                  caption="Data from Pro Football Ref") +
          scale_y_percent() + 
          coord_flip() + 
             theme_ipsum_rc() -> top_ten_NFL_int_pct
    
