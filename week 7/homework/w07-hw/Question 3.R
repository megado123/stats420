#Question 3

fish = read_csv("fish.csv")
fish_smaller = lm(Weight ~ Length1 + HeightPct * WidthPct, data = fish)
summary(fish_smaller)
fish_int = lm(Weight ~ Length1 + HeightPct + WidthPct + Length1:HeightPct + Length1:WidthPct + HeightPct:WidthPct + Length1:HeightPct:WidthPct, data = fish)
summary(fish_int)


fish_int = lm(Weight ~ Length1 + HeightPct + WidthPct + Length1:HeightPct + Length1:WidthPct + HeightPct:WidthPct + Length1:HeightPct:WidthPct, data = fish)

