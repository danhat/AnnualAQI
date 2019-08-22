# diagonal bar graph



Example.Data <- data.frame(matrix(vector(), 0, 3, dimnames = list(c(), c("Value", "Variable", "Fill"))), stringsAsFactors = F)

Example.Data[1, ] <- c(20, 'Diagonal Pattern', 'Diagonal Pattern')
HighlightDiag <- Example.Data[1,]

HighlightDiag$Value <- as.numeric(HighlightDiag$Value)
Example.Data$Value <- as.numeric(Example.Data$Value)

highest_value = 20
lowest_value = 0
line_width = 1.45
Diag <- data.frame(
  x = c(1, 1, 1.45, 1.45), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y = c(lowest_value, lowest_value, highest_value, highest_value),

  x2 = c(1.2, 1.2, 1.45, 1.45), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y2 = c(lowest_value, lowest_value, 11.5, 11.5), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x3 = c(1.38, 1.38, 1.45, 1.45), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y3 = c(lowest_value, lowest_value, 3.5, 3.5), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x4 = c(.8, .8, 1.26, 1.26), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y4 = c(lowest_value, lowest_value, highest_value, highest_value), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x5 = c(.6, .6, 1.07, 1.07), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y5 = c(lowest_value, lowest_value, highest_value, highest_value), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x6 = c(.555, .555, .88, .88), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y6 = c(6, 6, highest_value, highest_value), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x7 = c(.555, .555, .72, .72), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y7 = c(13, 13, highest_value, highest_value), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.

  x8 = c(.8, .8, 1.26, 1.26), # 1st 2 values dictate starting point of line. 2nd 2 dictate width. Each whole = one background grid
  y8 = c(lowest_value, lowest_value, highest_value, highest_value), # inner 2 values dictate height of horizontal line. Outer: vertical edge lines.
  #Variable = "Diagonal Pattern",
  Fill = "Diagonal Pattern"
)

ggplot(Example.Data, aes(x = Variable, y = Value, fill = Fill)) + theme_bw() + #facet_wrap(~Product, nrow=1)+ #Ensure theme_bw are there to create borders
  theme(legend.position = "none") +
  scale_fill_grey(start = .4) +
  #scale_y_continuous(limits = c(0, 100), breaks = (seq(0,100,by = 10)))+
  geom_bar(position = position_dodge(.9), stat = "identity", colour = "black", legend = FALSE) +
  #geom_path(data = Diag, aes(x = x, y = y), colour = "black") + # calls co-or for sig. line & draws
  #geom_path(data = Diag, aes(x = x2, y = y2), colour = "black") + # calls co-or for sig. line & draws
  #geom_path(data = Diag, aes(x = x3, y = y3), colour = "black") +
  #geom_path(data = Diag, aes(x = x4, y = y4), colour = "black") +
  #geom_path(data = Diag, aes(x = x5, y = y5), colour = "black") +
  #geom_path(data = Diag, aes(x = x6, y = y6), colour = "black") +
  #geom_path(data = Diag, aes(x = x7, y = y7), colour = "black")
  geom_path(data = Diag, aes(x = x, y = y), colour = "black") + # calls co-or for sig. line & draws
  geom_path(data = Diag, aes(x = x2, y = y2), colour = "black") + # calls co-or for sig. line & draws
  geom_path(data = Diag, aes(x = x3, y = y3), colour = "black") +
  geom_path(data = Diag, aes(x = x4, y = y4), colour = "black") +
  geom_path(data = Diag, aes(x = x5, y = y5), colour = "black") +
  geom_path(data = Diag, aes(x = x6, y = y6), colour = "black") +
  geom_path(data = Diag, aes(x = x7, y = y7), colour = "black")








