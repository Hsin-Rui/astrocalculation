# load_fonts()
# showtext_auto()
# 
# circle <- get_circle_coords(r=0.95, length.out=156)
# 
# sign_x <- circle$x[seq(from=7, by=13, length.out=12)]
# sign_y <- circle$y[seq(from=7, by=13, length.out=12)]
# 
## p_empty_whole_sign <- draw_chart_template()
# saveRDS(p_empty_whole_sign, "./inst/ggplot_objects/p_empty_whole_sign.rds")
#   
# # geom_text(aes(x=sign_x, y=sign_y, label=zodiac_sign), family="HamburgSymbols", size=6, color=zodiac_sign_color)
# 
# p_empty_brennan <- draw_chart_template("chris brennan")
# saveRDS(p_empty_brennan, "./inst/ggplot_objects/p_empty_brennan.rds")
# 
# p_empty_quadrant <- draw_chart_template("others")
# saveRDS(p_empty_quadrant, "./inst/ggplot_objects/p_empty_quadrant.rds")
# 
# geom_text(aes(x=sign_x, y=sign_y, label=zodiac_sign), family="HamburgSymbols", size=6, color=zodiac_sign_color)
# 
# draw_chart_template("others")

## 3. define x, y for the asepct tables
left_end <- -0.2
right_end <- left_end + 1.3
bottom <- -2.5
top<- bottom+1.3

x_vert <- seq(from=left_end, by=0.1, length.out=13)
x_hor <- rep(left_end, 14)
y_vert <- rep(bottom, 13)
y_hor <- seq(from=bottom, by=0.1, length.out=14)

xend_vert <- x_vert
xend_hor <- c(rep(right_end-0.1, 2), seq(from=right_end-0.1, by=-0.1, length.out=12))
yend_vert <- c(top, seq(top, by=-0.1, length.out=12))
yend_hor <- seq(from=bottom, by=0.1, length.out=14)

## 4. define x, y for astrological elements (aspect table)

planet_table_x <- rep(-1, 14)
planet_table_y <- seq(top, by=-0.1, length.out=14)
planets <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "U", "L", "P", "Q")


# 
#   # aspect table lines
#   geom_segment(aes(x=x_vert,y=y_vert,xend=xend_vert,yend=yend_vert),linewidth=0.3)+ 
#   geom_segment(aes(x=x_hor,y=y_hor,xend=xend_hor,yend=yend_hor),linewidth=0.3) +
#   # All astrological elements on planet table with AstroDotBasic fonts
#   annotate("text", x= planet_table_x, y=planet_table_y, label=planets, color="black", size=4.5, family ="AstroDotBasic") +
#   # sign border
#   