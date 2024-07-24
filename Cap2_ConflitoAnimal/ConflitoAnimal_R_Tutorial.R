source("mygame.R")
mygame()

# H-D

cat(readLines("game/H-D.txt"), sep="\n")
mygame("game/H-D.txt")
mygame("game/H-D.txt", options=c("arrow_size=2","edges"))
mygame("game/H-D.txt", v=3, c=2, 
       options=c("arrow_size=2","edges"))

# H-D-R: mygame.R

cat(readLines("game/H-D-R.txt"), sep="\n")
mygame("game/H-D-R.txt", lines=30, options=c("corners"))
mygame("game/H-D-R.txt", lines=30, options=c("edges"))
mygame("game/H-D-R.txt", lines=30, options=c("borders"))
mygame("game/H-D-R.txt", lines=30, options=c("inner_area"))
mygame("game/H-D-R.txt", lines=30)
       
# H-D-Retaliator: eiras.animalconflict.R

source("eiras.animalconflict.R") 

# game/H-D-R_v2c3.xlsx

source("eiras.ternary.replot.R")
eiras.ternary.replot("game/H-D-R_results.xlsx")

source("eiras.animalconflict.R") 

# game/H-D-R_v3c2.xlsx
# game/H-D-R_v2c2.xlsx

# H-D-HD: estratégia contínua: mygame.R

cat(readLines("game/H-D-HD.txt"), sep="\n")

mygame("game/H-D-HD.txt", 
       lines=50, 
       v=2, c=3, w0=5, 
       h=0.5, 
       options="end_dots")

mygame("game/H-D-HD.txt", 
       lines=50, 
       v=2, c=3, w0=5, 
       h=0.25, 
       options="end_dots")

mygame("game/H-D-HD.txt", 
       lines=50, 
       v=2, c=3, w0=5, 
       h=0.75, 
       options="end_dots")

# H-D-Bourgeois: eiras.animalconflict.R

source("eiras.animalconflict.R") 

# game/H-D-B_v2c3.xlsx
# game/H-D-B_v3c2.xlsx
# game/H-D-B_v2c2.xlsx

# H-D-Assessor: eiras.animalconflict.R

source("eiras.animalconflict.R") 

# game/H-D-A_v2c3.xlsx
# game/H-D-A_v3c2.xlsx
# game/H-D-A_v2c2.xlsx