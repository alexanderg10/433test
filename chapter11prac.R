library(tidyverse)
#read
read_csv("a,b,c
1,2,3
4,5,6")
#skipping
read_csv("# A comment I want to skip
x,y,z
1,2,3", comment = "#")
read_csv("The first line of metadata
The second line of metadata
x,y,z
1,2,3", skip = 2)
#column names
read_csv("1,2,3\n4,5,6", col_names = FALSE)
read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
read_csv("a,b,c\n1,2,.", na = ".")
#exercise(s)
read_csv("x,y\n1,'a,b'", quote = "'")
