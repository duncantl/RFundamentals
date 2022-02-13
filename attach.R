mtcars$mpg
class(mtcars)

search()

attach(mtcars)
search()

mpg
# where did we find the symbol mpg?

find("cyl")

mtcars$cyl[1]
cyl[1]
# So the same.


mtcars$cyl[1] = 100
mtcars$cyl[1]

cyl[1]
# So now cyl and mtcars$cyl are different.

find("cyl")

cyl[1] = 25

find("cyl")
# So cyl is now in 3 places
#  mtcars$cyl
#  the environment on the search path which is named mtcars, i.e. search()[2]
#    which we can access as an environment with parent.env(globalenv())

get("cyl", 2)[1]



