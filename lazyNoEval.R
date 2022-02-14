foo = function(a) h(a)
h = function(z) g(z) + 10
g = function(a,b) {
      if(missing(b))
        return(20)
      a + b
    }

foo(x+1)

foo(z <- x+1)


if(FALSE) {
foo = function(a, ...) h(a, ...)
h = function(z, ...) g(z, ...) + 10
g = function(a,b) {
      if(missing(b))
        return(20)
      a + b
    }

foo(100)

foo(x+1, 99)

# Error in g(z, ...) : object 'x' not found
}
