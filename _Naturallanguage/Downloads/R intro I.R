
# R as a calculator
2 + 2

# assignment
a <- 2 * 4
a

# mode of data
mode(a)
mode('hello world')
mode(TRUE)

# vectors have to contain data of one mode
a <- c(1,2,3)
b <- c(1,'2',TRUE)
d <- c(1,FALSE)


# select element from a vector
a <- c(5,6,7)
a[2]

# create matrix filled with random draws from N(0,1)
f <- matrix(rnorm(300,0,1),ncol=10,nrow=30)

# create data frame from matrix
e <- data.frame(f)

# select elements from matrix / data frame
f[2,2]
e[2,2]

# select column from matrix / data frame
f[,2]
e[,2]

# select row: matrix becomes vector, data frame stays data frame
is.vector(f[2,])
is.matrix(f[2,])
is.vector(e[2,])
is.matrix(e[2,])
is.data.frame(e[2,])

# creater functions
vector('numeric')
matrix()
data.frame()
list()

# create lists: can contain any object
g = list(c(1,2),c('1','2'),c(FALSE,TRUE))
h = list(list(c(1,2)),c(3,4),5)

# selecting elements from lists requires double brackets
is.list(h[3])
h[[3]]
h[[2]]
h[[1]][[1]][1]

# data frames are lists with elements of same lengths and single modes
i = data.frame(cbind(c(1,2,3),c(4,5,6)))
l = list(c('hello','world'),'world')

# data frames are lists, lists are not data frames
is.data.frame(i)
is.list(i)
is.data.frame(l)
is.list(l)

# R repeates elements of smaller element if multiple of larger element
l = data.frame(list(c('hello','world'),'world'))
l = data.frame(list((c('hello','world','!'),c('hello','world')))

# names of data frame columns can be used as selectors using the dollar operator
names(i)
i$X1
i$X2

# this doesnt work for vectors
k = c(1,2,3)
names(k) = c('first','second','third')
k
k$first

# selection by logical statements
sel = f[,1] > 1
f[sel,]
f[!sel,]
sum(sel)
nrow(f[sel,])
nrow(f[!sel,])

# turn logical vector into index vector
which(sel)
f[which(sel),]
f[sel,]

# create new data frame from matrix f
o = data.frame(f)

# create range using colon operator
1:30
1:nrow(o)

# create data container: empty character vector
biologist <- character(nrow(o))

# extract first column: used for logical test
first_column <- o[,1]

# start for loop from 1 to nrow(o)
for(i in 1:nrow(o)){

  # extract value at index i from first column
  value = first_column[i]

  # test if value is larger then one
  if(value > 1){

    # if larger then one store 'biologist at index i in data container
    biologist[i] <- 'biologist'

  # execute if not larger than one
  } else {

    # if larger then one store 'not a biologist at index i in data container
    biologist[i] <- 'not a biologist'
  }
}

# show data container in interpreter
biologist

# change first column of o to the contents of the data container
o[,1] <- biologist




