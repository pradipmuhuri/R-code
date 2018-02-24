
observed <- scan(text='
1 0 1 2 2 2 2 1 3 1 3 3
4 5 4 8 5 5 5 9 6 17 6 9
7 24 7 16 8 23 8 27',
                 what=list(integer(),integer(),integer(),integer()),
                 sep=' ',
)
names(observed) <- c('weeks','calls', 'x1','x2' )
observed <- as.data.frame(observed)

observed