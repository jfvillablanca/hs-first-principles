mult1 = x * y
  where
    x = 5
    y = 6

firstLet =
    let x = 3
        y = 1000
     in x * 3 + y

firstWhere = x * 3 + y
  where
    x = 3
    y = 1000

secondLet =
    let y = 10
        x = 10 * 5 + y
     in x * 5

secondWhere = x * 5
  where
    y = 10
    x = 10 * 5 + y

thirdLet =
    let x = 7
        y = negate x
        z = y * 10
     in z / x + y

thirdWhere =  z / x + y
    where  
        x = 7
        y = negate x
        z = y * 10
       
