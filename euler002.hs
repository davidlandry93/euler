-- Euler002: sum of the even fibonnaci numbers no larger that 4 million

sumOfEvenFibo n = let 
    sumOfEvenFibo_rec n secondPrevious previous acc = 
        if next > n then acc else sumOfEvenFibo_rec n previous next newAcc
        where 
            next = secondPrevious + previous 
            newAcc = if (even next) then acc + next else acc
    in
    sumOfEvenFibo_rec n 1 1 0
