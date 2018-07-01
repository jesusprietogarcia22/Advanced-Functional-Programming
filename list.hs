mapNext :: [Integer] -> [Integer] 
mapNext [] = [] 
mapNext (x:xs) = x+1 : mapNext xs

filterPositive  :: [Integer] -> [Integer] 
filterPositive [] = [] 
filterPositive (x:xs) | x>=0 = x : 
filterPositive xs |otherwise = filterPositive xs

mapSqr :: [Integer] -> [Integer] 
mapSqr [] = [] 
mapSqr (x:xs) = mp (filterPositive xs)   
	where mp [] = []         
		mp (x:xs) = (x * x) : mp xs

reverse :: [a] -> [a] 
reverse [] = [] 
reverse (x:xs) = reverse xs ++ [x]

nolast :: [a] -> [a] 
nolast [] = [] 
nolast [n] =[] 
nolast (x:xs) = x : nolast xs

lasts :: [a] -> a 
lasts [] = error "Empty list" 
lasts [n] = n 
lasts (x:xs) = lasts xs

maximum :: [Int] -> Int 
maximum []= 0 
maximum (x:xs) | x > (maximum xs)= x | otherwise = maximum xs










		
		
