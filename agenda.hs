data Contact = MKP Name Surname Date deriving Show 
data Name = MKN String deriving Show 
data Surname = MKA String deriving Show 
data Date =MKF Int deriving Show 

paja :: [Contact] -> [Contact] 
paja p = [] 

takeContact :: [Contact] -> Date -> [Contact] 
takeContact [] f = [] 
takeContact (p:ps) f | isMinor p f = p : (takeContact ps f) | otherwise = [] 

isMinor :: Contact -> Date -> Bool 
isMinor (MKP n a (MKF fn)) (MKF f) = fn < f

takeNameContact :: [Contact] -> Date -> [Name] 
takeNameContact [] f = [] 
takeNameContact (p:ps) f | isMinor p f = (getName p) : (takeNameContact ps f) | otherwise = [] 

getName :: Contact -> Name 
getName (MKP n a f)=  n
