module Main
where
import System.Time 
import Control.Parallel 
import System.Random

qsort :: Ord a => [a] −> [a] 
qsort [] = [] 
qsort (x:xs) = lowsort ++ x : highsort 

where 
	lowsort = qsort [y | y <− xs, y < x] 
	highsort = qsort [y | y <− xs, y >= x]
	
secDiﬀ :: ClockTime −> ClockTime −> Float 
secDiﬀ (TOD secs1 psecs1) (TOD secs2 psecs2) = fromInteger (psecs2 − psecs1) / 1e12 + fromInteger (secs2 − secs1)

main :: IO () 
	main = do t0 <− getClockTime
		let input = (take 20000 (randomRs (0,100) (mkStdGen 42)))::[Int] 
		seq (forceList input) (return ()) 
		t1 <− getClockTime 
		let r = sum (qsortF input) 
		seq r (return ())
		t2 <− getClockTime
		putStrLn (''Sum: '' ++ show r) 
		putStrLn (''Time measured: '' ++ show (secDiﬀ t1 t2))
