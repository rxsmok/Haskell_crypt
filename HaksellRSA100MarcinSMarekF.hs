import System.Random
import Foreign.Marshal 
import System.IO.Unsafe
import Debug.Trace
import Data.List.Split
import System.Environment
import System.IO
import Data.Char(toUpper)
import qualified Data.ByteString as B

--import GHC.Prim
import Data.Word
randomInt :: Integer -> Integer -> Integer
randomInt min max =  unsafePerformIO  (randomRIO (min,max) )








-- ekhm "czyste". Cii ze nie. czyste

rabinSaysSplit :: Integer -> (Integer , Integer) -- (a,b) ->  (2^b)*a = x-1 
rabinSaysSplit x  | x `mod` 2 == 0 = (0,0)
			      | otherwise = rabinSaysSplitInner (x-1) 0
				  where 
						rabinSaysSplitInner:: Integer -> Integer -> (Integer , Integer) 
						rabinSaysSplitInner d counter | d `mod` 2 == 1 = (d,counter)
													  | otherwise = rabinSaysSplitInner (d `div` 2)  (counter + 1 ) 
													  
powm :: Integer -> Integer -> Integer -> Integer -> Integer -- a^b  i zwraca tyle cyfr ile rozmiar m; r=1
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r
 												  
													  
superMod :: Integer -> Integer -> Integer -> Integer -> Bool  ---spelnie rownianie typu  a^b mod n == x 
superMod a b n x =  ((powm a b 100 1) `mod ` n) == x  
													 
last100Dig ::	 Integer -> Integer  -> Integer
last100Dig a b	= powm a b (10^100) 1										 
													  
													  
---------------------------------------------------------												  
-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
 
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0  --- co robi iterate ?
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)
													  
													  
													 

----------------------------------------------------------------------------------------

rabinYesRabinNo :: Integer -> Integer -> Bool
rabinYesRabinNo val precision = rabinYesRabinNoInner val precision 0 (rabinSaysSplit val)
			where 
			rabinYesRabinNoInner :: Integer -> Integer -> Integer -> (Integer,Integer) -> Bool
			rabinYesRabinNoInner val2 precision2 counter (d,s) | counter == precision2 = True
															   | otherwise = (rabinYesRabinNoInner2 val2 (d,s) counter (randomInt 1 (val2-1)) ) 	&&( rabinYesRabinNoInner val2 precision2 (counter +1 ) (d,s)) 
																where 
																rabinYesRabinNoInner2 :: Integer -> (Integer,Integer) -> Integer   -> Integer -> Bool
																rabinYesRabinNoInner2 val3   (d2,s2) cntDEBUG a  | a ^ d2 `mod` val3 /= 1 =  True
																													      | otherwise = rabinYesRabinNoInner3  val3  (d2,s2) a 0
																														  where
																														  rabinYesRabinNoInner3 :: Integer  -> (Integer,Integer) -> Integer -> Integer -> Bool
																														  rabinYesRabinNoInner3 val4 (d3,s3) a2 counter2 | counter2 == s3 =  False
																																										 | (a2 ^ ( (2^counter2)* d3) ) `mod` val4 /= (val4 -1) =  True
																																										 | otherwise = rabinYesRabinNoInner3 val4 (d3,s3) a2 (counter2+1)

																																											--(a2 ^ ( (2^counter2)* d3) ) `mod` val4 /= (-1) =  True
																																										 --not( superMod a2 ((2^counter2)* d3) val4 (-1) ) =  True   --
																																										 --last100Dig a2 ( (2^counter2)* d3)  `mod` val4 /= (-1) = True

------------------------RIP																																										 
																																										 
																																										 

																																										 
																																											
rabinPick2Big :: n -> (Integer,Integer)
rabinPick2Big n = (rabinPickBig1 1,rabinPickBig2 0)																															
											 
rabinPickBig1 :: a ->Integer
rabinPickBig1 n = if millerRabinPrimality a 10 then a else rabinPickBig1 n
				where a = randomInt (100000000000000000000000000000000000000000000000000)  ( 500000000000000000000000000000000000000000000000000 )

rabinPickBig2 n = if millerRabinPrimality a 10 then a else  rabinPickBig2 n
				where a =randomInt ( 500000000000000000000000000000000000000000000000001) ( 999999999999999999999999999999999999999999999999999)
				
				
rabinIfCoPrime :: Integer ->Integer -> Bool
rabinIfCoPrime	a b = (euklides a b)  == 1 
				where
				euklides a b = if b == 0 then a else euklides b (a `mod` b)
				

				
rabinGibFiN :: (Integer , Integer) -> Integer
rabinGibFiN (p,q) = (p-1)*(q-1)

rabinGimmeE :: Integer -> Integer
rabinGimmeE fiN = if rabinIfCoPrime fiN a then a else rabinGimmeE fiN
				where	
				a= randomInt 1 (fiN-1)					
				

			
rabinGimmeD:: Integer -> Integer -> Integer
rabinGimmeD e fiN = rabinGimmeDins e fiN 0 fiN 1 e
				where
					rabinGimmeDins :: Integer ->Integer ->Integer ->Integer ->Integer ->Integer ->Integer 
					rabinGimmeDins e fiN t r newt newr  | newr == 0 =  if t < 0 then (t + fiN) else t
													    | otherwise = rabinGimmeDins e fiN newt newr (t - (r `div` newr) * newt) (r -(r `div` newr)* newr)


														
														
		

--CHECK 		
rabinSplitFileIntoNumbers :: String -> [Integer] -- przeprowadzone Testy jednostkowe
rabinSplitFileIntoNumbers []  = []
rabinSplitFileIntoNumbers inpStr = map (haskellJestUposledzonyItoBardzo 0) (chunksOf 30 inpStr)

--CHECK
rabinGetLastSize :: String -> Int
rabinGetLastSize inpStr = ((length inpStr) `mod` 30)

														
-- haskell jest uposledzony XD. przekopoiowane z where osobno dziala. I don't even									
haskellJestUposledzonyItoBardzo :: Integer -> String ->  Integer
haskellJestUposledzonyItoBardzo acc [] = acc `div` 256
haskellJestUposledzonyItoBardzo acc (x:xs) =  haskellJestUposledzonyItoBardzo ((acc + (toInteger (fromEnum x)))* 256) xs





--CHECK
rabinCrypt:: Integer -> Integer -> [Integer] -> [Integer]
rabinCrypt _ _ []=[]
rabinCrypt e n (x:xs)=  (((powm x e n 1 ) `mod` n):(rabinCrypt e n xs))
-- encryption and decrytpion follow the same algorithm, 


rabinCreateCryptedFile:: String -> Integer-> Integer -> String
rabinCreateCryptedFile inpStr e n = [toEnum (rabinGetLastSize inpStr)] ++ (rabinNumbersToFile (rabinCrypt e n (rabinSplitFileIntoNumbers inpStr)) n ) 
-- tworzy szyfrowany plik waraz z naglowkiem mowiacym o ilosci z nakow w ostatnim chunku. naglwek jest nie szyfrowany i kazdy zaszyfrowany chunk ma dokladnie sufit(log 256 n) charekterow, lacznie z ostatnim 

--Check oprocz rabinCryptedToNumbers
rabinReadCryptedFile:: String -> Integer ->Integer-> String
rabinReadCryptedFile (x:inpStr) d n = foldr1 (++) (rabinDecryptedToString  (fromEnum x)   (rabinCrypt d n (rabinCryptedToNumbers inpStr n)))


--Tylko To sie sypie +/- zapis ale jest gucci albo zapis sie sypie 
{-
rabinCryptedToNumbers :: String -> Integer -> [Integer]
rabinCryptedToNumbers inpStr n = (map (f1 (ceiling (logBase (fromIntegral 256) (fromIntegral n) ) ) 0   ) (chunksOf (ceiling(logBase (fromIntegral 256) (fromIntegral n) ) ) inpStr))

f1:: Integer -> Integer -> String ->  Integer
f1 0 acc _  = acc
f1 k acc [] = f1 (k-1) (acc*256) []
f1 n acc (x:xs)= f1 (n-1) (acc*256+(toInteger(fromEnum x))) xs
-}
----
rabinCryptedToNumbers :: String -> Integer -> [Integer]
rabinCryptedToNumbers inpStr n = map f2  (chunksOf (ceiling(logBase (fromIntegral 256) (fromIntegral n) ) ) inpStr)

f2 :: String -> Integer
f2  inpStr= f3  (reverse inpStr) 

f3 :: String -> Integer
f3  [] = 0 
f3  (x:xs)= (toInteger(fromEnum x)) + (f3  xs )*256 




-----------------------------------------------------------------------------
---CHECK
rabinDecryptedToString:: Int -> [Integer] -> [String]
rabinDecryptedToString _ [] = []
rabinDecryptedToString sizeOfLast (x:xs) | (length xs==0) && (sizeOfLast /= 0) = [haskellJestUposledzonyItoBardzo2  "" (toInteger sizeOfLast) x]
										 | otherwise = [(haskellJestUposledzonyItoBardzo2  "" 30 x )]++ (rabinDecryptedToString sizeOfLast xs) 
								


-- nvm to raczej dziala (tak 95%)


--- jakims #########, rabinNumbersToFile dla 2 roznych liczb daje ten sam ciag znakow o.0
rabinNumbersToFile :: [Integer] -> Integer -> String
rabinNumbersToFile [] _= []
rabinNumbersToFile l n = foldr1 (++) (map (haskellJestUposledzonyItoBardzo2 "" (ceiling(logBase (fromIntegral 256) (fromIntegral n) ) )) l)
{-
map' :: (Integer -> String) -> [Integer] -> [String]
map' _ [] =[]
map' f (x:xs)= (f x):(map' f xs)
					
-}
------ to nie powinno byc mozlwie
haskellJestUposledzonyItoBardzo2 :: String ->Integer -> Integer -> String
haskellJestUposledzonyItoBardzo2 acc 0 _ = reverse acc
haskellJestUposledzonyItoBardzo2 acc n x = haskellJestUposledzonyItoBardzo2   (acc ++ [toEnum(fromInteger (x `mod` 256))]) (n-1) ( x `div` 256) 



haskellTest :: Int ->Char
haskellTest a = toEnum 255::Char








														
														

{-  Euklidesian based algortihm 
function inverse(a, n)
    t := 0;     newt := 1;    
    r := n;     newr := a;    
    while newr ≠ 0
        quotient := r div newr
        (t, newt) := (newt, t - quotient * newt) 
        (r, newr) := (newr, r - quotient * newr)
    if r > 1 then return "a is not invertible" -- nie interere nas bo a zawsze odwracalne bo z def GCD(e,fiN) =1
    if t < 0 then t := t + n 
    return t
-}

stringChToStringW8:: [Char] -> [Word8]

stringChToStringW8 k= map (\x -> fromIntegral (fromEnum x)) k


stringW8ToStringCh:: [Word8] -> [Char]

stringW8ToStringCh k= map (\x -> toEnum (fromIntegral x)) k












encrypt:: IO()
encrypt = do
	print "podaj sciezkud do odczytu \n"
	inFileName <- getLine 
	print "podaj sciezkud do zapisu\n"
	outFileName <- getLine
	inHdlr <- openFile inFileName ReadMode
	outHdlr <- openFile outFileName WriteMode
	inpStr <- hGetContents inHdlr
	let(p,q) = rabinPick2Big 1
	let n = p* q
	putStr("to bedzie n:" ++ (show n) ++ "\n")
	let fiN = rabinGibFiN (p,q)
	let e = rabinGimmeE fiN
	let d = rabinGimmeD e fiN
	putStr("to bedzie e (klucz szyfrujacy/publiczny):" ++ (show e) ++ "\n")
	putStr $ "to bedzie d (klucz dezyfrujacy/prywatny):" <> show d ++ "\n" -- jes prywatny bo sluzy do de szyfrowania
	let inpStr2=rabinCreateCryptedFile inpStr e n 
	--putStr("\n metoda wedrujacej dupy 1\n")
	let inpStrW8= stringChToStringW8 inpStr2
	let bsOutStr=B.pack inpStrW8
	--hPutStr outHdlr inpStr2
	B.hPut outHdlr bsOutStr
	hClose inHdlr
	hClose outHdlr
	
decrypt = do
	print "podaj sciezkud do odczytu \n"
	inFileName <- getLine 
	print "podaj sciezkud do zapisu\n"
	outFileName <- getLine
	print "podaj n"
	nStr <- getLine
	print "podaj d"
	dStr <- getLine
	inHdlr <- openFile inFileName ReadMode
	outHdlr <- openFile outFileName WriteMode
	inpBStr <- B.hGetContents inHdlr
	let inpStr= stringW8ToStringCh ( B.unpack  inpBStr  )
	let n = read nStr :: Integer
	let d = read dStr :: Integer
	let inpStr2 = rabinReadCryptedFile inpStr d n
	
 
	hPutStr outHdlr inpStr2
	hClose inHdlr
	hClose outHdlr






















