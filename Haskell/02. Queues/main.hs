import System.CPUTime

class Queue q where
	push :: a -> q a -> q a
	pop :: q a -> q a
	top :: q a -> a
	emp :: q a -> Bool
	create :: a -> q a

-- SIMPLE QUEUE

data SimpleQueue a = Record [a]

instance Queue SimpleQueue where
	push x (Record v) = Record (v++[x])
	pop (Record v) = Record (tail v)
	top (Record v) = head v
	emp (Record v) = null v
	create x = Record [x]

instance Show a => Show (SimpleQueue a) where show (Record v) = show v

-- FAST QUEUE
-- На двух стеках.

data FastQueue q = FastRecord [q] [q]
instance Queue FastQueue where
    push x (FastRecord pushStack popStack) = FastRecord (x:pushStack) popStack
    
    pop (FastRecord pushStack []) = pop (FastRecord [] (reverse pushStack))
    pop (FastRecord pushStack popStack) = FastRecord pushStack (tail popStack)
    
    top (FastRecord pushStack []) = top (FastRecord [] (reverse pushStack)) 
    top (FastRecord pushStack popStack) = head popStack
    
    emp (FastRecord pushStack popStack) = null popStack && null pushStack
    
    create x = FastRecord [x] []
    
instance Show a => Show (FastQueue a) where 
    show (FastRecord pushStack popStack) = show ((reverse pushStack) ++ popStack)


-- BALANCED QUEUE
-- pushStack всегда длинее popStack

data BalancedQueue a = BalancedRecord [a] [a] Int Int

instance Queue BalancedQueue where
    push x (BalancedRecord [] [] 0 0) = BalancedRecord [] [x] 0 1
    push x (BalancedRecord pushStack popStack pushLen popLen) = 
        if pushLen <= (popLen+1) 
            then BalancedRecord (x:pushStack) popStack (pushLen + 1) popLen
            else push x (BalancedRecord [] (popStack ++ (reverse pushStack)) 0 (popLen + pushLen))

    pop (BalancedRecord pushStack popStack pushLen popLen) = 
        if pushLen <= (popLen+1) 
            then BalancedRecord pushStack (tail popStack) pushLen (popLen-1)
            else pop (BalancedRecord [] (popStack ++ (reverse pushStack)) 0 (popLen + pushLen))

    top (BalancedRecord pushStack popStack pushLen popLen) = head popStack
    emp (BalancedRecord pushStack popStack pushLen popLen) = null pushStack && null popStack
    
    create x = BalancedRecord [] [x] 0 1


instance Show a => Show (BalancedQueue a) where
    show (BalancedRecord pushStack popStack pushLen popLen) = show (popStack ++ (reverse pushStack))





test t n = do
	s <- getCPUTime
	let t1 = foldl (\q x -> push x q) t [1..n]
	print $ extr t1
	e <- getCPUTime
	print $ fromIntegral (e - s) / 10^12

extr q = do
	if emp q then 0
	else (1 + (extr (pop q)))

main = do
	let t1 :: SimpleQueue Int; t1 = create 0
	print t1 >> test t1 10000
	let t2 :: FastQueue Int; t2 = create 0
	print t2 >> test t2 100000
	let t3 :: BalancedQueue Int; t3 = create 0
	print t3 >> test t3 100000
