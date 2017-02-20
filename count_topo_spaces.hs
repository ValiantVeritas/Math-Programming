import Control.Monad
import Data.List

--Implementation of the powerset found at this link http://evan-tech.livejournal.com/220036.html
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

is_subset x y = all (`elem`  y) x

--calculates the indiscrete topology for a given set
indiscrete :: [t] -> [[t]]
indiscrete n = n:[]:[]

--calculates the discrete topology for a given set
discrete :: [t] -> [[t]]
discrete n = powerset n

--the empty set and x itself belongs to t, the list of open sets
axiom1 :: Eq a => ([a], [[a]]) -> Bool
axiom1 (x, t) = (elem x t) && (elem [] t) 

--recursively union a set of sets
union_mutliple :: Eq t => [[t]] -> [t]
union_mutliple [] = []
union_mutliple [x] = x
union_mutliple [x,y] = union x y
union_mutliple (x:xs) = union x (union_mutliple xs)

--any union of open sets in t is an open set contained in t
axiom2 :: Ord a => [[a]] -> Bool
axiom2 t = is_subset all_unions t where all_unions = nub (map union_mutliple (powerset t))

--recursively intersect a set of sets
intersect_mutliple :: Eq t => [[t]] -> [t]
intersect_mutliple [] = []
intersect_mutliple [x] = x
intersect_mutliple [x,y] = intersect x y
intersect_mutliple (x:xs) = intersect x (intersect_mutliple xs)

--the intersection of any finite number of open sets in t is an open set contained in t
axiom3 :: Eq t => [[t]] -> Bool
axiom3 t = is_subset all_intersections t where all_intersections = nub (map intersect_mutliple (powerset t))

is_topo_space :: Ord t => ([t], [[t]]) -> Bool
is_topo_space (x, t) = axiom1 (x,t) && axiom2 t && axiom3 t
						
topo_spaces :: Ord t => [t] -> [[[t]]]					   				   	
topo_spaces x = filter (\n -> is_topo_space (x, n)) (powerset (powerset x)) 	

count_topo_spaces :: Ord t => [t] -> Int
count_topo_spaces x = length (topo_spaces x)

is_topo = ([1,2,3], [[1,2,3], [1,2], [1], [2], []])

not_topo = ([1,2,3], [[1,2,3], [2], [3], []])
