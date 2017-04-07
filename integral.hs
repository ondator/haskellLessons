module Integral where

integration x a b = h * ((x a + x b) / 2 + helper 1 x 0) where 
				n = 10000
				helper i x res | i < (n - 1) = helper (i + 1) x (res + x(a + h * i))
				  			   | otherwise = res + x(a + h * i)
				h = (b - a) / n