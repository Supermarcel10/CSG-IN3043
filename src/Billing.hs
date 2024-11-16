module Billing where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Customer = String
type Product = String

-- An order of some positive quantity of a product by a customer
data Order = Order Customer Product Int
  deriving (Show)

-- Prices for some products
type PriceList = Map Product Double


-- COURSEWORK FUNCTIONS
-- a list (without repetitions) of all the products that have been ordered.
products :: [Order] -> [Product]
products orders = removeDuplicates [p | Order _ p _ <- orders]

-- a list of products that have been ordered but were not in the price list,
-- each with a list of the customers who ordered them (without repetitions).
unavailable :: [Order] -> PriceList -> [(Product, [Customer])]
unavailable orders prices = Map.toList $ Map.filterWithKey (\p _ -> Map.notMember p prices) $ groupCustomersByPurchasedProduct orders

-- a list of customers who ordered products in the price list,
-- together with the total value of the products they ordered.
bill :: [Order] -> PriceList -> [(Customer, Double)]
bill = billOrdersUsingGivenBillingMethod sumOfAllProducts

-- like bill, but applying a "buy one, get one free" discounting policy,
-- i.e. if a customer orders 4 of a given product, they pay for 2;
-- if they order 5, they pay for 3.
bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount = billOrdersUsingGivenBillingMethod sumOfAllProductsWithBuyOneGetOneFree


-- EXTRA FUNCTIONS
-- Start Source: https://www.reddit.com/r/haskell/comments/bjeon4/comment/em8wvit/
-- Setting to set and back would break order of [Char], and after some searching this seems to be the easiest and cleanest approach I found
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (a:as) = a:removeDuplicates (filter (/=a) as)
-- End Source

removeDuplicatesInMapVal :: (Eq b) => Map a [b] -> Map a [b]
removeDuplicatesInMapVal = Map.map removeDuplicates

groupCustomersByPurchasedProduct :: [Order] -> Map Product [Customer]
groupCustomersByPurchasedProduct orders = removeDuplicatesInMapVal $ Map.fromListWith (++) [(p, [c]) | Order c p _ <- orders]

groupProductsByCustomer :: [Order] -> Map Customer (Map Product Int)
groupProductsByCustomer orders = Map.fromListWith (Map.unionWith (+)) [(c, Map.singleton p q) | Order c p q <- orders]

billOrdersUsingGivenBillingMethod :: (Map Product Int -> PriceList -> Double) -> [Order] -> PriceList -> [(Customer, Double)]
billOrdersUsingGivenBillingMethod billingMethod orders prices = [(c, v) | (c, p) <- Map.toList $ groupProductsByCustomer orders, v <- [billingMethod p prices]]

sumOfAllProducts :: Map Product Int -> PriceList -> Double
sumOfAllProducts products prices = sum [fromIntegral q * v | (p, q) <- Map.toList products, Just v <- [Map.lookup p prices]]

buyOneGetOneFree :: Integral a => a -> a
buyOneGetOneFree n = (n + 1) `div` 2

sumOfAllProductsWithBuyOneGetOneFree :: Map Product Int -> PriceList -> Double
sumOfAllProductsWithBuyOneGetOneFree products prices = sum [fromIntegral (buyOneGetOneFree q) * v | (p, q) <- Map.toList products, Just v <- [Map.lookup p prices]]
