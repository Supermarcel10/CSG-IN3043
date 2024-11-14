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
products order = removeDuplicates $ Prelude.map retrieveProductFromOrder order

-- a list of products that have been ordered but were not in the price list,
-- each with a list of the customers who ordered them (without repetitions).
unavailable :: [Order] -> PriceList -> [(Product, [Customer])]
unavailable orders prices = Map.toList $ Map.filterWithKey (\p _ -> Map.notMember p prices) $ groupCustomersByPurchasedProduct orders

-- a list of customers who ordered products in the price list,
-- together with the total value of the products they ordered.
bill :: [Order] -> PriceList -> [(Customer, Double)]
bill orders prices = undefined

-- like bill, but applying a "buy one, get one free" discounting policy,
-- i.e. if a customer orders 4 of a given product, they pay for 2;
-- if they order 5, they pay for 3.
bill_discount :: [Order] -> PriceList -> [(Customer, Double)]
bill_discount orders prices = undefined


-- EXTRA FUNCTIONS
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates xs = Set.toList (Set.fromList xs)

retrieveProductFromOrder :: Order -> Product
retrieveProductFromOrder (Order _ p _) = p

groupCustomersByPurchasedProduct :: [Order] -> Map Product [Customer]
groupCustomersByPurchasedProduct orders = Map.fromListWith (++) [(p, [c]) | Order c p _ <- orders]
