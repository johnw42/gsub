module UtilsTest where

import TestUtils

-- Test that firstJust works.
prop_firstJust_empty = once $ isNothing $ firstJust []
prop_firstJust_allNothing (Positive (Small n)) =
    isNothing $ firstJust $ replicate n Nothing
prop_firstJust_typical (NonEmpty items) =
    case firstJust items of
        Nothing -> all isNothing items
        Just x -> Just x == head (dropWhile isNothing items)

return []
test = $forAllProperties quickCheckResult
