## Development process

```
18.00 - Started to read & understand requirements
18.20 - Draft design of potential solution
18.35 - Break
18.40 - Draft design of potential solution (continue)
```

## Solution Design

**List items** (available items in store)

```
GET /items


type Price = Int -- cents
type SKU = String

data Item = Item
  { sku         :: SKU
  , price       :: Price
  , displayName :: String
  } deriving (Eq, Show)


data ItemsResponse = ItemsResponse
  { data :: [Item]
  } deriving (Eq, Show)
```

**Show cart**

```
GET /user/:user_id/cart


data Cart = Cart
  { totalPrice   :: Price
  , items        :: Map SKU Amount -- { [sku]: amount }
  } deriving (Eq, Show)


data CartResponse = CartResponse
  { data :: Cart
  } deriving (Eq, Show)
```

**Mutate cart** (add/remove item, increase/decrease count, empty cart)

```
PUT /user/:user_id/cart


data Mutation
  = AddItem SKU
  | ChangeCount SKU Int
  | RemoveItem SKU
  | EmptyCart
  deriving (Show, Eq)


data RequestBody = RequestBody
  { mutation :: Mutation
  } deriving (Eq, Show)


data MutationResponse = MutationResponse
  { data :: Boolean -- Ok or not -> TODO: change this
  } deriving (Eq, Show)
```
