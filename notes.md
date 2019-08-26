## Development process

```
Sunday
18.00 - Started to read & understand requirements
18.20 - Draft design of potential solution
18.35 - Break
18.40 - Draft design of potential solution (continue)
19.10 - Break (design ready)

Monday
21.00 - Setup boilerplate
22.00 - First user story (& fp-ts boilerplate)
```

## Solution Design

### API

These are the available endpoints of the cart:

- **List items** (available items in store)

```
GET /items
```

```haskell
type Price = Int -- cents
type SKU = String

data Item = Item
  { sku         :: SKU -- i.e. ID
  , price       :: Price
  , displayName :: String
  } deriving (Eq, Show)


data ItemsResponse = ItemsResponse
  { data :: [Item]
  } deriving (Eq, Show)
```

- **List discounts** (available discounts)

```
GET /discounts
```

```haskell
type Percentage = Int -- 10, 20, 30, ..., 100
type DiscountCode = String

data Discount = Discount
  { code          :: DiscountCode
  , percentage    :: Percentage
  } deriving (Eq, Show)


data DiscountsResponse = DiscountsResponse
  { data :: [Discount]
  } deriving (Eq, Show)
```

- **Show cart**

```
GET /user/:user_id/cart
```

```haskell
data Cart = Cart
  { totalPrice :: Price
  , items      :: Map SKU Amount -- { [sku]: amount }
  , discount   :: Maybe DiscountCode
  } deriving (Eq, Show)


data CartResponse = CartResponse
  { data :: Cart
  } deriving (Eq, Show)
```

- **Mutate cart** (add/remove item, increase/decrease count, apply discount
  code, empty cart)

```
PUT /user/:user_id/cart
```

```haskell
type Count = Int

data Mutation
  = AddItem SKU Count
  | UpdateItem SKU Count
  | DeleteItem SKU
  | ApplyDiscountCode DiscountCode
  | EmptyCart
  deriving (Show, Eq)


data RequestBody = RequestBody
  { mutation :: Mutation
  } deriving (Eq, Show)


data MutationResponse = MutationResponse
  { data :: Cart -- return the whole cart or only the price ?
  } deriving (Eq, Show)
```

### Events

These are the events tracked when mutating the user's carts.

```haskell
type UserID = String

data Event = Event
  { time     :: Time
  , userID   :: UserID
  , mutation :: Mutation
  } deriving (Eq, Show)
```

- **List events** (all the history of events)

```
GET /events
```

```haskell
data EventsResponse = EventsResponse
  { data :: [Event]
  } deriving (Eq, Show)
```

- **List events for a user**

```
GET /events/:user_id
```

```haskell
data EventsResponse = EventsResponse
  { data :: [Event]
  } deriving (Eq, Show)
```
