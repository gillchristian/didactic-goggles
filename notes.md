## Tech stack

For the tech stack I decided to go with a familiar framework (Node, Express &
TypeScript) but still experiment a bit using
[hyper-ts](https://gcanti.github.io/hyper-ts/) (from the
[fp-ts](https://github.com/gcanti/fp-ts) family).

> `hyper-ts` is an **experimental** middleware architecture for HTTP servers
> written in TypeScript.
>
> Its main focus is correctness and type-safety, using type-level information to
> enforce correct composition and abstraction for web servers.

Emphasis on the _experimental_. Which brought a few problems, mostly because it
depends on older versions of `fp-ts` & `io-ts` (pre v2) :confused:

Stil the composable approach and focus on type safety were very good benefits.
Type inference works most of the time as one would expect :metal:

## Architecture

Begin a simple API the architecture is simple as well.

```
src/
   |__ index.ts       -> entry point of the app
   |__ api.ts         -> handlers defined here
   |__ model.ts       -> io-ts encoders/decoders for the application models
   |__ config.ts
   |__ fixtures.ts    -> dummy data for /events and /discounts
   |__ events.ts      -> inmemory store of events
   |__ projections.ts -> events reducers
```

- **Events store**: this is far from realistic. Just sync setter (append) and
  getter of events. Ideally this would be async (with fp-ts `TaskEither`) and
  not in memory and replaced with a real event sourcing system.
- **Projections**: reducers of the events into calculated models. In this case
  only the cart. Does not do any validation of event history (e.g. "update item"
  event can be in the history without any "add item"). As with the _Events
  Store_, ideally this should be replaced with a real event sourcing system.
- **API**: the handlers are very simple and do not provide any error handling
  besides the one that fp-ts already includes (i.e. `Either`).
- **Functional programming**: For the sake of time I did not go into full FP
  approach (using IO/TaskEither for "fetching" data, using Reader for config,
  using IO for impure actions like `Date.now()`).
- **Produciton readiness**: the application misses monitoring, logging & tests.

With those caveats in mind, the architecture is simple yet I believe it can
scale very well for microservices.
