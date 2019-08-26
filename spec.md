## TimeWax

The goal is to have a starting point for a conversation, not a perfect algorithm
(if such a thing exists). We will look at how you think and how you communicate.
Keep in mind that there is no “right or wrong” way to implement it. Mistakes or
missing parts are fine. They can actually give an opening for an interesting
conversation during the evaluation.

### Challenge

Please implement the logic needed for an e-commerce shopping cart, using the
user stories below as guidance for the implementation.

### User stories

> As a customer I want a shopping cart so that I can order multiple items in a
> single transaction. An item is something with an
> [SKU](https://en.wikipedia.org/wiki/Stock_keeping_unit) (i.e. ID), display
> name and price.

**Acceptance criteria**:

- Every item (SKU) in the cart needs to have a quantity between 1 and 1000.
- Every **mutation** to the shopping cart needs to be **recorded as an "event"**
  with a datetime (a future user story could be about writing these mutations to
  e.g. Google Analytics, but for now this is out of scope!).
- Must be able to return a **list of items** with their **quantities** (as a
  materialized view or as is).
- Must be able to **add** and **remove** items.
- Must be able to **empty** the entire cart in **one operation**.
- Must be able to **change** the **quantity** of **individual items**.

---

> As a customer I want to be able to add a discount coupon so that I can receive
> my discount.

**Acceptance criteria**:

- We only support **fixed percentage** discounts (e.g. 10%), no special business
  rules about combinations, times of the week, minimum amount, et cetera.
- Discount codes **don't expire**.
- As there is no database it is acceptable to configure the available discount
  codes as part of the source code (i.e. hardcoded codes).
- Only **one discount** can be applied to the shopping cart **at a time**.

---

> As a customer I want to be able to see the accumulative value of the items in
> the shopping cart so that I know how much I need to pay when ordering.

**Acceptance criteria**:

- Take the optional discount into account (see the user story about discounts).
- Shipping costs are out of scope.
- There is **no minimum order** amount.

## Scope

The assignment is meant to be completable in **four hours or less**, so keep it
simple and approach it as an MVP or prototype implementation. Don't worry if you
didn't quite get there in the allotted time.

You can choose: either implement the front-end user interface of the shopping
cart or the business rules that can also run in the back-end. Of course, it is
fine if you implement both, but you don’t have to.

You have complete freedom when it comes to the programming language(s),
frameworks, libraries and what not. It doesn't have to be something that is
already part of our stack. You could decide to implement everything in Haskell
and use event sourcing as a data model or you could write everything in PHP. The
important part is that you are able to explain your choices (e.g. just picking
tools you're comfortable with or using that new programming language you always
wanted to try).

**Don't worry about these things**:

- The checkout process (including payment and handling customer information like
  a shipping address) is out of scope.
- Internationalization is out of scope (e.g. dealing with other timezones,
  currencies or languages).
- There is no minimum order.
- You can assume that there is only one shopping cart and only a single user.
- There are no shipping costs.
- You can assume that there is an unlimited amount in stock of every item.
- The data layer is out of scope (i.e. you can keep all the data in-memory and
  don't have to persist it to a database, for example).

While out of scope, issues like the above might be part of the conversation
after you've completed the assignment ("what if we were to add
internationalisation?").

## The evaluation

Please share the code on GitHub, BitBucket or similar before the appointment to
evaluate your project. During that meeting you will get to present your work and
we will dive deeper into the challenges you faced and choices you made. Please
make sure that you bring a laptop with a coding environment so that we can make
changes to the code if needed. It doesn't matter what editor you use. If you
really can't bring a laptop please provide full instructions for running the
code so we can set that up ahead of time.
