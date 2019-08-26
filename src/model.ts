import * as t from 'io-ts'

export const Item = t.type(
  {
    sku: t.string,
    price: t.number,
    displayName: t.string,
  },
  'Item',
)

export type Item = t.TypeOf<typeof Item>

export const Discount = t.type(
  {
    code: t.string,
    percentage: t.number,
  },
  'Discount',
)

export type Discount = t.TypeOf<typeof Discount>

const AddItem = t.type(
  {
    type: t.literal('AddItem'),
    sku: t.string,
    count: t.number,
  },
  'AddItem',
)

const UpdateItem = t.type(
  {
    type: t.literal('UpdateItem'),
    sku: t.string,
    count: t.number,
  },
  'UpdateItem',
)

const DeleteItem = t.type(
  {
    type: t.literal('DeleteItem'),
    sku: t.string,
  },
  'DeleteItem',
)

const ApplyDiscount = t.type(
  {
    type: t.literal('ApplyDiscount'),
    code: t.string,
  },
  'ApplyDiscount',
)

const EmptyCart = t.type(
  {
    type: t.literal('EmptyCart'),
    sku: t.string,
    count: t.number,
  },
  'EmptyCart',
)

export const Mutation = t.taggedUnion(
  'type',
  [AddItem, UpdateItem, DeleteItem, ApplyDiscount, EmptyCart],
  'Mutation',
)

export const Cart = t.type(
  {
    totalPrice: t.number,
    items: t.record(t.string, t.union([t.number, t.undefined])),
    discount: t.union([t.string, t.undefined]),
  },
  'Cart',
)

export type Cart = t.TypeOf<typeof Cart>

export const Event = t.type({
  time: t.number, // Unix timestamp
  userID: t.string,
  mutation: Mutation,
})

export type Event = t.TypeOf<typeof Event>
