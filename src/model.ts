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

export const MutationType = {
  AddItem: 'AddItem',
  UpdateItem: 'UpdateItem',
  DeleteItem: 'DeleteItem',
  EmptyCart: 'EmptyCart',
  ApplyDiscount: 'ApplyDiscount',
} as const

const AddItem = t.type(
  {
    type: t.literal(MutationType.AddItem),
    sku: t.string,
    count: t.number,
  },
  MutationType.AddItem,
)

const UpdateItem = t.type(
  {
    type: t.literal(MutationType.UpdateItem),
    sku: t.string,
    count: t.number,
  },
  MutationType.UpdateItem,
)

const DeleteItem = t.type(
  {
    type: t.literal(MutationType.DeleteItem),
    sku: t.string,
  },
  MutationType.DeleteItem,
)

const EmptyCart = t.type(
  {
    type: t.literal(MutationType.EmptyCart),
  },
  MutationType.EmptyCart,
)

const ApplyDiscount = t.type(
  {
    type: t.literal(MutationType.ApplyDiscount),
    code: t.string,
  },
  MutationType.ApplyDiscount,
)

export const Mutation = t.taggedUnion(
  'type',
  [AddItem, UpdateItem, DeleteItem, EmptyCart, ApplyDiscount],
  'Mutation',
)

export type Mutation = t.TypeOf<typeof Mutation>

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
