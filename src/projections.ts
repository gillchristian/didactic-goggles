import {MutationType as MT, Event, Cart, Item, Discount} from './model'

// totalPrice: t.number,
// items: t.record(t.string, t.union([t.number, t.undefined])),
// discount: t.union([t.string, t.undefined]),

const between = (from: number, to: number) => (x: number) => {
  if (x > to) return to
  if (x < from) return from
  return x
}

const itemRange = between(0, 1000)

const EMPTY_CART: Cart = {totalPrice: 0, items: {}, discount: undefined}

export const cart = (items: Item[], discounts: Discount[]) => (
  events: Event[],
): Cart => {
  const cartProjection: Cart = events.reduce((accCart, {mutation}) => {
    switch (mutation.type) {
      case MT.AddItem:
      case MT.UpdateItem: {
        const {count} = mutation
        const item = accCart.items[mutation.sku]

        accCart.items[mutation.sku] = itemRange(item ? item + count : count)

        return accCart
      }

      case MT.DeleteItem: {
        accCart.items[mutation.sku] = undefined

        return accCart
      }

      case MT.EmptyCart: {
        return EMPTY_CART
      }

      case MT.ApplyDiscount: {
        return {...accCart, discount: mutation.code}
      }
    }
  }, EMPTY_CART)

  const totalPrice = Object.entries(cartProjection.items).reduce(
    (acc, [sku, count]) => {
      const item = items.find((item) => item.sku === sku)

      return item && count ? acc + item.price * count : acc
    },
    0,
  )

  const discount = discounts.find(
    (discount) => discount.code === cartProjection.discount,
  )

  return {
    ...cartProjection,
    totalPrice: discount
      ? totalPrice * (discount.percentage / 100)
      : totalPrice,
  }
}
