import {Discount, Item} from './model'

export const discounts: Discount[] = [
  {code: 'discount-qwe', percentage: 5},
  {code: 'discount-123', percentage: 10},
  {code: 'discount-asd', percentage: 15},
  {code: 'discount-456', percentage: 20},
  {code: 'discount-jkl', percentage: 25},
  {code: 'discount-678', percentage: 30},
]

export const items: Item[] = [
  {sku: 'sku-qwe-123-123-qwe', price: 10.09, displayName: 'iPhone Case'},
  {sku: 'sku-123-qwe-678-jkl', price: 34.89, displayName: 'Amazon Echo'},
  {sku: 'sku-asd-456-qwe-123', price: 84.54, displayName: 'Kindle Paperwhite'},
  {sku: 'sku-456-asd-asd-456', price: 45, displayName: "Vans Men's Ward Suede"},
  {sku: 'sku-jkl-678-jkl-678', price: 50, displayName: 'Fitbit Versa Lite'},
  {sku: 'sku-678-jkl-456-asd', price: 479, displayName: 'Apple Watch Series 4'},
  {sku: 'sku-qwe-123-908-qwe', price: 149, displayName: 'HUAWEI MediaPad T5'},
  {sku: 'sku-123-klj-798-rop', price: 13.99, displayName: 'SHO Bottle'},
  {sku: 'sku-aam-389-qwe-811', price: 3.99, displayName: 'Groot small model'},
  {sku: 'sku-419-asd-mna-367', price: 45, displayName: "Vans Men's Ward Suede"},
  {sku: 'sku-jlm-713-jbl-678', price: 5.75, displayName: 'Men Short Sleeve'},
  {sku: 'sku-678-jkl-266-wql', price: 1.09, displayName: 'Sanwood earring'},
]
