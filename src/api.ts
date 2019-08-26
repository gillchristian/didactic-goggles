import {Status, status} from 'hyper-ts'

import * as fixtures from './fixtures'

export const hello = status(Status.OK)
  .closeHeaders()
  .send('Welcome to the Cart API')

export const discounts = status(Status.OK).json(
  {data: fixtures.discounts, error: false},
  () => 'Failed to encode body',
)

export const items = status(Status.OK).json(
  {data: fixtures.items, error: false},
  () => 'Failed to encode body',
)
