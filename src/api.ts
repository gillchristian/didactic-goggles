import {Status, status} from 'hyper-ts'

export const hello = status(Status.OK)
  .closeHeaders()
  .send('Welcome to the Cart API')
