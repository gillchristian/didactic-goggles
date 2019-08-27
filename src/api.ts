import * as t from 'io-ts'
import {
  Middleware,
  of,
  decodeParam,
  decodeBody,
  Status,
  status,
  StatusOpen,
} from 'hyper-ts'

import {Cart, Event, Mutation} from './model'
import * as events from './events'
import * as projections from './projections'

import * as fixtures from './fixtures'

const sendJson = (data: unknown) =>
  status(Status.OK)
    .header('Content-Type', 'application/json; charset=utf-8')
    .closeHeaders()
    .send(
      JSON.stringify({
        data,
        error: false,
        count: Array.isArray(data) ? data.length : undefined,
      }),
    )

const userIdParam = decodeParam('user_id', t.string.decode)

const loadEvents = (): Middleware<StatusOpen, StatusOpen, never, Event[]> =>
  of(events.getEvents())

const loadUserEvents = (
  userId: string,
): Middleware<StatusOpen, StatusOpen, never, Event[]> =>
  of(events.getEventsOfUser(userId))

// const parseMutation = decodeBody(t.type({mutation: Mutation}).decode)
// TODO: replace with ^^^
// This is a temporary fix: `Mutation.decode` doesn't handle the properties properly
// it succeds with Right({ type: undefined, sku: undefined, count: undefined })
const parseMutation = decodeBody((input: any) =>
  t
    .type({mutation: Mutation})
    .decode(input)
    .map((body) => ({mutation: input.mutation})),
)

interface MutationBody {
  mutation: Mutation
}

const eventOfIdAndMutation = (body: MutationBody) =>
  userIdParam.map((userID: string) => ({
    time: Date.now(),
    mutation: body.mutation,
    userID,
  }))

const saveEvent = (
  event: Event,
): Middleware<StatusOpen, StatusOpen, never, string> => {
  events.appendEvent(event)

  return of(event.userID)
}

const loadCart = (
  userID: string,
): Middleware<StatusOpen, StatusOpen, never, Cart> =>
  of(
    projections.cart(fixtures.items, fixtures.discounts)(
      events.getEventsOfUser(userID),
    ),
  )

// --- handlers

export const hello = status(Status.OK)
  .closeHeaders()
  .send('Welcome to the Cart API')

export const listItems = sendJson(fixtures.items)

export const listDiscounts = sendJson(fixtures.discounts)

export const showUserCart = userIdParam.ichain(loadCart).ichain(sendJson)

export const cartMutation = parseMutation
  .ichain(eventOfIdAndMutation)
  .ichain(saveEvent)
  .ichain(loadCart)
  .ichain(sendJson)

export const listEvents = of<StatusOpen, never, void>(undefined)
  .ichain(loadEvents)
  .ichain(sendJson)

export const listUserEvents = userIdParam
  .ichain(loadUserEvents)
  .ichain(sendJson)
