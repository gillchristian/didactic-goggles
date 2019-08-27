import {Event} from './model'

const events: Event[] = []

export const appendEvent = (event: Event) => {
  events.push(event)
}

export const getEvents = () => events

export const getEventsOfUser = (desiredUserID: string) =>
  events.filter((event) => event.userID === desiredUserID)
