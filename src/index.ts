import dotenv from 'dotenv'

dotenv.config()

import express from 'express'
import cors from 'cors'
import morgan from 'morgan'
import bodyParser from 'body-parser'
import {toRequestHandler} from 'hyper-ts/lib/express'

import conf from './config'

import * as handlers from './api'

const app = express()

app.use(cors())
app.use(morgan('tiny'))

app.use(bodyParser.json())
app.use(bodyParser.urlencoded({extended: true}))

app.get('/', toRequestHandler(handlers.hello))
app.get('/items', toRequestHandler(handlers.listItems))
app.get('/discounts', toRequestHandler(handlers.listDiscounts))

app.get('/user/:user_id/cart', toRequestHandler(handlers.showUserCart))
app.put('/user/:user_id/cart', toRequestHandler(handlers.cartMutation))

app.get('/events', toRequestHandler(handlers.listEvents))
app.get('/events/:user_id', toRequestHandler(handlers.listUserEvents))

console.log('Starting server with following config:')
console.log(JSON.stringify(conf, null, 2))

app.listen(conf.port, () => {
  console.log(`Express listening on port ${conf.port}`)
})
