import dotenv from 'dotenv'

dotenv.config()

import express from 'express'
import cors from 'cors'
import morgan from 'morgan'
import {toRequestHandler} from 'hyper-ts/lib/express'

import conf from './config'

import {hello, items, discounts} from './api'

const app = express()

app.use(cors())
app.use(morgan('tiny'))

app.get('/', toRequestHandler(hello))
app.get('/items', toRequestHandler(items))
app.get('/discounts', toRequestHandler(discounts))

console.log('Starting server with following config:')
console.log(JSON.stringify(conf, null, 2))

app.listen(conf.port, () => {
  console.log(`Express listening on port ${conf.port}`)
})