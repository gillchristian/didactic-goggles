const env = process.env.NODE_ENV || 'development'

const conf = {
  port: process.env.PORT || '8080',
  isProd: env === 'production',
  env,
}

export default conf
