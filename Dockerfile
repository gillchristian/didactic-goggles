FROM node:11.15-alpine

WORKDIR /app

ADD yarn.lock /app/yarn.lock
ADD package.json /app/package.json

ENV NODE_PATH=/app/node_modules
ENV PATH=$PATH:/app/node_modules/.bin

RUN yarn

ADD . /app

RUN yarn build

CMD ["yarn", "start"]
