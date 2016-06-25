FROM pbrisbin/heroku-haskell-stack:1.0.2

COPY src /app/user/src
COPY static /app/user/static
RUN stack install