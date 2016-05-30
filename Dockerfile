FROM alpine:3.3

COPY .dist/bbcheck bbcheck
COPY service.prod.cfg service.cfg

ENV PORT 3000

CMD CONFIG_FILE=service.cfg ./bbcheck
