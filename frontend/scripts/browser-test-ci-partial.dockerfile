FROM plazaproject/ci-base-frontend-browser:9ce3fd15744b80f959a0aad27ee22fe0cacef38d

# Change user to "tester". This will lift some restrictions on chromium
RUN adduser -D tester
USER tester
