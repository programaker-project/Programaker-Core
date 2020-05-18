FROM plazaproject/ci-base-frontend-browser:410be9260b3be635dfcceb5c85aed7173aca649f

# Change user to "tester". This will lift some restrictions on chromium
RUN adduser -D tester
USER tester
