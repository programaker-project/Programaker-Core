FROM programakerproject/ci-base-frontend-browser:b0fcf3b17c87060f72aae4407c9789925bcfd668

# Change user to "tester". This will lift some restrictions on chromium
RUN adduser -D tester
USER tester
