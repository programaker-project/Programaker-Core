FROM programakerproject/ci-base-frontend-browser:50e8c3f1a1fb0e7d24b7a42cf77421764e0f27e3

# Change user to "tester". This will lift some restrictions on chromium
RUN adduser -D tester
USER tester
