const SpecReporter = require('jasmine-spec-reporter').SpecReporter;

jasmine.getEnv().clearReporters();   // remove default reporter logs
jasmine.getEnv().addReporter(new SpecReporter({
    suite: {
        displayNumber: false,
    },
    spec: {
        displayFailed: true,
        displaySuccessful: false,
        displayPending: true,
        displayDuration: true,
    }
}));
