const SpecReporter = require('jasmine-spec-reporter').SpecReporter;

jasmine.getEnv().clearReporters();   // remove default reporter logs
jasmine.getEnv().addReporter(new SpecReporter({
    suite: {
        displayNumber: false,
    },
    spec: {
        displayFailed: true,
        displaySuccessful: true,
        displayPending: true,
        displayDuration: true,
        displayStacktrace: false,
    },
    summary: {
        displayErrorMessages: true,
        displayStacktrace: true,
    }

}));
