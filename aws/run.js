const spawn = require('child_process').spawn;
var exec = require('child_process').exec;

exports.handle = function(event, context) {
    exec('./main --json="'+ JSON.stringify(event) + '"', function(error, stdout) {
        context.done(error, JSON.parse(stdout));
    });
}
