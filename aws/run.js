const exec = require('child_process').exec;

exports.handle = function(event, context) {
    console.log('starting app with event', event);
    exec('./main --json="'+ JSON.stringify(event) + '"', function(error, stdout) {
        console.log('app exited');
        console.log('error:', error);
        console.log('stdout:', stdout);
        context.done(error, JSON.parse(stdout));
    });
}
