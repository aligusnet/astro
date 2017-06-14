const { execFile } = require('child_process');

exports.handle = function(event, context) {
    console.log('starting app with event', event);
    execFile('./main', ['--json', event], function(error, stdout) {
        console.log('app exited');
        console.log('error:', error);
        console.log('stdout:', stdout);
        context.done(error, JSON.parse(stdout));
    });
}
