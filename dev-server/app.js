/**
 * Main application file
 */

'use strict';

import express from 'express';
import config from './config/environment';
import http from 'http';

// Setup server
var proxy = require('express-http-proxy');
var app = express();
var server = http.createServer(app);
require('./config/express').default(app);


// New hostname+path as specified by question:
var apiProxy = proxy('127.0.0.1:8080', {
	proxyReqPathResolver: function (req, res) {
		return req.originalUrl;
	}
});

//Route API requests to php
app.use("/api/*", apiProxy);

// Start server
function startServer() {
	app.angularFullstack = server.listen(config.port, config.ip, function () {
		console.log('Express server listening on %d, in %s mode', config.port, app.get('env'));
	});
}

setImmediate(startServer);

// Expose app
exports = module.exports = app;
