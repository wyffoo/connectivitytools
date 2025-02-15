'use strict';

(function () {

	/**
	 * The Util service is for thin, globally reusable, utility functions
	 */
	function UtilService($window) {
		var Util = {
			/**
			 * Return a callback or noop function
			 *
			 * @param  {Function|*} cb - a 'potential' function
			 * @return {Function}
			 */
			safeCb(cb) {
				return (angular.isFunction(cb)) ? cb : angular.noop;
			},

			/**
			 * Parse a given url with the use of an anchor element
			 *
			 * @param  {String} url - the url to parse
			 * @return {Object}     - the parsed url, anchor element
			 */
			urlParse(url) {
				var a = document.createElement('a');
				a.href = url;

				// Special treatment for IE, see http://stackoverflow.com/a/13405933 for details
				if (a.host === '') {
					a.href = a.href;
				}

				return a;
			},

			/**
			 * Test whether or not a given url is same origin
			 *
			 * @param  {String}           url       - url to test
			 * @param  {String|String[]}  [origins] - additional origins to test against
			 * @return {Boolean}                    - true if url is same origin
			 */
			isSameOrigin(url, origins) {
				url = Util.urlParse(url);
				origins = (origins && [].concat(origins)) || [];
				origins = origins.map(Util.urlParse);
				origins.push($window.location);
				origins = origins.filter(function (o) {
					return url.hostname === o.hostname &&
						url.port === o.port &&
						url.protocol === o.protocol;
				});
				return (origins.length >= 1);
			},
			prefillParamsModel(params, model) {
				params.forEach((param) => {
					if (typeof param.value !== 'undefined') {
						model[param.id] = +param.value;//Convert value to number
					}
					if (param.type==='select') {
						model[param.id] = typeof param.value !== 'undefined'
							? param.value.toString()
							: param.default_value.toString();
					}
				});
			},
			cleanupParamsModel(params, model) {
				//Cleanup of default select values and nulls (we don't want to send them to server)
				params.forEach((param) => {
					if (param.type==='select' && model[param.id] === param.default_value.toString()) {
						delete model[param.id];
					}
					if (model[param.id] === null) {
						delete model[param.id];
					}
				});
			}
		};

		return Util;
	}

	angular.module('gigaApp.util')
		.factory('Util', UtilService);

})();
