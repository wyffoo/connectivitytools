'use strict';

angular.module('gigaApp.auth', [
	'gigaApp.constants',
	'gigaApp.util',
	'ngCookies',
	'ui.router'
])
	.config(function ($httpProvider) {
		$httpProvider.interceptors.push('authInterceptor');
	});

angular.module('gigaApp.auth').config([
	'$provide',
	function($provide) {
		$provide.decorator('$http', [
			'$delegate', '$cacheFactory', '$timeout',
			function($delegate, $cacheFactory, $timeout) {

				//Create cache object
				var customCache = $cacheFactory('customCache');

				//copy old function for further usage
				var oldGetFunc = angular.copy($delegate.get);

				//Our magic
				function preventDuplicate(url, config) {

					//Create unique identifier for our request
					//TODO: PROBABLY WE ALSO NEED TO TAKE INTO CONSIDERATION HEADERS
					var confString = null;
					if (typeof config == 'object') {
						confString = JSON.stringify(config);
					}

					var requestKey = url + confString;

					//Cache request for one second as this is probably enough to prevent duplicate requests
					var requestPromise = customCache.get(requestKey);
					if (!requestPromise) {
						requestPromise = oldGetFunc(url, config);
						customCache.put(requestKey, requestPromise);
						$timeout(function() {
							customCache.remove(requestKey);
						}, 1000);
					}

					return requestPromise;
				}

				//Change default behavior of GET.
				//Probably it is not good idea to prevent duplicates for other methods
				$delegate.get = function(url, config) {
					return preventDuplicate(url, config)
				};

				return $delegate;
			}
		]);
	}
]);
