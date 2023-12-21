'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('object-params', {
				url: '/object-params/:objectId',
				authenticate: true,
				params: {
					objects: []
				},
				template: '<object-params></object-params>',
				resolve: {
					loadObjectParams: function ($http, $stateParams) {
						// $http returns a promise for the url data
						if ($stateParams.objects) {//Multi-object, defaults only
							return $http({method: 'GET', url: '/api/variables/class/object'});
						} else if ($stateParams.objectId) {
							return $http({method: 'GET', url: '/api/variables/object/' + $stateParams.objectId});
						}

					}
				}
			});
	});
