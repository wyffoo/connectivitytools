'use strict';

(function () {

	function LocationsResource($resource, $localStorage) {
		let api = $resource('/api/locations/:id', {
			id: '@id'
		}, {
			'update': { method:'PUT' },
		});

		return api;
	}

	angular.module('gigaApp')
		.factory('LocationsResource', LocationsResource);

})();
