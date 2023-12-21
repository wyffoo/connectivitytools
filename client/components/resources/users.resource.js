'use strict';

(function () {

	function UsersResource($resource, $localStorage) {
		let api = $resource('/api/users/:action', {}, {
			'invite': { method:'POST', params: {action: 'sendInvite'} }
		});

		return api;
	}

	angular.module('gigaApp')
		.factory('UsersResource', UsersResource);

})();
