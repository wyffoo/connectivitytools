'use strict';

(function () {

	function UserResource($resource, $localStorage) {
		var api = $resource('/api/users/:id/:controller', {
		//var api = $resource('/assets/mocks/users/:id/:controller.json', {
			id: '@id'
		}, {
			changePassword: {
				method: 'PUT',
				params: {
					controller: 'password'
				}
			},
			setLanguage: {
				method: 'POST',
				params: {
					controller: 'setLanguage'
				}
			},
			verifyEmailIsRegistered: {
				method: 'POST',
				params: {
					controller: 'verifyEmailIsRegistered'
				}
			},
			signup: {
				method: 'POST',
				params: {
					controller: 'signup'
				}
			},
			signout: {
				method: 'POST',
				params: {
					controller: 'signout'
				}
			},
			passwordreset: {
				method: 'POST',
				params: {
					controller: 'passwordreset'
				}
			},
			get: {
				method: 'GET',
				params: {
					id: 'me'
				}
			}
		});

		if (!$localStorage.userData) {
			$localStorage.userData = {};
		}

		return angular.extend(api, {
			local: $localStorage.userData
		});
	}

	angular.module('gigaApp.auth')
		.factory('User', UserResource);

})();
