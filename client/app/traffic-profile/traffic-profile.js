'use strict';

angular.module('gigaApp.traffic-profile')
	.config(function ($stateProvider) {
		$stateProvider
			.state('traffic-profile-form', {
				params: {
					profile_id: null
				},
				url: '/traffic-profile/form/:profile_id',
				template: '<traffic-profile-form></traffic-profile-form>',
				authenticate: 'admin',
				resolve: {
					loadTrafficSources: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/tp/traffic_sources'});
					},
					loadServices: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/tp/services'});
					},
					loadRecords: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/tp/records/'+$stateParams.profile_id});
					},
					loadProfile: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/tp/profile/'+$stateParams.profile_id});
					}
				}
			})
	});
