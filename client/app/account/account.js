'use strict';

angular.module('gigaApp')
	.config(function ($stateProvider) {
		$stateProvider
			.state('login', {
				url: '/login',
				templateUrl: 'app/account/login/login.html',
				controller: 'LoginController',
				controllerAs: 'vm'
			})
			.state('logout', {
				url: '/logout?referrer',
				referrer: 'main',
				template: '',
				controller: function ($state, Auth) {
					var referrer = $state.params.referrer ||
						$state.current.referrer ||
						'main';
					Auth.logout();
					$state.go(referrer);
				}
			})
			.state('forgetpassword', {
				url: '/forgetpassword',
				templateUrl: 'app/account/forgetpassword/forgetpassword.html',
				controller: 'ForgetPasswordController',
				controllerAs: 'vm'
			})		
			.state('signup', {
				url: '/signup/:role',
				templateUrl: 'app/account/signup/signup.html',
				controller: 'SignupController',
				controllerAs: 'vm'
			})
			.state('settings', {
				url: '/settings',
				templateUrl: 'app/account/settings/settings.html',
				controller: 'SettingsController',
				controllerAs: 'vm',
				authenticate: true
			});
	})
	.run(function ($rootScope, $translatePartialLoader){
		$translatePartialLoader.addPart('account');

		$rootScope.$on('$stateChangeStart', function (event, next, nextParams, current) {
			if (next.name === 'logout' && current && current.name && !current.authenticate) {
				next.referrer = current.name;
			}
		});
	});
