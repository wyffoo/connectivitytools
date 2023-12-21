;(function (ng) {
	'use strict';

	ng.module('gigaApp')
		.controller('LoginController', [
			'$scope', '$state', '$timeout', 'Auth', '$translate', '$localStorage',
			function ($scope, $state, $timeout, Auth, $translate, $localStorage) {
				let translations = {};
				$translate([
                    'login.general_error',
                    'login.send',
                    'login.loading',
                    'login.welcome2',
                    'login.checklogin'
				]).then((result)=>{
					translations = result;
					$scope.btLogin = translations['login.send'];
				});
				$scope.login = {
					email: null,
					password: null
				};
				$scope.loading = false;
				$scope.alerts = [];
				$scope.btLogin = '';

				// Function for logout
				$scope.logout = function () {//TODO: move to directive
					disableForm();
					auth.userLogout().then(function () {
						$state.go('home');
					}, function (res) {
						enableForm();
                        $scope.addAlert('danger', translations['login.general_error']);
					});
				};

				let enableForm = function () {
					$scope.loading = false;
					$scope.btLogin = translations['login.send'];
				};

				let disableForm = function () {
					$scope.loading = true;
					$scope.btLogin = translations['login.loading'];
				};

				// Function for login
				$scope.submitFormLogin = function () {
					disableForm();
					Auth.login({
						email: $scope.login.email,
						password: $scope.login.password
					}).then(function (user) {
						$localStorage.SchoolWizardModel = null;//Clean school wizard model
						$scope.addAlert('success', translations['login.welcome2'] + user.firstname + ' ' + user.lastname);
						$state.go('admin-projects-list');
					}, function (res) {
						enableForm();
						$scope.addAlert('danger', translations['login.checklogin']);
					});
				};

				$scope.addAlert = function (type, message) {

					let icone = '';

					if (type === 'danger') {
						icone = 'glyphicon glyphicon-exclamation-sign';
					} else {
						icone = 'glyphicon glyphicon-ok-sign';
					}

					$scope.alerts = [{
						'type': type,
						'msg': message,
						'icone': icone
					}];

				};

				$scope.closeAlert = function () {
					$scope.alerts.splice(0, 1);
				};
			}
		]);
}(window.angular));
