;(function (ng) {
	'use strict';

	ng.module('gigaApp')
		.controller('SignupController', [
			'$scope', '$location', '$stateParams', 'Auth', '$translate',
			function ($scope, $location, $stateParams, Auth, $translate) {
				let translations = {};
				$translate([
                    'signup.send',
                    'signup.loading',
                    'signup.error'
				]).then((result)=>{
					translations = result;
					$scope.btSignUp = translations['signup.send'];
				});
				$scope.recaptchaKey = '6LcQ3FIaAAAAAEIT6L4aCwk_WJEmJJNiI7pFgjtl';
				$scope.recaptchaResponse = null;
				$scope.setRecaptchaResponse = function (response) {
					$scope.recaptchaResponse = response;
				};
				$scope.signUp = {
					'firstname': null,
					'lastname': null,
					'email': null,
					'password': null,
					'confirmPassword': null,
					'recaptchaToken': null,
					'role': $stateParams.role ? $stateParams.role : null
				};
				$scope.alerts = [];
				$scope.loading = false;

				let onlineCheckTimeout;

				// Watch email for validate
				$scope.$watchCollection(
					'signUp',
					function (newValue, oldValue) {
						if (!angular.isUndefined($scope.signUpForm)) {
							// for email
							if ($scope.signUpForm.email.$dirty) {
								if (!angular.isUndefined(newValue.email) && newValue.email !== oldValue.email) {
									clearTimeout(onlineCheckTimeout);
									onlineCheckTimeout = setTimeout(function () {
										$scope.signUpForm.email.$invalid = false;
										$scope.signUpForm.email.$error.onlineCheckWasDone = false;
										Auth.verifyEmailIsRegistered(newValue.email).then(function (res) {
											$scope.signUpForm.email.$error.onlineCheckWasDone = true;
											$scope.signUpForm.email.$error.emailCheckError = false;
											$scope.signUpForm.email.$error.emailAlreadyRegistered = res.status !== 'available';
										}, function (res) {
											$scope.signUpForm.email.$error.onlineCheckWasDone = true;
											$scope.signUpForm.email.$error.emailCheckError = true;
										});
									}, 1000);
								}

							}

						}
					}
				);

				let enableForm = function () {
					$scope.loading = false;
					$scope.btLogin = translations['signup.send'];
				};

				let disableForm = function () {
					$scope.loading = true;
					$scope.btLogin = translations['signup.loading'];
				};

				/*
				 * Submit form SignUp
				 */
				$scope.submitFormSignUp = function () {
					disableForm();
					$scope.signUp.recaptchaToken = $scope.recaptchaResponse;
					Auth.createUser($scope.signUp).then(function (res) {
						$location.path('main');
					}, function (res) {
						enableForm();
						$scope.addAlert('danger', translations['signup.error']);
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
