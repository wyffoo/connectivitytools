;(function (ng) {
	'use strict';

	ng.module('gigaApp')
		.directive('validateUsername', [function () {
				return {
					scope: true,
					require: 'ngModel',
					link: function postLink(scope, element, attrs, ctrl) {

						var regex = /^[A-Z][a-zA-Z0-9]{8,24}$/;

						var validate = function () {

							var stringValue = scope.$eval(attrs.ngModel);

							if (stringValue !== null) {
								return regex.test(stringValue);
							}

						};

						scope.$watch(validate, function (newValue) {
							ctrl.$setValidity('usernameIsNotValid', newValue);
						});

					}
				};
			}]
		);
}(window.angular));
