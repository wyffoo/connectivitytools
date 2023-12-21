'use strict';

angular.module('gigaApp.language-picker', [])
	.directive('languagePicker', function (Auth, $state, $translate, $localStorage) {
		return {
			scope: {},
			templateUrl: 'components/language-picker/language-picker.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.type = attrs.type;
				scope.stateIs = $state.is;
				scope.currentLanguage = $translate.use();
				scope.switchLanguage = function(language) {
					scope.currentLanguage = language;
					$localStorage.preferredLanguage = language;
					$translate.use(language);
					if (Auth.isLoggedIn()) {
						Auth.setLanguage(language);
					}
				};
			}
		};
	});
