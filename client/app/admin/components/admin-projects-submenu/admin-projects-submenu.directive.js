'use strict';

angular.module('gigaApp.admin')
	.directive('adminProjectsSubmenu', function (appConfig) {
		return {
			templateUrl: 'app/admin/components/admin-projects-submenu/admin-projects-submenu.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.isCountriesMode = appConfig.isCountriesMode;
				scope.isSchoolsMode = appConfig.isSchoolsMode;
				scope.isGlobalMode = appConfig.isGlobalMode;
			}
		};
	});
