'use strict';

angular.module('gigaApp.admin')
	.directive('adminProjectStatus', function () {
		return {
			scope: {
				status:'=',
				progress:'='
			},
			templateUrl: 'app/admin/components/admin-project-status/admin-project-status.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.showtext = typeof attrs.showtext !== 'undefined';
			}
		};
	});
