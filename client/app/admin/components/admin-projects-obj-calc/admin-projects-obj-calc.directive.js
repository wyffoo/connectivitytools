'use strict';

angular.module('gigaApp.admin')
	.directive('adminProjectsObjCalc', function () {
		return {
			scope: {
				projectFieldTitle: '=',
				results: '=',
				projects: '=',
				selectedProject: '=',
				objects: '=',
				selectedObject: '=',
				onDownloadReport: '&onDownloadReport'
			},
			templateUrl: 'app/admin/components/admin-projects-obj-calc/admin-projects-obj-calc.html',
			restrict: 'EA',
			link: function (scope, element, attrs) {
				scope.report = () => {
					scope.onDownloadReport();
				};
			}
		};
	});
