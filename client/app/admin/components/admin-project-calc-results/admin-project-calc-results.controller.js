'use strict';

function adminProjectCalcResultsController($scope, $uibModalInstance, results, project, object, onDownloadReport) {
	$scope.results = results.data;
	$scope.project = project;
	$scope.object = object;
	$scope.onDownloadReport = onDownloadReport;

	$scope.dismiss = function () {
		$uibModalInstance.dismiss('cancel');
	};
}

angular.module('gigaApp.admin')
	.controller('adminProjectCalcResultsController', adminProjectCalcResultsController);
