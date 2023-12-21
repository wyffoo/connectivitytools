'use strict';

function adminProjectTechDependencyController($scope, $uibModalInstance, techParam, objectParams, onSubmit) {
	$scope.techParam = techParam;
	$scope.model = objectParams.data;
	$scope.onSubmit = onSubmit;
	$scope.progress = false;

	$scope.ok = function () {
		$scope.progress = true;
		$scope.onSubmit($scope.model).then(() => {
			$uibModalInstance.close();
		}, (err) => {

		});
	};

	$scope.dismiss = function () {
		$uibModalInstance.dismiss('cancel');
	};
}

angular.module('gigaApp.admin')
	.controller('AdminProjectTechDependencyController', adminProjectTechDependencyController);
