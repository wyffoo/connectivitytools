'use strict';

function adminProjectTechProviderParamsController($scope, $uibModalInstance, Util, params, technology, onSubmit) {
	$scope.params = params.data;
	$scope.technology = technology;
	$scope.model = {};
	$scope.onSubmit = onSubmit;
	$scope.progress = false;
	$scope.errors = {};

	Util.prefillParamsModel($scope.params, $scope.model);

	$scope.resetForm = function () {
		$scope.errors = {};
		$scope.form.$setPristine();
		$scope.progress = false;
	};

	$scope.ok = function () {
		$scope.form.$setSubmitted(true);
		if ($scope.form.$invalid) return;

		$scope.progress = true;

		Util.cleanupParamsModel($scope.params, $scope.model);

		$scope.onSubmit($scope.model).then(() => {
			$scope.resetForm();
			$uibModalInstance.close();
		}, (err) => {
			err = err.data;
			$scope.resetForm();
			// Update validity of form fields that match the mongoose errors
			angular.forEach(err.errors, (error, field) => {
				$scope.form[field].$setValidity('mongoose', false);
				$scope.errors[field] = error.message;
			});
		});
	};

	$scope.cancel = function () {
		$uibModalInstance.dismiss('cancel');
	};
}

angular.module('gigaApp.admin')
	.controller('adminProjectTechProviderParamsController', adminProjectTechProviderParamsController);
