'use strict';

function adminProjectTechParamsController($scope, $http, $uibModalInstance, Modal, Util, project, defaultParams, params, technology, onSubmit) {
	$scope.defaultParams = defaultParams;
	let customParams = params.data;
	customParams.forEach((cp) => {
		customParams[cp.id] = cp.value;
	});
	$scope.params = [];
	$scope.defaultParams.forEach((dp) => {
		dp.value = customParams[dp.id];
		$scope.params.push(dp);
	});

	$scope.project = project;
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

	$scope.editDependencies = function (techParam) {
		let modalInstance = Modal.$uibModal.open({
			animation: true,
			keyboard: false,
			backdrop: 'static',
			templateUrl: 'app/admin/components/admin-project-tech-dependency/admin-project-tech-dependency.html',
			controller: 'AdminProjectTechDependencyController',
			size: 'lg',
			resolve: {
				techParam: () => {
					return techParam;
				},
				objectParams: () => {
					return $http({method: 'GET', url: 'assets/mocks/param_dependencies.json'});
				},
				onSubmit: () => {
					return (dependencies) => {
						return $scope.saveDependencies($scope.project.id, $scope.technology.id, techParam.id, dependencies);
					}
				}
			}
		});
	};

	//TODO: move to parent controller
	$scope.saveDependencies = function (projectId, technologyId, techParamId, dependencies) {
		//console.log(projectId, technologyId, techParamId, dependencies);

		return $http({method: 'GET', url: '/'});
	};

	$scope.ok = function () {
		$scope.form.$setSubmitted(true);
		if ($scope.form.$invalid) return;

		$scope.progress = true;

		Util.cleanupParamsModel($scope.params, $scope.model);

		$scope.onSubmit($scope.model).then(() => {
			$scope.resetForm();
			$uibModalInstance.close($scope.items);
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
	.controller('adminProjectTechParamsController', adminProjectTechParamsController);
