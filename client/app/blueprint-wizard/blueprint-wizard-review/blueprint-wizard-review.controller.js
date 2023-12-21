'use strict';

(function () {

	class BlueprintWizardReviewController {

		model = {};

		constructor($scope, $stateParams, $state, $filter, ProjectsResource, $translate, Modal, $localStorage, appConfig) {
			this.translations = {};
			$translate([]).then((result) => {
				this.translations = result;
			});
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			this.model = $localStorage.SchoolWizardModel;
			this.model.isCountriesMode = appConfig.isCountriesMode;
			this.model.isGlobalMode = appConfig.isGlobalMode;
			this.ProjectsResource = ProjectsResource;
			this.localStorage = $localStorage;
		}

		saveProject() {
			//Add new project
			let project = new this.ProjectsResource(this.model);
			this.scope.isLoading = true;
			project.$save((resp)=>{
				if (resp.status === 'success') {
					this.localStorage.SchoolWizardModel = {};
					this.scope.isLoading = false;
					this.state.go('admin-projects-list');
				}
			}, ()=>{
				this.scope.isLoading = false;
			});
		}

		back() {
			this.state.go('admin-blueprint-project-form');
		}
	}

	angular.module('gigaApp.blueprint-wizard')
		.component('blueprintWizardReview', {
			templateUrl: 'app/blueprint-wizard/blueprint-wizard-review/blueprint-wizard-review.html',
			controller: BlueprintWizardReviewController
		});

})();
