'use strict';

(function () {

	class MultischoolWizardReviewController {

		model = {};

		constructor($scope, $stateParams, $state, $filter, ProjectsResource, $translate, Modal, $localStorage) {
			this.translations = {};
			$translate([]).then((result) => {
				this.translations = result;
			});
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			this.template = $scope.$parent.$resolve.loadTemplateInfo.data;
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 7;
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
			if (this.model.is_lan_required) {
				this.state.go('multischool-wizard-lan-configuration');
			} else if (!this.model.is_lan_required && this.model.calculate_only_required_bandwidth) {
				this.state.go('multischool-wizard-broadband-tp');
			} else if (!this.model.is_lan_required && !this.model.calculate_only_required_bandwidth) {
				this.state.go('multischool-wizard-lan-initial');
			} else if (!this.model.net_in_output_optimization && this.model.is_bandwidth_calc_required === 'calculate') {
				this.state.go('multischool-wizard-broadband-bandwidth-selector');
			} else {
				this.state.go('multischool-wizard-lan-initial');
			}
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardReview', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-review/multischool-wizard-review.html',
			controller: MultischoolWizardReviewController
		});

})();
