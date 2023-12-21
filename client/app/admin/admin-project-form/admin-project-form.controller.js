'use strict';

(function () {

	class AdminProjectFormComponent {
		//start-non-standard
		model = {
			country_id: null,
			name: '',
			description: '',
			steps: [],
			progress:{"active":0,"status":['pristine','pristine','pristine','pristine','pristine','pristine','pristine','pristine']}
		};

		//end-non-standard


		constructor($scope, $stateParams, $state, $translate, $localStorage) {
			this.translations = {};
			$translate([]).then((result)=>{
				this.translations = result;
			});
			this.scope = $scope;
			this.state = $state;
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			if ($stateParams.project === 'new') {
				$localStorage.SchoolWizardModel = {
					country_id: null,
					name: '',
					description: '',
					steps: [],
					progress:{"active":0,"status":['pristine','pristine','pristine','pristine','pristine','pristine','pristine','pristine']}
				};
			}
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 0;
		}

		save() {
			if (this.scope.newProjectForm.$invalid) {
				return;
			}
			this.model.progress.status[0] = 'completed';
			this.state.go('multischool-wizard-broadband-initial');
		}
	}

	angular.module('gigaApp.admin')
		.component('adminProjectForm', {
			templateUrl: 'app/admin/admin-project-form/admin-project-form.html',
			controller: AdminProjectFormComponent
		});

})();
