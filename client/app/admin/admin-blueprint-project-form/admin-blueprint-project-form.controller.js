'use strict';

(function () {

	class AdminBlueprintProjectFormComponent {
		//start-non-standard
		model = {
			country_id: null,
			name: '',
			description: '',
			steps: [],
			is_net_in_output: true,
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
					description: ''
				};
			}
			this.model = $localStorage.SchoolWizardModel;
		}

		save() {
			if (this.scope.newProjectForm.$invalid) {
				return;
			}
			this.state.go('blueprint-wizard-review');
		}
	}

	angular.module('gigaApp.admin')
		.component('adminBlueprintProjectForm', {
			templateUrl: 'app/admin/admin-blueprint-project-form/admin-blueprint-project-form.html',
			controller: AdminBlueprintProjectFormComponent
		});

})();
