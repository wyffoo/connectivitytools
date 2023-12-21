'use strict';

(function () {

	class SchoolWizardLanConfigurationController {
		model = {
			lan_conf: {wired: false, wireless: false, public_hotspot: false}
		}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		back() {
			this.state.go('school-wizard-lan-initial');
		}

		gotoNext() {
			this.state.go('school-wizard-lan-users-simple');
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanConfiguration', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-configuration.html',
			controller: SchoolWizardLanConfigurationController
		});

})();
