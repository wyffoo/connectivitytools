'use strict';

(function () {

	class SchoolWizardLanInitialController {
		model = {

		}

		constructor($scope, $stateParams, $state,Modal, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
			this.infoModal = Modal.info.simple();
		}

		modalTest() {
			this.infoModal('Title','<strong>Description</strong>');
		}

		open() {

		}

		calculate() {
			this.state.go('school-wizard-broadband-bandwidth-selector');
		}

		skip() {
			this.state.go('school-wizard-lan-configuration');
		}

		// nav buttons
		continue() {
			this.state.go('multischool-wizard-broadband-bandwidth-selector');
		}

		back() {
			this.state.go('multischool-wizard-broadband-initial');
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanInitial', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-initial.html',
			controller: SchoolWizardLanInitialController
		});

})();
