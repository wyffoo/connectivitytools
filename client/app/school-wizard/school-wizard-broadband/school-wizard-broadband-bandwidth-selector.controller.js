'use strict';

(function () {

	class SchoolWizardBroadbandBandwidthSelectorController {
		model = {}

		constructor($scope, $stateParams, $state, Modal, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			//TODO: load filled model from the local storage
			this.infoModal = Modal.info.simple();
			this.model = $localStorage.SchoolWizardModel;
		}

		modalTest() {
			this.infoModal('Title','<strong>Description</strong>');
		}

		previous() {
			this.state.go('school-wizard-broadband-initial');
		}

		gotoReqBand() {
			this.state.go('school-wizard-broadband-required-bandwidth');
		}
		gotoTP() {
			this.state.go('school-wizard-broadband-tp');
		}
// nav buttons
		continue(){
			this.state.go('school-wizard-broadband-tp');
		}

		back(){

		}

	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandBandwidthSelector', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-bandwidth-selector.html',
			controller: SchoolWizardBroadbandBandwidthSelectorController
		});

})();
