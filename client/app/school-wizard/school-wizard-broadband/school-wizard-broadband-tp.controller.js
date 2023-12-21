'use strict';

(function () {

	class SchoolWizardBroadbandTp {
		model = {
			tp_id: ""
		}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		descriptionPanelContent(content){
			this.model.tp_id = content;
			//console.log(this.model.tp_id);
		}

		previous() {
			this.state.go('school-wizard-broadband-bandwidth-selector');
		}

		saveContinue() {
			this.state.go('school-wizard-broadband-users-simple');
		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardBroadbandTp', {
			templateUrl: 'app/school-wizard/school-wizard-broadband/school-wizard-broadband-tp.html',
			controller: SchoolWizardBroadbandTp
		});

})();
