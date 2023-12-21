'use strict';

(function () {

	class SchoolWizardLanUsersSimple {
		model = {}

		constructor($scope, $stateParams, $state, $localStorage) {
			this.scope = $scope;
			this.state = $state;
			this.model = $localStorage.SchoolWizardModel;
		}

		previous() {
			this.state.go('');
		}

		submit() {
			this.state.go('');
		}

		gotoUsrExt(){
			this.state.go('school-wizard-lan-extended-users');
		}

		gotoNext(){
			this.state.go('school-wizard-lan-extended-computers');
		};
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardLanUsersSimple', {
			templateUrl: 'app/school-wizard/school-wizard-lan/school-wizard-lan-users-simple.html',
			controller: SchoolWizardLanUsersSimple
		});

})();
