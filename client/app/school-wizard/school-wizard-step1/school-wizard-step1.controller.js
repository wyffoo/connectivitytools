'use strict';

(function () {

	class SchoolWizardStep1Controller {
		step = 'initial';
		//step = 'broadbandA';

		model = {
			countrycode: '',
			name: '',
			configuration: '',
			bandwidth_wizard: null,
			shifts: '',
			lan_configuration: {wired: false, wireless: false, public_hotspot: false}
		};

		constructor($scope, $stateParams, $state, $filter, ProjectsResource, $translate, Modal, $localStorage) {
			this.translations = {};
			$translate([]).then((result) => {
				this.translations = result;
			});
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			this.model = $localStorage.SchoolWizardModel;
		}

		modalTest() {
			this.infoModal('Title','<strong>Description</strong>');
		}

		submit() {
			this.state.go('school-wizard-broadband-initial');
			// switch (this.step) {
			// 	case "initial":
			// 		this.step = 'broadband1';
			// 		break;
			// 	case "broadband1":
			// 		this.step = 'broadband2';
			// 		break;
			// 	case "broadband2":
			// 		this.step = 'broadband3';
			// 		break;
			// 	case "broadband3":
			// 		this.step = 'broadband4';
			// 		break;
			// 	case "broadband4":
			// 		this.step = 'broadband5';
			// 		break;
			// 	case "broadband5":
			// 		this.step = 'broadband6';
			// 		break;
			// 	case "broadband6":
			// 		this.step = 'broadband7';
			// 		break;
			// 	case "broadband7":
			// 		this.step = 'broadband8';
			// 		break;
			// 	case "broadband8":
			// 		this.step = 'broadband9';
			// 		break;
			// 	case "broadband9":
			// 		this.step = 'broadbandA';
			// 		break;
			// 	case "broadbandA":
			// 		this.step = 'broadbandB';
			// 		break;
			// 	case "broadbandB":
			// 		this.step = 'broadbandC';
			// 		break;
			// 	case "broadbandC":
			// 		this.step = 'broadbandD';
			// 		break;
			// 	case "broadbandD":
			// 		this.step = 'broadbandE';
			// 		break;
			// 	case "broadbandE":
			// 		this.step = 'broadbandF';
			// 		break;
			// 	case "broadbandF":
			// 		this.step = 'broadbandG';
			// 		break;
			// 	case "broadbandG":
			// 		this.step = 'broadbandH';
			// 		break;
			// }

		}
	}

	angular.module('gigaApp.school-wizard')
		.component('schoolWizardStep1', {
			templateUrl: 'app/school-wizard/school-wizard-step1/school-wizard-step1.html',
			controller: SchoolWizardStep1Controller
		});

})();
