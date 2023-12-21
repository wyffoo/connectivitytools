'use strict';

(function () {

	class MultischoolWizardBroadbandCtController {
		model = {}

		constructor($scope, $stateParams, $state, Modal, $localStorage, $translate) {
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 4;
			this.translations = {};
			$translate([
				'multi-bandwidth-ct.modal1_title',
				'multi-bandwidth-ct.modal1_body',
				'multi-bandwidth-ct.modal2_title',
				'multi-bandwidth-ct.modal2_body'
			]).then((result)=>{
				this.translations = result;
			});


			$scope.$watch('tech_cellular', (newVal, oldVal) => {
				if (newVal === false) $scope.tech_all = false;
			});

			$scope.$watch('tech_satellite', (newVal, oldVal) => {
				if (newVal === false) $scope.tech_all = false;
			});

			$scope.$watch('tech_rts', (newVal, oldVal) => {
				if (newVal === false) {
					if (this.model.is_net_in_output && $scope.tech_focl === false) $scope.tech_focl = true;
					$scope.tech_all = false;
				}
			});

			$scope.$watch('tech_focl', (newVal, oldVal) => {
				if (newVal === false) {
					if (this.model.is_net_in_output && $scope.tech_rts === false) $scope.tech_rts = true;
					$scope.tech_all = false;
				}
			});

			setTimeout(()=> {
				if (this.model.net_in_output_techs) {
					//Restore techs from model
					if (this.model.net_in_output_techs.includes('cellular')) {
						//console.log('set cellular');
						$scope.tech_cellular = true;
					}
					if (this.model.net_in_output_techs.includes('satellite')) {
						$scope.tech_satellite = true;
					}
					if (this.model.net_in_output_techs.includes('rts')) {
						$scope.tech_rts = true;
					}
					if (this.model.net_in_output_techs.includes('focl')) {
						$scope.tech_focl = true;
					}
					if (this.model.tp_ql==='HIGH') {
						$scope.tech_satellite = false;
					}
				} else {
					//Enable all by default
					$scope.tech_all = true;
					this.switchTechAll();
				}
			},0);
		}

		switchTechAll() {
			if (this.scope.tech_all === true) {
				this.scope.tech_cellular = this.scope.tech_satellite = this.scope.tech_rts = this.scope.tech_focl = true;
			} else {
				this.scope.tech_cellular = this.scope.tech_satellite = this.scope.tech_rts = this.scope.tech_focl = false;
			}
		}

		modal(num) {
			this.infoModal(this.translations['multi-broadband-initial.modal'+num+'_title'],
				this.translations['multi-broadband-initial.modal'+num+'_body']);
		}

		// nav buttons
		continue() {
			this.model.progress.status[4] = 'completed';
			//Save techs to model
			this.model.net_in_output_techs = [];
			if (this.scope.tech_cellular) this.model.net_in_output_techs.push('cellular');
			if (this.scope.tech_satellite) this.model.net_in_output_techs.push('satellite');
			if (this.scope.tech_rts) this.model.net_in_output_techs.push('rts');
			if (this.scope.tech_focl) this.model.net_in_output_techs.push('focl');
			this.state.go('multischool-wizard-lan-initial');
		}

		back() {
			if (this.model.is_bandwidth_calc_required === 'provided') {
				this.state.go('multischool-wizard-broadband-bandwidth-selector');
			} else {
				this.state.go('multischool-wizard-broadband-tp');
			}
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardBroadbandCt', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-broadband/multischool-wizard-broadband-ct.html',
			controller: MultischoolWizardBroadbandCtController
		});

})();
