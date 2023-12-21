'use strict';

(function () {

	class MultischoolWizardBroadbandInitialController {
		constructor($scope, $stateParams, $state, Modal, $localStorage, $translate) {
			this.scope = $scope;
			this.state = $state;
			this.infoModal = Modal.info.simple();
			this.model = $localStorage.SchoolWizardModel;
			this.model.progress.active = 1;
			this.translations = {};
			$translate([
				'multi-broadband-initial.modal1_title',
				'multi-broadband-initial.modal1_body',
				'multi-broadband-initial.modal2_title',
				'multi-broadband-initial.modal2_body'
			]).then((result)=>{
				this.translations = result;
			});

			if (typeof this.model.net_in_output_optimization === 'undefined') {
				this.model.net_in_output_optimization = 'cost';
			}

			$scope.$watch('optimization_npv', (newVal, oldVal) => {
				if (typeof newVal === 'undefined' || typeof oldVal === 'undefined') return;
				if (newVal === true) {
					this.model.net_in_output_optimization = 'npv';
				}
				//console.log('npv',newVal,oldVal);
				$scope.optimization_cost = !newVal;
			});

			$scope.$watch('optimization_cost', (newVal, oldVal) => {
				if (typeof newVal === 'undefined' || typeof oldVal === 'undefined') return;
				if (newVal === true) {
					this.model.net_in_output_optimization = 'cost';
				}
				//console.log('npv',newVal,oldVal);
				$scope.optimization_npv = !newVal;
			});

			setTimeout(()=> {
				if (typeof this.model.net_in_output_optimization !== 'undefined') {
					if (this.model.net_in_output_optimization === 'npv') {
						$scope.optimization_npv = true;
					} else {
						$scope.optimization_cost = true;
					}
				} else {
					$scope.optimization_cost = true;
					this.model.net_in_output_optimization = 'cost';
				}
			},0);
		}

		modal(num) {
			this.infoModal(this.translations['multi-broadband-initial.modal'+num+'_title'],
				this.translations['multi-broadband-initial.modal'+num+'_body']);
		}

		calculate() {
			this.model.progress.status[1] = 'completed';
			this.model.progress.status[2] = 'pristine';
			this.model.progress.status[3] = 'pristine';
			this.model.progress.status[4] = 'pristine';
			this.model.is_broadband_required = true;
			this.model.is_net_in_output = false;
			this.model.net_in_output_optimization = null;
			this.state.go('multischool-wizard-broadband-bandwidth-selector');
		}

		calculateWithTopology() {
			this.model.progress.status[1] = 'completed';
			this.model.progress.status[2] = 'pristine';
			this.model.progress.status[3] = 'pristine';
			this.model.progress.status[4] = 'pristine';
			this.model.is_broadband_required = true;
			this.model.is_net_in_output = true;
			this.state.go('multischool-wizard-broadband-bandwidth-selector');
		}

		skip() {
			this.model.progress.status[1] = 'partial';
			this.model.progress.status[2] = 'completed';
			this.model.progress.status[3] = 'completed';
			this.model.progress.status[4] = 'completed';
			this.model.is_broadband_required = false;
			this.model.is_net_in_output = false;
			this.model.net_in_output_optimization = null;
			this.model.is_bandwidth_calc_required = null;
			this.state.go('multischool-wizard-lan-initial');
		}

		// nav buttons
		back() {
			this.state.go('admin-project-form');
		}
	}

	angular.module('gigaApp.multischool-wizard')
		.component('multischoolWizardBroadbandInitial', {
			templateUrl: 'app/multischool-wizard/multischool-wizard-broadband/multischool-wizard-broadband-initial.html',
			controller: MultischoolWizardBroadbandInitialController
		});

})();
