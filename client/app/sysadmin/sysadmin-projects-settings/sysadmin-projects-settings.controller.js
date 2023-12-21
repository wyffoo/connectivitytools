'use strict';

(function () {

	/**
	 * У технологий не будет уровня как в ТЗ, только название, описаие + сет параметров
	 * А.5.4 - только форма с названием, описанием и сетом переменных
	 */
	class SysadminProjectsSettingsComponent {
		//start-non-standard
		technologies = [];
		//end-non-standard

		constructor($scope, $state, Modal, $http) {
			this.technologies = $scope.$parent.$resolve.Technologies.data;
			this.state = $state;
			this.modal = Modal;
			this.http = $http;
		}

		showForm(technology) {
			//Editing if technology !== null
			//Creation if technology === null
			let modalInstance = this.modal.$uibModal.open({
				animation: true,
				keyboard: false,
				backdrop: 'static',
				templateUrl: 'app/sysadmin/components/sysadmin-technology-form/sysadmin-technology-form.html',
				controller: 'SysadminTechnologyFormController',
				size: 'lg',
				resolve: {
					technology: () => {
						return technology;
					},
					params: () => {
						// if (technology === null) {//New technology
						// 	return [];
						// }
						return this.http({method: 'GET', url: 'assets/mocks/sysadmin-technology-params.json'});
					},
					onSubmit: () => {
						return (model) => {
							return this.save(model, technology === null);
						}
					}
				}
			});
		}

		delete(technology) {
			//TODO: send to server
			this.technologies.splice(this.technologies.indexOf(technology), 1);
		}

		save(model, isNew) {
			if (isNew) {//New technology
				//TODO: save on server
				return this.http({method: 'GET', url: '/'});
			} else {
				return this.http({method: 'GET', url: '/'});
				//TODO: save on server
			}
		}

		reloadTechnologies() {
			this.state.reload();
		}

	}

	angular.module('gigaApp.sysadmin')
		.component('sysadminProjectsSettings', {
			templateUrl: 'app/sysadmin/sysadmin-projects-settings/sysadmin-projects-settings.html',
			controller: SysadminProjectsSettingsComponent
		});

})();
