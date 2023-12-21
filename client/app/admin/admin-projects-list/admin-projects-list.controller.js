'use strict';

(function () {

	class AdminProjectsListComponent {
		//start-non-standard
		projects = [];
		//end-non-standard

		constructor(Modal, $scope, $state, ProjectsResource, $translate, appConfig, Auth) {
			this.modal = Modal;
			if (typeof window.showdisclaimer === 'undefined') {
				setTimeout(() => {
					this.modal.$uibModal.open({
						animation: true,
						keyboard: false,
						backdrop: 'static',
						templateUrl: appConfig.isCountriesMode
							? 'app/main/main.disclaimer.countries.en.html'
							: 'app/main/main.disclaimer.schools.en.html',
						controller: 'mainDisclaimerController',
						size: 'md'
					});
				});
				window.showdisclaimer = false;
			}
			this.translations = {};
			this.isAdmin = Auth.isAdmin();
			this.state = $state;
			this.isCountriesMode = appConfig.isCountriesMode;
			this.isSchoolsMode = appConfig.isSchoolsMode;
			this.isGlobalMode = appConfig.isGlobalMode;
			$translate([]).then((result)=>{
				this.translations = result;
			});
			this.ProjectsResource = ProjectsResource;
			this.projects = $scope.$parent.$resolve.loadProjects.data;
			this.confirmDuplicate = Modal.confirm.simple((project) => {
				this.duplicateProject(project);
			});
			this.confirmDelete = Modal.confirm.delete((project) => {
				this.delete(project);
			});
		}

		duplicateProject(project) {
			this.ProjectsResource.duplicate({id:project.id}, ()=>{
				this.state.reload();
			});
		}

		calculate(project) {
			this.ProjectsResource.calculate({id:project.id}, ()=>{
				this.state.reload();
			});
		}

		downloadXLSX(project) {
			this.ProjectsResource.downloadXLSX({id:project.id});
		}

		delete(project) {
			this.ProjectsResource.delete({id:project.id}, ()=>{
				this.projects.splice(this.projects.indexOf(project), 1);
			});
		}
	}

	function mainDisclaimerController($scope, $uibModalInstance) {
		$scope.dismiss = function () {
			$uibModalInstance.dismiss('cancel');
		};
	}

	angular.module('gigaApp')
		.controller('mainDisclaimerController', mainDisclaimerController);

	angular.module('gigaApp.admin')
		.component('adminProjectsList', {
			templateUrl: 'app/admin/admin-projects-list/admin-projects-list.html',
			controller: AdminProjectsListComponent
		});

})();
