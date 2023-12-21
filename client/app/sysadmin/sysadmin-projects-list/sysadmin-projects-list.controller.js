'use strict';

(function () {

	class SysadminProjectsListComponent {
		//start-non-standard
		projects = [];
		//end-non-standard

		constructor(Modal, $scope, $state, ProjectsResource, $translate) {
			this.translations = {};
			this.state = $state;
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

	angular.module('gigaApp.sysadmin')
		.component('sysadminProjectsList', {
			templateUrl: 'app/admin/sysadmin-projects-list/sysadmin-projects-list.html',
			controller: SysadminProjectsListComponent
		});

})();
