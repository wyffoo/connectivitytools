'use strict';

angular.module('gigaApp.admin')
	.config(function ($stateProvider) {
		$stateProvider
			.state('admin-project-setup', {
				url: '/admin/project/setup/:projectId',
				template: '<admin-project-setup></admin-project-setup>',
				authenticate: ['admin','sysadmin'],
				resolve: {
					loadProject: function ($http, $stateParams) {
						// $http returns a promise for the url data
						return $http({method: 'GET', url: '/api/projects/' + $stateParams.projectId});

					},
					loadSummary: function ($http, $stateParams) {
						return $http({method: 'GET', url: '/api/results/project/'+$stateParams.projectId+'/summary'});
					},
					loadCountries: function ($http) {
						return $http({method: 'GET', url: '/api/projects/countries'});
					},
					loadVariables: function ($http, $stateParams) {
						return $http({method: 'GET', url: '/api/variables/project/' + $stateParams.projectId});
					},
					loadTemplateInfo: function ($http, $stateParams, appConfig) {
						if (!appConfig.isSchoolsMode) return null;
						return $http({method: 'GET', url: '/api/projects/input_templates_by_project/' + $stateParams.projectId})
					},
					loadTopology: function ($http, $stateParams, appConfig) {
						//if (!isSchoolsMode) return null;
						return $http({method: 'GET', url: '/api/results/project/'+$stateParams.projectId+'/topology_graph'});
					},
					loadTopologySummary: function ($http, $stateParams, appConfig) {
						//if (!isSchoolsMode) return null;
						return $http({method: 'GET', url: '/api/results/project/'+$stateParams.projectId+'/topology_summary'});
					}
				}

			});
	});
