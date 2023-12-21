'use strict';

(function () {

	function ProjectsResource($resource, $localStorage) {
		let api = $resource('/api/projects/:id', {
			id: '@id'
		}, {
			'update': { method:'PUT' },
			'calculate': { method:'POST', url: '/api/calculation/project/:id' },
			'stop': { method:'POST', url: '/api/calculation/stop/project/:id' },
			'calculateTopology': { method:'POST', url: '/api/calculation/project/topology/:id' },
			'duplicate': { method: 'POST', url: '/api/projects/duplicate/:id' }
		});

		return api;
	}

	angular.module('gigaApp')
		.factory('ProjectsResource', ProjectsResource);

})();
