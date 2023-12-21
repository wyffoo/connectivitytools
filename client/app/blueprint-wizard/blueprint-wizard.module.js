'use strict';

angular.module('gigaApp.blueprint-wizard', [
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('project-wizard');
	$translate.refresh();
});
