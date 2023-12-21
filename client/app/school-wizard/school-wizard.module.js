'use strict';

angular.module('gigaApp.school-wizard', [
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('project-wizard');
	$translate.refresh();
});
