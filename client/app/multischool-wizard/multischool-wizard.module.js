'use strict';

angular.module('gigaApp.multischool-wizard', [
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('project-wizard');
	$translate.refresh();
});
