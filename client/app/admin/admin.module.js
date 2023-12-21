'use strict';

angular.module('gigaApp.admin', [
	'gigaApp.auth',
	'ngTagsInput',
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('admin');
	$translate.refresh();
});
