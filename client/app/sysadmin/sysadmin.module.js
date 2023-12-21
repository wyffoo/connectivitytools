'use strict';

angular.module('gigaApp.sysadmin', [
	'gigaApp.auth',
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('sysadmin');
});
