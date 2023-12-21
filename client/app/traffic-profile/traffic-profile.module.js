'use strict';

angular.module('gigaApp.traffic-profile', [
	'ui.router'
]).run(function($translatePartialLoader, $translate){
	$translatePartialLoader.addPart('traffic-profile');
	$translate.refresh();
});
